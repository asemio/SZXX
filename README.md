# SZXX

_Streaming ZIP XML XLSX parser_

SZXX is a streaming, **non-seeking** and efficient XLSX parser built from the ground up for very low memory usage. It begins outputting rows while a file is still incomplete. In many cases it can be configured to run in **constant memory**. [It even works in the browser!](#does-it-work-in-the-browser)

SZXX can be used to stream data out of ZIP files, XML files, as well as XLSX files.

Modules:
- [SZXX.Xlsx](#SZXX.Xlsx)
- [SZXX.Xml](#SZXX.Xml)
- [SZXX.Zip](#SZXX.Zip)

## SZXX.Xlsx

To paraphrase an [infamous rant](https://github.com/gco/xee/blob/4fa3a6d609dd72b8493e52a68f316f7a02903276/XeePhotoshopLoader.m#L108-L136):
> At this point, I'd like to take a moment to speak to you about the XLSX format... XLSX is not my favourite file format.

XLSX appears to be designed to force the entire file to be deserialized into memory. It fights back every attempt at streaming or extracting partial data.

An XLSX file is actually a ZIP archive containing several XML files, each containing different pieces of the data.

This library abstracts away as much complexity as possible and documents the complexity that can't be avoided. As such, this README contains a number of notes labeled ⚠️ **XLSX Hazards** ⚠️. These notes explain why this library's API is the way it is. It's all due to the format: XLSX does not want to be streamed, it resists and fights back!


### Full Example: let's stream a file

1. First we need a `feed` function (`SZXX.Zip.feed`) that SZXX can call whenever it needs more data. The following snippet creates a `feed` function from a `Lwt_io.input_channel`. See [this section](#feed-function-examples) for more examples of `feed` functions.

```ocaml
let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)
```

2. We also need a `'a SZXX.Xlsx.cell_parser` value.

⚠️ **XLSX Hazard #1** ⚠️ XLSX cells are typed. The types are: `string`, `formula`, `error`, `boolean`, `number`, `date` (rarely used), and `null`. This is simply a set of 7 functions, one to parse each of the 7 cells types. SZXX will automatically invoke the right one based on the cell type. You can inspect the cell location (sheet, row, column) to determine how to map it to your own `'a` type.

```ocaml
type 'a cell_parser = {
  string: location -> string -> 'a;
  formula: location -> formula:string -> string -> 'a;
  error: location -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
  date: location -> string -> 'a;
  null: 'a;
}
```

A simple Yojson cell_parser is included in this library (`SZXX.Xlsx.yojson_cell_parser`) but creating your own cell_parser is probably a good idea.

⚠️ **XLSX Hazard #2** ⚠️ String cells use XML-escaping (`&gt;` for ">", `&#x1F600;` for "😀" etc). For performance reasons SZXX avoids preemptively unescaping String cells in case they're not used. `SZXX.Xlsx.yojson_cell_parser` already unescapes strings for you. If you write your own `cell_parser` and your String cells might contain reserved XML characters (`<`, `>`, `'`, `"`, `&`, etc) you will need to call `SZXX.Xml.unescape` on data coming from String cells.

⚠️ **XLSX Hazard #3** ⚠️ Most XLSX applications use the `number` type (OCaml float) to encode Date and DateTime. Pass this float to `SZXX.Xlsx.parse_date` or `SZXX.Xlsx.parse_datetime` to decode it. The `date` type was only introduced to Excel in 2010 and very XLSX readers/writers use it.

3. We can finally process our XLSX file.

```ocaml
open Lwt.Syntax

let print_rows_as_json xlsx_path =
  Lwt_io.with_file ~flags:[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun input_channel ->
      (* 1. Create a `feed` function *)
      let feed = feed_string input_channel in

      (* 2. Invoke one of the stream_rows_* functions. They return the following:
          - `stream` is a Lwt_stream
          - `success` is a `unit Lwt.t` *)
      let stream, success =
        SZXX.Xlsx.stream_rows_buffer ~feed SZXX.Xlsx.yojson_cell_parser
      in

      (* 3. Work with the stream, but DO NOT AWAIT this promise!
          Again, do not bind unto this promise yet! *)
      let processed =
        Lwt_stream.iter_s (fun ({ sheet_number; row_number; data } as row) ->
            let open SZXX.Xlsx in
            (* `row.data` has type `Yojson.Basic.t array` because of our choice of `cell_parser` *)
            (* For this example we just print the JSON to stdout *)
            let all_columns_as_json = `List (Array.to_list row.data) in
            Lwt_io.printl (Yojson.Basic.pretty_to_string all_columns_as_json))
          stream
      in

      (* 4. Bind/await the `success` promise to catch any error that may have terminated the stream early
          This could be due to a corrupted file or similar issues. *)
      let* () = success in

      (* 5. Finally we can bind/await the promise from step 3 and use it! *)
      let* result = processed in

      do_work result
    )
```

### Streaming is hard

⚠️ **XLSX Hazard #4** ⚠️ Unfortunately, the vast majority of applications that generate XLSX files will keep not inline the contents of String cells directly into the spreadsheet. Instead, String cells will contain a reference to a Shared String. This saves space, but 99.9% of the time the Shared String Table (SST) is located **after** the sheet! SZXX is a non-seeking parser: it can't skip ahead to parse the SST and then rewind to parse the spreadsheet!

Therefore, the user must either:
- ignore String cells
- OR buffer rows until String cells can be read
- OR work exclusively with spreadsheets that use Inline Strings and/or place the SST at the beginning of the XLSX file.

Keep reading to see how to design a solution for your use case.

The main functions of this module are:

| Function | Rows | Strings cells | Memory | Notes |
|----------|------|---------------|--------|-------|
| `stream_rows` | Parsed | Delayed (usually) | Constant | Inlined String cells are available immediately, SST lookups are delayed unless the SST is located before the sheet within the file. |
| `stream_rows_buffer` | Parsed | Available | Linear (usually) | Constant memory if the SST is located before the sheet within the file. |
| `stream_rows_unparsed` | Unparsed | Delayed | Constant | Slightly faster than `stream_rows`. This allows the user to skim through a file and only parse the rows they're interested in. |

Rows are always emitted strictly in the order they appear in the sheet.

```ocaml
type 'a status =
  | Available of 'a
  | Delayed   of delayed_string
[@@deriving sexp_of]

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a array;
}
[@@deriving sexp_of]
```

#### `stream_rows`

Returns a `'a status row Lwt_stream.t`: a stream of fully parsed XLSX rows, with one caveat: cells are wrapped in `'a status`. This status will be Available for everything except for Shared Strings that are referencing a SST that has yet to be seen.

Some workflows simply do not need to read data from String cells at all.

For those that do, the typical process is to filter out as many rows as possible and then buffer the rest until the SST becomes available.

```ocaml
let open Lwt.Syntax in
let open SZXX.Xlsx in

(* Step 1: Invoke `stream_rows` *)
let stream, sst_p, success = stream_rows ~feed yojson_cell_parser in

(* Step 2: Filter out as many rows as possible by inspecting metadata (row_number, etc) and non-string data *)
let filtered =
  Lwt_stream.filter
    (fun { sheet_number = _; row_number; data } ->
      (* In this example we look at column 3 and the row_number to determine if a row is relevant.
          Let's assume we only want to look at the first 1000 rows
          and only those that contain TRUE in column 3. *)
      match (Array.get data 3) with
      | _ when row.row_number > 1000 -> false
      | Available (`Bool x) -> x
      | Available _
      | Delayed _ ->
        failwith "Unexpected data in column D")
    stream
in

(* Step 3: Process the stream.
    As usual, DO NOT BIND/AWAIT the promise yet! *)
let count =
  Lwt_stream.fold_s
    (fun row acc ->
      (* 99% of the time, the location of the SST is at the end of the XLSX file, meaning `sst_p` will
          only resolve AFTER SZXX has finished parsing your sheet (adding its rows to the stream).
          Therefore your program will consume as much RAM as necessary to store all the rows you
          didn't filter out at Step 1 *)

      (* await the SST. This causes buffering. *)
      let* sst = sst_p in

      (* unwrap the status (Available/Delayed) by calling `unwrap_status` to resolve all Delayed cells *)
      let fully_available_row = unwrap_status yojson_cell_parser sst row in

      let+ result = do_work fully_available_row in
      acc + 1)
    filtered 0
in

(* Step 3: Await `success` to catch corruption errors *)
let* () = success in

(* Step 4: Use the result of Step 3! *)
let* count = count in
Lwt_io.printlf "Number of rows: %d" count
```

#### `stream_rows_buffer`

Same as `stream_rows`, but returns `'a row Lwt_stream.t` instead of `'a status row Lwt_stream.t`.

In other words it eliminates the complexity of having to call `unwrap_status` to read String cells.

**Caution**: This function is only safe to use if **at least one** of these conditions is true:
- You are certain that your spreadsheet does not contain Shared Strings. In other words, all strings are inlined into the spreadsheet itself instead of being stored in the SST.
- OR: You are certain that the SST is located _before_ your sheet within the ZIP archive. This appears to be rare in practice.
- OR: Your sheet is small. You don't mind if every row is held in memory -until the SST is reached- before any row is available on the `Lwt_stream.t`.

#### `stream_rows_unparsed`

Same as `stream_rows`, but returns each row as a raw XML element: `Xml.DOM.element row Lwt_stream.t`.

Call `Xlsx.parse_row` to get the more user friendly `'a status row`, or directly manipulate the XML element through the functions provided in the `Xml.DOM` module.

This function is especially interesting for users that might be interested in just a few rows out of a large spreadsheet because it avoids wasting time parsing every row.

#### `parse_row`

Converts an `Xml.DOM.element Xlsx.row` (as returned by `stream_rows_unparsed`) into an `'a Xlsx.status Xlsx.row` (as returned by `stream_rows`).

#### `unwrap_status`

Converts a parsed row `'a Xlsx.status Xlsx.row` (as returned by `stream_rows` and `parse_row`) into `'a Xlsx.row` (as returned by `stream_rows_buffered`).

#### `resolve_sst_index`

Manually resolve a single SST reference.

#### `parse_date`

XLSX uses its `number` type (OCaml float) to encode Date cells. This function converts one such float into a Core_kernel `Date.t`.

#### `parse_datetime`

XLSX uses its `number` type (OCaml float) to encode DateTime cells. This function converts one such float into a Core_kernel `Time.t`.

#### `index_of_column`

Converts from a column reference (such as "D7" or "AA2") into a 0-based column index.


## SZXX.Xml

#### `parser`

This XML parser is far from fully spec-compliant and does not attempt to validate, correct errors, or follow references. Text nodes are seen as opaque bytes.

It returns one "XML event" (XML node) at a time. Invoke it using `Angstrom.many` to form a sequence/list/etc that can be passed to `Xml.SAX.To_DOM.folder` or `Xml.SAX.Stream.folder` to form a valid XML tree.

#### `To_DOM` module

Fold your sequence/list/stream of `SAX.node` values into a fully formed XML tree.

When done, the final output of `folder` will contain the DOM tree in the `top` field and the XML document attributes in the `decl_attrs` field.

**Example 1: synchronous input string**
```ocaml
let open SZXX in
let raw_xml_string = "...xml..." in

let nodes =
  Angstrom.parse_string ~consume:All (Angstrom.many Xml.parser) raw_xml_string
  |> Result.ok_or_failwith
in
let xml =
  List.fold_result nodes ~init:Xml.SAX.To_DOM.init ~f:(fun acc x -> Xml.SAX.To_DOM.folder (Ok acc) x)
  |> Result.ok_or_failwith
in
(* Do something with `xml` *)
```

**Example 2: streamed raw input data, progressively parse whole XML tree in memory**

Here the XML tree is constructed on the fly as the raw bytes come in, instead of having to read everything into a string before we even begin constructing the XML tree.
```ocaml
let state = ref (Ok Xml.SAX.To_DOM.init) in
let on_parse node =
  state := Xml.SAX.To_DOM.folder !state node;
  Lwt.return_unit
in
let* _rest, result =
  Lwt.finalize
    (fun () -> Angstrom_lwt_unix.parse_many Xml.parser on_parse input_channel)
    (fun () -> Lwt_io.close input_channel)
in

match result, !state with
| Error msg, _
| _, Error msg ->
  failwith msg
| Ok (), Ok parsed_xml ->
  (* Do something with parsed_xml *)
  Lwt.return_unit
```

#### `Stream` module

Fold your sequence/list/stream of `SAX.node` values into a **shallow** DOM tree.

The `folder` function takes 2 more arguments than `To_DOM.folder`:
- `filter_path`: where to truncate the tree.
- `on_match`: the callback invoked whenever an XML node is a direct child of `filter_path`.

Example: given a standard HTML document, if `~filter_path:["html"; "head"]` then `on_match` will be called for every `<script>`, `<style>`, etc. Furthermore, `html -> head` will have no `children` because they were passed to `on_match` _instead_ of being added to the tree.

Hence **shallow** DOM tree.

This is how `SZXX.Xlsx` manages to run in constant memory.

**Example 3: streamed raw input data, streamed parsed XML elements, constant memory**

This is similar to Example 2, except that we define a `filter_path` and all nodes at that path will be passed to `on_match` instead of being added to the tree. This allows us to process matches on the fly without storing all of them in the tree in memory.

In this example our `filter_path` will select all `<div>` elements at the root of the `<body>` element.

The `on_match` function will receive them and work on them by side effect.
```ocaml
let state = ref (Ok Xml.SAX.Stream.init) in
let filter_path = [ "html"; "body"; "div" ] in
let on_match div =
  (* Do something with `div` *)
  print_endline (Xml.DOM.sexp_of_element div |> Sexp.to_string)
in
let on_parse node =
  state := Xml.SAX.Stream.folder ~filter_path ~on_match !state node;
  (* Here we can asynchronously process matches we saved from `on_match` by side effect *)
  Lwt.return_unit
in
let* _rest, result =
  Lwt.finalize
    (fun () -> Angstrom_lwt_unix.parse_many Xml.parser on_parse input_channel)
    (fun () -> Lwt_io.close input_channel)
in

match result, !state with
| Error msg, _
  |_, Error msg ->
  failwith msg
| Ok (), Ok shallow_tree ->
  (* Do something with `shallow_tree` *)
  let* () =
    Lwt_io.printl
      (Option.value_map shallow_tree.decl_attrs ~default:"--" ~f:(fun x ->
            Xml.sexp_of_attr_list x |> Sexp.to_string))
  in
  Lwt.return_unit
```

## SZXX.Zip

This ZIP parser always works in constant memory. It calls a [`feed` function](#feed-function-examples) to request more input.

It is fully featured and supports every type of ZIP, including the important ZIP64 (sub)format, with the exception of files using deprecated compression methods.

Compression methods `0` (stored/none) and `8` (deflated) are supported.

#### stream_files

Takes a [`feed` function](#feed-function-examples) and a `callback`.

SZXX will call `feed` any time it needs more bytes. Return `None` to indicate End Of File.

SZXX will call `callback` when it encounters a new file within the ZIP archive. You must choose an Action for SZXX to perform on each file.

- Return `Action.Skip` to skip over the compressed bytes of this file without attempting to uncompress them.
- Return `Action.String` to collect the whole uncompressed file into a single string. **This will obviously consume memory proportional to the size of the file.**
- Return `Action.Fold_string` to fold this file into a final state, in string chunks of roughly ~1k-5k.
- Return `Action.Fold_bigstring` to fold this file into a final state, in bigstring chunks of roughly ~1k-5k.
- Return `Action.Parse` to apply an `Angstrom.t` parser to the file while it is being uncompressed without having to fully uncompress it first. This will obviously consume memory proportial to the size of your parse state.

```ocaml
let exact_file_from_zip ~extract_filename input_channel =
  let open Lwt.Syntax in
  let open SZXX in
  (* 1. Create a `feed` function. This README contains examples of this towards the end. *)
  let feed = feed_string input_channel in

  (* 2. Create a callback function. Here we skip all files except the one we're interested in.
      If you deem the file(s) too large for `Action.String`,
      then look into `Action.Fold_string`, `Action.Fold_bigstring` and `Action.Angstrom` *)
  let callback = function
    | ({ filename; _ } : Zip.entry) when String.(filename = extract_filename) -> Zip.Action.String
    | _ -> Zip.Action.Skip
  in

  (* 3. Invoke `Zip.stream_files` *)
  let stream, success = Zip.stream_files ~feed callback in

  (* 4. Work with the stream, but DO NOT AWAIT this promise!
      Again, do not bind unto this promise yet! *)
  let unzipped =
    (* Here we're just going to flush out the contents of the stream by collecting into a list *)
    let+ files = Lwt_stream.to_list stream in
    (* Due to our `callback` function, all values in `files` will be `Zip.Data.Skipped`,
        except for possibly one `Zip.Data.String` if the desired file (`extract_filename`)
        was found in the ZIP archive *)
    List.find_map files ~f:(function
      | _entry, Zip.Data.String raw -> Some raw
      | _ -> None)
  in

  (* 4. Bind/await the `success` promise to catch any error that may have terminated the stream early
      This could be due to a corrupted file or similar issues. *)
  let* () = success in

  (* 5. Finally we can bind/await the promise from step 3 and use it! *)
  let* unzipped = unzipped in
  match unzipped with
  | None -> failwithf "File `%s` not found in ZIP archive" extract_filename ()
  | Some _raw ->
    (* Use `raw` file contents *)
    Lwt.return_unit
```

### Feed function examples

_Example 1:_ Make a naive `feed` function from an `Lwt_io.input_channel`.
```ocaml
let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)
```

_Example 2:_ Make an **efficient** `feed` function from an `Lwt_io.input_channel`.
```ocaml
let feed_bigstring ic =
  let open SZXX.Zip in
  let open Lwt.Infix in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })
```

_Example 3:_ Make a `feed` function from a `string Lwt_stream.t`.
```ocaml
let feed_stream stream =
  SZXX.Zip.String (fun () -> Lwt_stream.get stream)
```

### FAQ

#### Why `Lwt_stream`?

It's a convenient way to expose this sort of functionality. Unfortunately it forces the library to also return a promise to pass back errors that killed the stream half way through. A future version might improve on this.

#### Does it work in the browser?

Yes, with js_of_ocaml! Performance only drops by about 60% compared to a binary.

You'll need to pin a fork of Angstrom. In your OPAM file:
```
pin-depends: [
  [ "angstrom.0.15.0.lazy" "git+https://github.com/SGrondin/angstrom.git#jsoo2" ]
]

depends: [
  # ...other dependencies...

  "angstrom" { = "0.15.0.lazy" }
]
```

#### Is it fast?

Not really.

Streaming data is always going to be slower than deserializing a whole file into memory.

Using 1 core on an older 2017 Macbook Pro, it processes an enormous 107MB, 28-column x 1,048,576-row XLSX file in 160 seconds **using only 27MB of memory**. The same file takes 70 seconds to open in LibreOffice using 2 cores and 1.8GB of memory.
