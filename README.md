# SZXX

_Streaming ZIP XML XLSX parser_

SZXX is a streaming, **non-seeking** and efficient XLSX parser built from the ground up for very low memory usage. It begins outputting rows while a file is still incomplete. In cases it can be configured to run in **constant memory**. [It even works in the browser!](#does-it-work-in-the-browser)

To paraphrase an [infamous rant](https://github.com/gco/xee/blob/4fa3a6d609dd72b8493e52a68f316f7a02903276/XeePhotoshopLoader.m#L108-L136):
> At this point, I'd like to take a moment to speak to you about the OOXML (XLSX) format... XLSX is not my favourite file format.

XLSX seems to have been actively designed to consume obscene amounts of memory and computing power. It fights back every attempt at efficiently streaming data without deserializing the entire document into memory.

This library attempts to abstract away as much of that complexity as possible and document what cannot be abstracted. As such, this README contains a number of notes labeled ⚠️ **XLSX Hazards** ⚠️. These notes explain why this library's API can sometimes appear user hostile. It's all due to the format: XLSX does not want to be streamed, it resists and fights back!

An XLSX file is actually a ZIP archive containing a number of XML files and SZXX is therefore made up of three modules: Zip, Xml, and Xlsx. They can all be used independently, but none of them implement the entire spec due to the non-seeking requirement, only enough to stream XLSX rows. Despite this, SZXX is probably a solid choice for your ZIP extraction or XML parsing needs.

### Example
First we need a "feed" function. See [this section](#feed-function-examples) for more examples.
```ocaml
let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)
```
And then we can process our XLSX file:
```ocaml
open Lwt.Syntax

let print_rows_as_json xlsx_path =
  Lwt_io.with_file ~flags:[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
      (* yojson_cell_parser is an easy way to quickly inspect a file by mapping XLSX's data types to JSON *)
      let stream, success =
        SZXX.Xlsx.stream_rows_buffer ~feed:(feed_string ic) SZXX.Xlsx.yojson_cell_parser
      in
      (* We create promise called `processed` but we don't await it until later *)
      let processed =
        Lwt_stream.iter_s (fun row ->
            let open SZXX.Xlsx in
            (* `row.data` has type `Yojson.Basic.t array` because of our choice of `cell_parser` *)
            let all_columns_as_json = `List (Array.to_list row.data) in
            (* Print to stdout *)
            Lwt_io.printl (Yojson.Basic.pretty_to_string all_columns_as_json))
          stream
      in
      (* Bind to/await the `success` promise to catch any error that may have terminated the stream early *)
      let* () = success in
      (* Make sure to bind to/await `processed` AFTER `success` to avoid deadlocks on broken files *)
      processed)
```


### SZXX.Xlsx

The `SZXX.Xlsx` module is the entrypoint for most users.

⚠️ **XLSX Hazard #1** ⚠️ Unfortunately, the vast majority of applications that generate XLSX files will keep not inline the contents of String cells directly into the spreadsheet. Instead, String cells will contain a reference to a Shared String. 99.9% of the time, the Shared String Table (SST) is located **after** the sheet. This is the single most important technical detail about `SZXX.Xlsx`. It forces the user to either 1. ignore String cells, 2. buffer rows until String cells can be read, or 3. work exclusively with spreadsheets that use Inline Strings and/or place the SST at the beginning of the file. Keep reading to see how to design a solution for your use case.

The main functions of this module are:

| Function | Rows | Strings cells | Memory | Notes |
|----------|------|---------------|--------|-------|
| `stream_rows` | Parsed | Delayed (usually) | Constant | Inlined String cells are available immediately, SST lookups are delayed unless the SST is located before the sheet within the file. |
| `stream_rows_buffer` | Parsed | Available | Linear (usually) | Constant memory if the SST is located before the sheet within the file. |
| `stream_rows_unparsed` | Unparsed | Delayed | Constant | Slightly faster than `stream_rows`. This allows the user to skim through a file and only parse the rows they're interested in. |

Rows are always emitted strictly in the order they appear in the sheet.

#### `stream_rows`

Returns an `Lwt_stream.t` of fully parsed XLSX rows, with one caveat: every cell value is wrapped in `'a status`. The `status` can be either `Available of 'a` or `Delayed of delayed_string`.

Some workflows simply do not need to read data from String cells at all.

For those that do, the typical process is to filter out as many rows as possible by inspecting the `Available` cells, and then resolve the rest.

```ocaml
let open Lwt.Syntax in
let open SZXX.Xlsx in
let stream, sst_p, success = stream_rows ~only_sheet:1 ~feed yojson_cell_parser in
(* Step 1: Filter out as many rows as possible by inspecting metadata (row_number, etc) and non-string data *)
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
(* Step 2: await the SST, unwrap the status (Available/Delayed) by calling unwrap_status to resolve all Delayed cells *)
let count =
  Lwt_stream.fold_s
    (fun row acc ->
      (* 99% of the time, the location of the SST is at the end of the XLSX file, meaning [sst_p] will
       only resolve AFTER SZXX has finished parsing your sheet (adding its rows to the stream).
       Therefore your program will consume as much RAM as necessary to store all the rows you
       didn't filter out at Step 1. *)
      let+ sst = sst_p in
      let _fully_available_row = unwrap_status yojson_cell_parser sst row in
      (* do something with [fully_available_row] *)
      acc + 1)
    filtered 0
in
(* Step 3: Await [success] before awaiting the promise returned by Step 2 *)
let* () = success in
let* count = count in
Lwt_io.printlf "Number of rows: %d" count
```

`SZXX.Xlsx.stream_rows` takes a `cell_parser` argument. A simple Yojson cell_parser is included in this library (`SZXX.Xlsx.yojson_cell_parser`) but creating your own is probably a good idea for all but the simplest use cases.

⚠️ **XLSX Hazard #2** ⚠️ XLSX cells are typed. Those types are: `string`, `error`, `boolean`, `number`, and `null`. Several functions in this module take an argument of type `'a cell_parser`. This is simply a set of 5 functions, one to parse each of the 5 cells types. SZXX will automatically invoke the right one based on the cell type. You can inspect the cell location (sheet, row, column) to determine how to map it to your own types.

⚠️ **XLSX Hazard #3** ⚠️ String cells use XML-escaping (`&gt;` for `>`, etc). For performance reasons SZXX avoids preemptively unescaping String cells in case they're not used. `SZXX.Xlsx.yojson_cell_parser` already unescapes strings for you. If you write your own `cell_parser` and your String cells might contain reserved XML characters (`<`, `>`, `'`, `"`, `&`, etc) you will need to call `SZXX.Xml.unescape` on data coming from String cells.

⚠️ **XLSX Hazard #4** ⚠️ XLSX uses its `number` type (OCaml float) to encode Date and DateTime. Pass this float to `SZXX.Xlsx.parse_date` or `SZXX.Xlsx.parse_datetime` to decode it.

#### `stream_rows_buffer`

Same as `stream_rows`, but rows are not wrapped in `'a status`. It returns `'a Xlsx.row Lwt_stream.t` where `'a` is the datatype returned by your `cell_parser`.

In other words it eliminates the complexity of having to call `unwrap_status` to read String cells.

**Caution**: This function is only safe to use if **at least one** of these conditions is true:
- You are certain that your spreadsheet does not contain Shared Strings. In other words, all strings are inlined into the spreadsheet itself instead of being stored in the SST.
- OR: You are certain that the SST is located _before_ your sheet within the ZIP archive. This appears to be rare in practice.
- OR: Your sheet is small. You don't mind if every row is held in memory -until the SST is reached- before any row is available on the `Lwt_stream.t`.

#### `stream_rows_unparsed`

Same as `stream_rows`, but returns raw XML elements for row data: `Xml.DOM.element Xlsx.row Lwt_stream.t`.

Call `Xlsx.parse_row` to get the more user friendly `'a Xlsx.status Xlsx.row`, or directly manipulate the XML element through the functions provided in the `Xml.DOM` module.

A common workflow is to call `Lwt_stream.filter` on the stream returned by `stream_rows` to discard uninteresting rows. See the example for `stream_rows` above.

#### `parse_row`

Converts an `Xml.DOM.element Xlsx.row` (as returned by `stream_rows_unparsed`) into an `'a Xlsx.status Xlsx.row` (as returned by `stream_rows`).

#### `unwrap_status`

Converts a parsed row `'a Xlsx.status Xlsx.row` -as returned by `stream_rows` and `parse_row`- into `'a Xlsx.row` -as returned by `stream_rows_buffered`-.

#### `resolve_sst_index`

Manually resolve a single SST reference.

#### `parse_date`

XLSX uses its `number` type (OCaml float) to encode Date cells. This function converts one such float into `Date.t`.

#### `parse_datetime`

XLSX uses its `number` type (OCaml float) to encode DateTime cells. This function converts one such float into `Time.t`.

#### `index_of_column`

Converts from a column reference (such as "D7" or "AA2") into a 0-based column index.


### SZXX.Xml

Most users will not need to directly interact with this module other than to call `Xml.unescape`.

#### `parser`

This XML parser is far from fully spec-compliant and does not attempt to validate, correct errors, or follow references. Text nodes are seen as opaque bytes.

It returns one XML "event" (entity/node) at a time. Invoke it repeatedly to form a sequence/list/etc.

If your XML file can sit comfortably in memory:
```ocaml
let nodes_result = Angstrom.parse_string Angstrom.(many SZXX.Xml.parser) in
(* Then fold using SZXX.Xml.SAX.To_DOM or SZXX.Xml.SAX.Stream *)
```

If it doesn't fit into memory, then look into `Angstrom_lwt_unix.parse_many` to generate a sequence of `SAX.node` values. Then fold such sequence using `SAX.To_DOM` or `SAX.Stream`.

#### `To_DOM` module

Fold your sequence/list/stream of `SAX.node` values into a fully formed DOM tree.

When done, the final result of `folder` will contain the DOM tree in the `top` field and the XML document attributes in the `decl_attrs` field.

#### `Stream` module

Fold your sequence/list/stream of `SAX.node` values into a **shallow** DOM tree.

The `folder` function takes 2 more arguments than `To_DOM.folder`:
- `filter_path`: where to truncate the tree.
- `on_match`: the callback invoked whenever an XML node is a direct child of `filter_path`.

Example: given a standard HTML document, if `~filter_path:["html"; "head"]` then `on_match` will be called for every `<script>`, `<style>`, etc. Furthermore, `html -> head` will have no `children` because they were passed to `on_match` _instead_ of being added to the tree.

Hence **shallow** DOM tree.

This is used by `SZXX.Xlsx` to run in constant memory.

### SZXX.Zip

This module is easy to use on its own.

It supports compression methods `0` (stored/none) and `8` (deflated). It supports ZIP 2.0 and 4.5 (ZIP64).

#### stream_files

Takes a "feed" function and a "callback".

SZXX will call "feed" any time it needs more bytes. Return `None` to indicate End Of File.

SZXX will call "callback" when it encounters a new file within the ZIP archive. You must choose an Action for SZXX to perform over each file.

- Return `Action.Skip` to skip over the compressed bytes of this file without attempting to uncompress them.
- Return `Action.String` to collect the whole uncompressed file into a single string.
- Return `Action.Fold_string` to fold this file into a final state, in string chunks of roughly ~1k-5k.
- Return `Action.Fold_bigstring` to fold this file into a final state, in bigstring chunks of roughly ~1k-5k.
- Return `Action.Parse` to apply an `Angstrom.t` parser to the file while it is being uncompressed without having to fully uncompress it first.

This function returns `stream * success_promise`.
`stream` contains all files in the same order they were found in the archive.
`success_promise` is a promise that resolves once the entire zip archive has been processed.

Important: bind to/await `success_promise` in order to capture any errors encountered while processing the file.

### Feed function examples

Example 1: Make a trivial "feed" function from an `Lwt_io.input_channel`.
```ocaml
let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)
```

Example 2: Make an **efficient** "feed" function from an `Lwt_io.input_channel`.
```ocaml
let feed_bigstring ic =
  let open SZXX.Zip in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })
```

Example 3: Make a "feed" function from a `string Lwt_stream.t`.
```ocaml
let feed_stream stream =
  SZXX.Zip.String (fun () -> Lwt_stream.get stream)
```

### FAQ

#### Why `Lwt_stream`?

It's a convenient way to expose this sort of functionality. Unfortunately it forces the library to also return a promise to pass back errors that killed the stream half way through. A future version might improve on this.

#### Does it work in the browser?

Yes, with js_of_ocaml! Performance only drops by about 60% compared to a binary.

...but you'll need to pin a fork of Angstrom. In your OPAM file:
```
pin-depends: [
  [ "angstrom.0.15.0.lazy" "git+https://github.com/SGrondin/angstrom.git#jsoo2" ]
]
```

#### Is it fast?

Not really.

Streaming data is always going to be slower than deserializing a whole file into memory.

Using 1 core on an older 2017 Macbook Pro, it processes an enormous 107MB, 28-column x 1,048,576-row file in 160 seconds **using only 35MB of memory**. The same file takes 70 seconds to open in LibreOffice using 2 cores and 1.8GB of memory.
