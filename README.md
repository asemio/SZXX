# SZXX

_Streaming ZIP XML XLSX parser_

SZXX is a streaming, **non-seeking** and efficient XLSX parser built from the ground up for low memory usage. SZXX will begin outputting XLSX rows while a file is still incomplete. It supports even the largest spreadsheets out there. It also works in the browser.

An XLSX file is actually a ZIP archive containing a number of XML files. As such, SZXX is made up of three layers: ZIP, XML, and XLSX. They can all be used independently, but none of them implement the entire spec due to the non-seeking requirement, only enough to stream XLSX rows. Despite this, SZXX might be sufficient for your needs.

- ZIP layer: The ZIP archive is parsed while the file is still being read.
- XML layer: The decompressed file slices from the first layer are fed into this XML SAX parser.
- XLSX layer: SAX events are converted into a shallow DOM. The rows are parsed from their DOM structure into a more natural XLSX row and placed on an `Lwt_stream.t`.

The XLSX format makes **streaming** and **non-seeking** extremely difficult. This library attempts to abstract away all of that complexity and document what cannot be abstracted.

### Example
```ocaml
(* See the SZXX.Zip docs for examples of how to implement this helper function efficiently for your use case *)
let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None
      | chunk -> Some chunk)

let print_rows_as_json xlsx_path =
  Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
      let open SZXX.Xlsx in
      (* yojson_cell_parser is an easy way to quickly inspect a file by mapping XLSX's data types to JSON *)
      let stream, success = stream_rows_buffer ~feed:(feed_string ic) yojson_cell_parser in
      let processed =
        Lwt_stream.iter
          (fun row -> `List (Array.to_list row.data) |> Yojson.Basic.pretty_to_string |> print_endline)
          stream
      in
      (* Bind to/await the `success` promise to catch any error that may have terminated the stream early *)
      let* () = success in
      (* Make sure to bind to/await `success` before `processed` to avoid possible deadlocks *)
      let* () = processed in
      (* ... *)
      Lwt.return_unit)
```

### SZXX.Xlsx

The `SZXX.Xlsx` module is the entrypoint for most users.

**Important XLSX note #1**: Unfortunately, the vast majority of applications that generate XLSX files will not inline the contents of String cells. Instead, those cells contain a reference to a Shared String. 99.9% of the time, the Shared String Table (SST) is located _after_ the sheet. This is the single most important technical detail about `SZXX.Xlsx`.

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
let stream, sst_p, success = stream_file ~only_sheet:1 ~feed yojson_cell_parser in
(* Step 1: Filter out as many rows as possible by inspecting metadata (row_number, etc) and non-string data *)
let filtered =
  Lwt_stream.filter
    (fun row ->
      (* In this example we look at column 3 and the row_number to determine if a row is relevant *)
      match row.data.(3), row.row_number with
      | _, i when i > 1000 -> false
      | Available (`Bool x), _ -> x
      | Available _, _
      | Delayed _, _ ->
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

`stream_rows` also takes a `cell_parser`. A simple Yojson cell_parser is included in this library: `SZXX.Xlsx.yojson_cell_parser`, but creating your own is probably a good idea for all but the simplest use cases.

**Important XLSX note #2**: XLSX cells are typed. Those types are: `string`, `error`, `boolean`, `number`, and `null`. Several functions in this module take an argument of type `'a cell_parser`. This is simply a set of 5 functions, one to parse each of the 5 cells types. SZXX will automatically invoke the right one based on the cell type. You can inspect the cell location (sheet, row, column) to determine how to map it to your own types.

**Important XLSX note #3**: String cells use XML-escaping (`&gt;` for `>`, etc). For performance reasons SZXX avoids preemptively unescaping String cells. `SZXX.Xlsx.yojson_cell_parser` already unescapes strings for you. If you write your own cell_parser and your String cells contain reserved XML characters (`<`, `>`, `'`, `"`, `&`, etc) you will need to call `Xml.unescape` on data coming from String cells.

**Important XLSX note #4**: XLSX uses its `number` type (OCaml float) to encode Date and DateTime. Pass this float to `Xlsx.parse_date` or `Xlsx.parse_datetime` to decode it.

#### `stream_rows_buffer`

Same as `stream_rows`, but rows are not wrapped in `'a status`. It returns `'a Xlsx.row Lwt_stream.t` where `'a` is the datatype returned by your `cell_parser`.

In other words it eliminates the complexity of having to call `unwrap_status` to read String cells.

**Caution**: This function is only safe to use in 3 cases:
- You are certain that your spreadsheet does not contain Shared Strings. In other words, all strings are inlined into the spreadsheet itself instead of being stored in the SST.
- OR: You are certain that the SST is located _before_ your sheet within the ZIP archive. In practice, this appears to be rare.
- OR: Your sheet is small. You don't mind if every row is held in memory -until the SST is reached- before any row is available on the `Lwt_stream.t`.

#### `stream_rows_unparsed`

Same as `stream_rows`, but returns raw XML elements for row data: `Xml.DOM.element Xlsx.row Lwt_stream.t`.

Call `Xlsx.parse_row` to get the more user friendly `'a Xlsx.status Xlsx.row`, or directly manipulate the XML element through the functions provided in the `Xml.DOM` module.

A common workflow is to call `Lwt_stream.filter` on the stream returned by `stream_rows` to discard uninteresting rows. See the example for `stream_rows` above.

#### `parse_row`

Converts an `Xml.DOM.element Xlsx.row` (as returned by `stream_rows_unparsed`) into an `'a Xlsx.status Xlsx.row` (as returned by `stream_rows`).

#### `unwrap_status`

Converts a parsed row `'a Xlsx.status Xlsx.row` -as returned by `stream_rows` and `parse_row`- into `'a Xlsx.row` -as returned by `stream_rows_buffered`-. All SST references are resolved.

#### `resolve_sst_index`

Manually resolve a single SST reference.

#### `parse_date`

XLSX uses the `number` type


The XML parser is far from spec-compliant and does not attempt to validate, correct errors, or follow references. Text nodes are seen as opaque bytes.

The XLSX layer always streams rows in the order they are encountered. It does not automatically load string references from the Shared Strings Table (SST) because it is usually located after the worksheets within the ZIP.

### FAQ

#### Why `Lwt_stream`?

It's a convenient way to expose this sort of functionality. Unfortunately it results in having to return a promise to pass back errors that killed the stream half way through. A future version might improve on this.

#### Does this work in the browser?

Yes, with js_of_ocaml! Performance only drops by about 60% compared to a binary.

...but you'll need to pin a fork of Angstrom. In your OPAM file:
```
pin-depends: [
  [ "angstrom.0.15.0.lazy" "git+https://github.com/SGrondin/angstrom.git#jsoo2" ]
]
```

#### Is it fast?

Not really. This file format seems almost designed to make streaming as difficult and inefficient as possible.

Using 1 core on an older 2017 Macbook Pro, it processes an enormous 107MB, 28-column x 1,048,576-row file in 160 seconds using 35MB of memory. The same file takes 70 seconds to open in LibreOffice using 2 cores and 1.8GB of memory.
