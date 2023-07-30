# SZXX

_**S**treaming **Z**IP **X**ML **X**LSX_

SZXX is an efficient streaming parser built from the ground up for very low and constant memory usage.
It can be used to stream data out of ZIP, XML, and XLSX. It support both files and streams (like HTTP).

There are 3 independent modules. Skip to the one appropriate for your use case.
- [SZXX.Xlsx](#szxxxlsx)
- [SZXX.Xml](#szxxxml)
- [SZXX.Zip](#szxxzip)

```sh
opam install SZXX
```
[FAQ](#faq)

**SZXX v4** replaces Lwt with Eio. See the [release page](https://github.com/asemio/SZXX/releases/tag/4.0.0) to upgrade from v3.

## SZXX.Xlsx

This example reads from a file and prints a JSON representation of each row:
```ocaml
open! Core
open Eio.Std
open SZXX

let print_xlsx xlsx_path =
  (* The Switch receives any parsing errors *)
  Switch.run @@ fun sw ->
  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / xlsx_path)) in

  let seq = Xlsx.stream_rows_double_pass ~sw file Xlsx.yojson_cell_parser in

  (* Print each row *)
  Sequence.iter seq ~f:(fun row ->
      `List row.data |> Yojson.Basic.to_string |> print_endline )
```

### Intro to XLSX

XLSX is tabular (like CSV) and typed (like JSON).

Most XLSX files consist of about a dozen separate XML files and folders zipped together.

> Why do I need to know this?

That's a fair question. SZXX tries extremely hard to quietly handle all the complexity of this chimera of a format.

But the design of the XLSX format demands that you -the user- make a few decisions.

#### Just the essentials

The types of cells are: `String`, `Formula`, `Error`, `Boolean`, `Number`, `Date` (rarely used), and `Null`.

Many `SZXX.Xlsx` functions require a `cell_parser` argument to convert from XLSX types to your own `'a` type.

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
This library includes two simple cell_parsers to get started: `Xlsx.yojson_cell_parser` and `Xlsx.string_cell_parser`, but creating your own `'a cell_parser` is probably a good idea.

XLSX cells use XML-escaping (`&amp;` for "&", `&#x1F600;` for "😀" etc). `Xlsx.yojson_cell_parser` and `Xlsx.string_cell_parser` already unescape everything for you. If you write your own `cell_parser` and your cells might contain special characters or non-ASCII characters, you will need to call `SZXX.Xml.unescape` on data coming from **String**, **Formula**, and **Error** cells.

Most XLSX applications use Number cells (OCaml float) to encode Date and DateTime. Pass this float to `Xlsx.parse_date` or `Xlsx.parse_datetime` to decode it. The Date cell type was only introduced to Excel in 2010 and few XLSX files use it.

The vast majority of applications that generate XLSX files will not inline the contents of String cells directly into the spreadsheet. Instead, String cells will contain a reference to an offset in the **Shared Strings Table** (the **SST**). This saves space by reusing strings, but 99% of the time those applications will place the SST **after** the sheet!

If the XLSX document is in the form of a file, then it's fine because SZXX can jump around the file to parse the SST before reading the rows. But the location of the SST matters when we cannot rewind, such as when parsing directly from an HTTP stream.

We can inspect the structure of a file using `zipinfo`:
```sh
zipinfo financial.xlsx
```
The SST is indicated by `xl/sharedStrings.xml` and sheets look like `xl/worksheets/sheet1.xml`.

### Xlsx.stream_rows_double_pass

This function extracts rows from an XLSX file. It is **guaranteed to run in constant memory**, without buffering.

It first skims the file to find and extract the SST, then it streams out fully parsed rows from the various sheets. Rows are always emitted strictly in the order they appear in the sheet(s).

```ocaml
open! Core
open Eio.Std
open SZXX

let print_xlsx xlsx_path =
  (* The Switch receives any parsing errors *)
  Switch.run @@ fun sw ->
  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / xlsx_path)) in

  let seq = Xlsx.stream_rows_double_pass ~sw file Xlsx.yojson_cell_parser in

  (* Print each row *)
  Sequence.iter seq ~f:(fun row ->
      `List row.data |> Yojson.Basic.to_string |> print_endline )
```

There's just one limitation to `stream_rows_double_pass`: your XLSX document has to **a file** so that SZXX can "rewind" after extracting the SST. To stream XLSX data out of non-seekable streams such as HTTP transfers, use [Xlsx.stream_rows_single_pass](#xlsxstream_rows_single_pass) instead.

```ocaml
Xlsx.stream_rows_double_pass ?filter_sheets ~sw file cell_parser
```

#### Arguments:
- `filter_sheets`
  - Default: all sheets.
  - Pass a function to limit parsing/extraction to only certain sheets.
  - Sheet IDs start at 1. Note: it does not necessarily match the visual order of the sheets in Excel.
- `sw`
  - A regular `Eio.Switch.t`. This is where parsing errors go.
- `file`
  - A file opened with `Eio.Path.open_in` or `Eio.Path.with_open_in`.
- `cell_parser`
  - A cell parser converts from XLSX types to your own data type (usually a variant).
  - Use `Xlsx.string_cell_parser` or `Xlsx.yojson_cell_parser` to get started quickly, then make your own.

#### Returns:
`'a Xlsx.row Sequence.t` where `'a` is the type of your cell_parser.

Note that a `Sequence.t` is an ephemeral data structure. Each element can only be seen once, so for example calling `Sequence.is_empty` will attempt to generate the next element in the Sequence to return true if there was one, _but that element will be lost_.

SZXX generates elements on demand (lazily), meaning that it will not begin parsing until you begin reading from the Sequence.

Note that you must consume the Sequence from within the Switch. Attempting to read from the Sequence from outside the Switch will fail with `Invalid_argument "Coroutine has already failed"`. The Sequence's contents can safely leave the Switch.

### Xlsx.stream_rows_single_pass

This function extracts rows from an XLSX document. Rows encountered **before** the SST will be buffered (to be streamed out once we've encountered the SST), or dropped (to minimize buffering).

You get full control over any and all buffering.

```ocaml
Xlsx.stream_rows_single_pass ?max_buffering ?filter ?filter_sheets ~sw ~feed cell_parser
```

#### Arguments:
- `max_buffering`
  - Default: unlimited.
  - Sets a limit to the number of rows that may be buffered. Raises an exception if it runs out of buffer space before reaching the SST.
- `filter`
  - Use this filter to drop uninteresting rows and reduce the number of rows that must be buffered.
  - If necessary, use `Xlsx.Expert.parse_row_without_sst` to access cell-level data. See the examples below.
  - This function is called on every row of every sheet (except for sheets removed by `filter_sheets`).
- `filter_sheets`
  - Default: all sheets.
  - Pass a function to limit parsing/extraction to only certain sheets.
  - Sheet IDs start at 1. Note: it does not necessarily match the visual order of the sheets in Excel.
- `sw`
  - A regular `Eio.Switch.t`. This is where parsing errors go.
- `feed`
  - A producer of raw input data. Create a `Feed.t` by using the `SZXX.Feed` module.
- `cell_parser`
  - A cell parser converts from XLSX types to your own data type (usually a variant).
  - Use `Xlsx.string_cell_parser` or `Xlsx.yojson_cell_parser` to get started quickly, then make your own.

#### Returns:
`'a Xlsx.row Sequence.t` where `'a` is the type of your cell_parser.

Note that a `Sequence.t` is an ephemeral data structure. Each element can only be seen once, so for example calling `Sequence.is_empty` will attempt to generate the next element in the Sequence to return true if there was one, _but that element will be lost_.

SZXX generates elements on demand (lazily), meaning that it will not begin parsing until you begin reading from the Sequence.

Note that you must consume the Sequence from within the Switch. Attempting to read from the Sequence from outside the Switch will fail with `Invalid_argument "Coroutine has already failed"`. The Sequence's contents can safely leave the Switch.

#### Example 1

Let's read a file in a single pass.

For this example we'll pretend we have 3 requirements:
- **R1:** we're only interested in the first 1000 rows
- **R2:** and only the rows where the `K` column (11th column) contains a number `>= 100`
- **R3:** and we only care about sheet #1

We'll use `Xlsx.yojson_cell_parser` to keep things simple.

First, let's define our `filter` function:
```ocaml
let open SZXX in

let filter raw =
  let open Xlsx in
  (* R1: If the row number is over 1000 we don't need to parse the row at all *)
  if raw.row_number > 1000
  then false
  else (
    (* Let's parse the row to the best that we can do without the SST,
       unless we feel like working with raw XML (and we sure don't!) *)
    let row = Expert.parse_row_without_sst yojson_cell_parser raw in

    (* R2: 10th column is the "K" column we're interested in *)
    match List.nth row.data 10 with
    | Some (Available (`Float x)) ->
      (* Keep the row only if the number is >= 100 *)
      Float.(x >= 100.0)

    | Some (Available _) (* The cell in K column isn't a Number *)
    | Some (Delayed _) (* The cell in K column is a non-inlined String *)
    | None (* There's fewer than 11 columns *)
      -> false )
in
```
Our `filter` function guarantees we'll never buffer more than 1000 rows, so we don't really need to pass `~max_buffering`.
```ocaml
(* The Switch receives any parsing errors *)
Switch.run @@ fun sw ->
let src : Eio.Flow.source =
  (* Some kind of data flow such as an HTTP transfer *)
in
let seq =
  Xlsx.stream_rows_single_pass
    ~max_buffering:1000 (* Not necessary unless we made a mistake in ~filter *)
    ~filter
    ~filter_sheets:(fun ~sheet_id ~raw_size:_ ->
      (* R3: we only care about sheet #1 *)
      sheet_id = 1
    )
    ~sw
    ~feed:(SZXX.Feed.of_flow src) (* See SZXX.Feed for other ways to construct a ~feed *)
    Xlsx.yojson_cell_parser
in
```
We can then now work with the data in `seq` as usual.

#### Example 2

For this example let's just count the number of rows across all sheets in the XLSX, with zero buffering.
```ocaml
let open SZXX in
let src : Eio.Flow.source =
  (* Some kind of data flow such as an HTTP transfer *)
in
let count = ref 0 in
let filter _ =
  incr count;
  false (* Always reject *)
in
let seq =
  Xlsx.stream_rows_single_pass
    ~filter
    ~sw
    ~feed:(SZXX.Feed.of_flow src) (* See SZXX.Feed for other ways to construct a ~feed *)
    Xlsx.yojson_cell_parser
in

(* Optional sanity check *)
Sequence.iter seq ~f:(fun _ -> assert false);

(* Print our count! *)
Eio.traceln "Found %d rows" !count;
```

There are two major benefits to using `~filter` in this way:
1. we don't waste any time parsing the rows themselves; `~filter` operates on raw rows
2. we're guaranteed to run in constant memory, no matter the XLSX's internal layout

### Xlsx.parse_date

XLSX dates are stored as floats. This function converts from a `float` to a `Date.t`.

Note: the XLSX type `Date` exists, but it's rarely ever used.

### Xlsx.parse_datetime

XLSX datetimes are stored as floats. This function converts from a `float` to a `Time_float.t`.

### Xlsx.index_of_column

This function converts a column reference such as `"D7"` or `"AA2"` to a 0-based column index.

### Xlsx.Expert (module)

See [xlsx.mli](src/xlsx.mli).

## SZXX.Xml

This XML parser is not fully spec-compliant: it does not attempt to validate schemas or follow references.

But it's simple, efficient and sufficient for the vast majority of use cases.

It can be configured to parse HTML. It can also stream nested elements out of large XML files without having to hold the whole file in memory.

```ocaml
open! Core
open Eio.Std

let parse_xml xml_path =
  let open SZXX in
  let doc =
    Eio.Path.with_open_in
      Eio.Path.(Eio.Stdenv.fs env / xml_path)
      (fun file -> Xml.parse_document (Feed.of_flow file))
    |> Result.ok_or_failwith
  in
  (* Do something with the doc using the utilities in Xml.DOM *)
```

To operate on the document and its children, use the utility functions in the [`Xml.DOM` module](src/xml.mli).

This module transparently unescapes text nodes. Your text nodes will contain the correct e.g. `Fast & Furious 🏎️` instead of `Fast &amp; Furious &#x1F3CE;&#xFE0F;`.

### Xml.parse_document

This function progressively assembles an XML document while reading from the raw input. It begins parsing without having to read the whole input in its entirety.

To extract (stream out) specific nested elements instead of fully parsing (very) large documents, use [Xml.stream_matching_elements](#xmlstream_matching_elements) instead.

```ocaml
Xml.parse_document ?parser ?strict feed
```

#### Arguments:
- `parser`
  - Override the default parser.
  - Make your own parser with `Xml.SAX.make_parser` or pass `Xml.html_parser`.
- `strict`
  - Default: `true`.
  - When `false`, unclosed elements are treated as self-closing elements, HTML-style. For example a `<br>` without a matching `</br>` will be treated as a self-closing `<br />`.
- `feed`
  - A producer of raw input data. Create a `Feed.t` by using the `SZXX.Feed` module.

#### Returns:
`Xml.document` or an error message.

### Working with HTML

Let's adapt the previous example and add HTML support:
- add `~parser:Xml.html_parser`
- add `~strict:false`

```ocaml
open! Core
open Eio.Std

let parse_html html_path =
  let open SZXX in
  let doc =
    Eio.Path.with_open_in
      Eio.Path.(Eio.Stdenv.fs env / html_path)
      (fun file ->
        Xml.parse_document
          ~parser:Xml.html_parser (* for HTML *)
          ~strict:false (* for HTML *)
          (Feed.of_flow file) )
    |> Result.ok_or_failwith
  )
  in

  (* Let's extract the text of the first <h1> directly within the first <div> directly within <body> *)
  let text =
    doc.top |> Xml.DOM.(get [ dot "body"; dot "div" ]) |> Option.bind ~f:(Xml.DOM.dot_text "h1")
  in
```

### Xml.parse_document_from_string

Convenience function equivalent to `Xml.parse_document (Feed.of_string some_string)`.

### Xml.stream_matching_elements

This function progressively assembles an XML document, but every element that matches `filter_path` is passed to `on_match` instead of being added to the DOM. This "shallow DOM" is then returned. It begins parsing without having to read the whole input in its entirety.

```ocaml
Xml.stream_matching_elements ?parser ?strict ~filter_path ~on_match feed
```

#### Arguments:
- `parser`
  - Override the default parser.
  - Make your own parser with `Xml.SAX.make_parser` or pass `Xml.html_parser`.
- `strict`
  - Default: `true`.
  - When `false`, unclosed elements are treated as self-closing elements, HTML-style. For example a `<br>` without a matching `</br>` will be treated as a self-closing `<br />`.
- `feed`
  - A producer of raw input data. Create a `Feed.t` by using the `SZXX.Feed` module.
- `filter_path`
  - Indicates which part of the DOM should be streamed out instead of being stored in the DOM.
  - For example `["html"; "body"; "div"; "div"; "p"]` will emit all the `<p>` tags nested inside exactly 2 levels of `<div>` tags in an HTML document.
- `on_match`
  - Called on every element that matched `filter_path`.

#### Returns:
A "shallow" `Xml.document` or an error message.

### Xml.DOM (module)

This module contains various utilities to traverse and process XML documents. See [xml.mli](src/xml.mli).
- `Xml.DOM.get_attr`: get attribute value
- `Xml.DOM.dot`: get first child element by tag name
- `Xml.DOM.dot_text`: get first child element's inner text by tag name
- `Xml.DOM.at`: get the nth child element
- `Xml.DOM.filter_map`: filter_map child elements
- etc

### Xml.Expert (module)

See [xml.mli](src/xml.mli). Performs no automatic escaping.

## SZXX.Zip

This ZIP parser is fully featured and will parse every common type of ZIP found in the wild: every subtype of ZIP `2.0` and `4.5`, with compression methods `0` or `8`.

Other types of ZIPs are rare and not realistically expected to be understood by applications other than the one that created it.

There are two ways to use with `SZXX.Zip`:
- if you only need a subset of files stored in a ZIP **AND** your ZIP is a file (not a stream)
  - ➡️ use `Zip.index_entries` and `Zip.extract_from_index`
- otherwise:
  - ➡️ use `Zip.stream_files`

### Zip.stream_files

This function processes a ZIP one file at a time.

For each file, it invokes a `callback` to ask what it should do with the file, then processes that file, adds the outcome to the output Sequence, then invokes `callback` for the next file and so on.

In this example, we will extract all files that end in `.jpg`, skipping over any other files contained within the ZIP archive.

```ocaml
open! Core
open Eio.Std

let save_jpg ~filename contents = ...
  (* Let's pretend this function saves a .jpg file somewhere *)

let unzip_and_save_jpgs zip_path =
  let open SZXX in
  (* The Switch receives any parsing errors *)
  Switch.run @@ fun sw ->

  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / zip_path)) in

  let callback = function
    | ({ filename; _ } : Zip.entry) when String.is_suffix (String.lowercase filename) ~suffix:".jpg" ->
      (* Here we'll simply extract each .jpg file into a string *)
      Zip.Action.String
    | _ ->
      (* All other files: skip/ignore them *)
      Zip.Action.Fast_skip
  in

  let seq = Zip.stream_files ~sw ~feed:(Feed.of_flow file) callback in

  Sequence.iter seq ~f:(fun (entry, data) ->
    match data with
    | Zip.Data.String contents -> save_jpg ~filename:entry.filename contents
    | _ -> () )
```

Note that in a real world scenario, we should process each JPG in small chunks instead of decompressing each of them into a (potentially large) string. For that we would use `Zip.Action.Fold_string` or `Zip.Action.Fold_bigstring`.

SZXX will call `callback` for each file it encounters within the ZIP archive. You must choose an Action for SZXX to perform on each file.

#### Actions:
- `Action.Skip`
  - Skip over the compressed bytes of this file without attempting to decompress them.
  - It will still validate the file's integrity as usual
- `Action.Fast_skip`
  - Skip over the compressed bytes without attempting to decompress or validate them
- `Action.String`
  - Collect the whole decompressed file into a single string.
- `Action.Bigstring`
  - Collect the whole decompressed file into a single bigstring.
  - More efficient than `Action.String` if you don't need to convert the final result into a string later on.
- `Action.Fold_string f`
  - Fold this file into a final state using function `f`, in string chunks of ~8192 bytes.
- `Action.Fold_bigstring f`
  - Fold this file into a final state using function `f`, in bigstring chunks of ~8192 bytes.
  - **IMPORTANT:** this `Bigstring.t` is volatile! It's only safe to read from it until the end of function `f` (the "folder" function). If you need to access the data again later, make a copy of it in before the end of function `f`.
- `Action.Parse p`
  - Apply the `Angstrom.t` parser `p` to the file while it is being decompressed without having to fully decompress it first.
  - `Action.Parse` expects `p` to consume all bytes and leave no trailing junk bytes after a successful parse.
- `Action.Parse_many { parser; on_parse }`
  - Repeatedly apply the `Angstrom.t` parser to the file while it is being decompressed without having to fully decompress it first.
  - It invokes `on_parse` on each parsed value.
  - `Action.Parse_many` expects the file to end on a complete parse and leave no trailing junk bytes.
- `Action.Terminate`
  - Abruptly terminate processing of the ZIP archive.
  - The output Sequence will finish with a `Data.Terminate` element.
  - SZXX stops reading from the `Feed.t` immediately, without even skipping over the bytes of that entry.


```ocaml
Zip.stream_files ~sw ~feed callback
```

#### Arguments:
- `sw`
  - A regular `Eio.Switch.t`. This is where parsing errors go.
- `feed`
  - A producer of raw input data. Create a `Feed.t` by using the `SZXX.Feed` module.
- `callback`
  - A function called on every file found within the ZIP archive.
  - You must choose an Action (`Zip.Action.t`) to perform over each file encountered within the ZIP archive.

#### Returns:
A `Sequence.t` of all files within the archive.
The order of the files passed to the `callback` and on the Sequence matches the arrangement of the files within the ZIP.

The Sequence contains `Zip.Data.t` elements that match the `Zip.Action.t` returned by the `callback`: `Zip.Action.Fold_string` yields a `Zip.Data.Fold_string` on the Sequence, `Zip.Action.Skip` yields `Zip.Data.Skip`, etc.

Note that a `Sequence.t` is an ephemeral data structure. Each element can only be seen once, so for example calling `Sequence.is_empty` will attempt to generate the next element in the Sequence to return true if there was one, _but that element will be lost_.

SZXX generates elements on demand (lazily), meaning that it will not begin parsing until you begin reading from the Sequence.

Note that you must consume the Sequence from within the Switch. Attempting to read from the Sequence from outside the Switch will fail with `Invalid_argument "Coroutine has already failed"`. The Sequence's contents can safely leave the Switch.

### Zip.index_entries

This function quickly indexes the contents of the ZIP file and returns a list of `Zip.entry`.

You can then extract files one by one (in any order) using `Zip.extract_from_index`.

```ocaml
Zip.index_entries file
```

#### Arguments:
- `file`
  - A file opened with `Eio.Path.open_in` or `Eio.Path.with_open_in`.

#### Returns:
`Zip.entry list`

### Zip.extract_from_index

This function extracts a single file from a ZIP file.

```ocaml
open! Core
open SZXX

let get_file_contents zip_path =
  Eio.Path.(with_open_in (Eio.Stdenv.fs env / zip_path)) (fun file ->
    let entries = Zip.index_entries file in
    let entry = List.filter entries ~f:(fun entry -> ...) in

    let file_contents =
      match Zip.extract_from_index file entry Zip.Action.String with
      | Zip.Data.String s -> s
      | _ -> assert false
    in

    file_contents
  )
```

```ocaml
Zip.extract_from_index file entry action
```

See [Actions](#actions) for the meaning of `Zip.Action.*` and `Zip.Data.*`

#### Arguments:
- `file`
  - A file opened with `Eio.Path.open_in` or `Eio.Path.with_open_in`.
- `entry`
  - A `Zip.entry` value returned by `Zip.index_entries`
- `action`
  - See [Actions](#actions)

#### Returns:
A `Zip.Data.t` value that matches your `action` argument.

## FAQ

### Why am I getting a "Coroutine" error?

Because you are reading a Sequence from outside the Switch that populates the Sequence.

This restriction applies to the following 3 SZXX functions that take a Switch argument and return a Sequence:
- `SZXX.Xlsx.stream_rows_double_pass`
- `SZXX.Xlsx.stream_rows_single_pass`
- `SZXX.Zip.stream_files`

To fix your code: only interact with the Sequence while inside the Switch. In other words: do not let the Sequence escape the Switch scope.

The Sequence's contents can safely leave the Switch.

### Is it fast?

Given similar optimization efforts, streaming parsers should always be slower than parsers that deserialize a whole file into memory and read from that.

However, SZXX has received **extensive** performance optimizations, to the point that SZXX is faster than many non-streaming XLSX libraries.

It takes a lot of CPU work to extract each row from an XLSX file:
- the ZIP format was designed for floppy disks and old hard drives, not parallelism or SSDs
- the XML format is quite verbose and inefficient
- the XLSX format requires reading and parsing **a lot** of XML data just to produce a single row of XLSX output

All in all:
- `SZXX.Zip` and `SZXX.Xml` are fast
- `SZXX.Xlsx` is extremely fast for a streaming parser, and fast compared to the average non-streaming parser that loads everything into memory.

Using 1 core on an ancient 2015 Macbook Pro, SZXX processes an enormous 28-column x 1,048,576-row XLSX file in 55 seconds **using only 9MB of memory**. In 2022, the same file used to take 70 seconds to open in LibreOffice using 2 cores and **1.8GB of memory**, but improvements to LibreOffice have recently brought it down to 36 seconds and **1.1GB**.

Where SZXX wins is on memory usage and predictability.

### Any performance tips?

Use flambda. Install the OPAM package named `ocaml-option-flambda` and add the following to your `dune` `(executable)`:
```
(ocamlopt_flags -O3)
```

### Can I run it on my _insert exotic CPU here_?

SZXX assumes a 64-bit CPU architecture.

It will probably work on 32-bit as well as long as the ZIP and XLSX files are small (<2Gb per zipped file or sheet).

Please report back if you try it.

### Does it work on Windows?

Maybe. Probably. It depends on [Eio's level of Windows support](https://github.com/ocaml-multicore/eio/issues/125). You'll probably need OCaml 5.1 too.

### Does it work in the browser?

Version 3 [worked but needed a special fork of angstrom](https://github.com/asemio/SZXX/tree/f0e35f34298e03fa1b7a561c7d64179bd356f4ca#does-it-work-in-the-browser).

Version 4 will work too once [Eio's level of JS support](https://github.com/ocaml-multicore/eio/pull/405) is sufficient. It may very well be sufficient today already.

You will definitely need to edit your `dune` files' `(libraries)` section.

Replace
```
(libraries SZXX some-other-lib)
```
with
```
(libraries checkseum.ocaml SZXX some-other-lib)
```
`checkseum.ocaml` has to be **before** SZXX [for it to work](https://github.com/mirage/checkseum/tree/96f959a8f473bf1e2061572c0579288d53afb243#linking-trick--variant).

You may or may not also need to use the aforementioned angstrom fork.

Please give it a try and report back.
