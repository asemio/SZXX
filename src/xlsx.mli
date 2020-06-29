open! Core_kernel

type location = {
  col_index: int;
  sheet_number: int;
  row_number: int;
} [@@deriving sexp_of]

type 'a cell_of_string = {
  string: location -> string -> 'a;
  error: location -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
  null: 'a;
}

type delayed_string = {
  location: location;
  sst_index: string;
} [@@deriving sexp_of]

type 'a status =
| Available of 'a
| Delayed of delayed_string
[@@deriving sexp_of]

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a array;
} [@@deriving sexp_of]

type sst

(** Convenience reader to read rows as JSON *)
val yojson_readers : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_of_string

(** XLSX dates are stored as floats. Converts from a [float] to a [Date.t] *)
val parse_date: float -> Date.t

(** XLSX datetimes are stored as floats. Converts from a [float] to a [Time.t] *)
val parse_datetime: zone:Time.Zone.t -> float -> Time.t

(** Converts from a cell ref such as [C7] or [AA2] to a 0-based column index *)
val column_to_index: string -> int

(**
   Stream rows from an [Lwt_io.input_channel].
   SZXX does not hold onto memory any longer than it needs to.
   Most XLSX files can be streamed without buffering.
   However, some documents that make use of the Shared Strings Table (SST) will place it at the end of the Zip archive,
   forcing SZXX to buffer those rows until the SST is found in the archive.
   Using inline strings and/or placing the SST before the worksheets allows SZXX as efficiently as possible.

   [SZXX.Xlsx.stream_rows ?only_sheet readers ic]

   [only_sheet]: when present, only stream rows from this sheet, numbered from 1.

   [readers]: the parsers to convert from [string] into the type used in your application.
   [SZXX.Xlsx.yojson_readers] is provided for convenience.

   Note: you must pass your own readers if you need XML escaping, call [SZXX.Xml.unescape].

   Note: XLSX dates are encoded as numbers, call [SZXX.Xlsx.parse_date] or [SZXX.Xlsx.parse_datetime] in your readers to convert them.

   [ic]: The channel to read from

   Returned: [stream * sst promise * unit promise]

   [stream]: Lwt_stream.t of rows where the cell data is either [Available v] where [v] is of the type returned by your readers,
   or [Delayed d]. [Delayed] cells are caused by that cell's reliance on the SST.

   [sst promise]: A promise resolved once the SST is available.

   [unit promise]: A promise resolved once all the rows have been written to the stream.
   It is important to bind to/await this promise in order to capture any errors encountered while processing the file.
*)
val stream_rows:
  ?only_sheet:int ->
  'a cell_of_string ->
  Lwt_io.input_channel ->
  'a status row Lwt_stream.t * sst Lwt.t * unit Lwt.t

(**
   Unwraps a single row, resolving SST references.

   A common workflow is to filter the stream returned by [stream_rows],
   discarding uninteresting rows in order to buffer as few rows as possible,
   then await the [sst Lwt.t], and finally call [Lwt_stream.map (await_delayed ... ) stream].
*)
val await_delayed:
  'a cell_of_string ->
  sst ->
  'a status row ->
  'a row

(**
   Convenience function around [stream_rows] and [await_delayed].
   As the name implies, it will buffer any cells referencing the SST that are located before the SST.
*)
val stream_rows_buffer:
  ?only_sheet:int ->
  'a cell_of_string ->
  Lwt_io.input_channel ->
  'a row Lwt_stream.t * unit Lwt.t
