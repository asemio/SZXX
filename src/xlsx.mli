open! Core_kernel

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a array;
} [@@deriving sexp_of]

type 'a cell_reader = col_index:int -> sheet_number:int -> row_number:int -> string -> 'a

type 'a cell_of_string = {
  string: 'a cell_reader;
  error: 'a cell_reader;
  boolean: 'a cell_reader;
  number: 'a cell_reader;
  null: 'a;
}

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
   Using inline strings and/or placing the SST before the worksheets allows SZXX to run within very little memory.

   [SZXX.Xlsx.stream_rows ?only_sheet readers ic]

   [only_sheet]: when present, only stream rows from this sheet, numbered from 1.

   [readers]: the parsers to convert from [string] into the type used in your application.
   [SZXX.Xlsx.yojson_readers] is provided for convenience.

   [ic]: The channel to read from

   Returned: [stream * promise]

   [stream]: Lwt_stream.t of rows where the data is encoded as an array of the type returned by your readers.

   [promise]: A promise resolved once all the rows have been written to the stream.
   It is important to bind to (await) this promise in order to capture any errors encountered while processing the file.
*)
val stream_rows:
  ?only_sheet:int ->
  'a cell_of_string ->
  Lwt_io.input_channel ->
  'a row Lwt_stream.t * unit Lwt.t
