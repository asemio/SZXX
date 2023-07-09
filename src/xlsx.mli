open! Core
open Eio.Std

type location = {
  sheet_number: int;
  row_number: int;
  col_index: int;
}
[@@deriving sexp_of]

type 'a cell_parser = {
  string: location -> string -> 'a;
  formula: location -> formula:string -> string -> 'a;
  error: location -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
  date: location -> string -> 'a;
  null: 'a;
}

type delayed_string = {
  location: location;
  sst_index: string;
}
[@@deriving sexp_of]

type 'a status =
  | Available of 'a
  | Delayed of delayed_string
[@@deriving sexp_of]

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a list;
}
[@@deriving sexp_of]

module SST : sig
  type t

  val filter_path : string list

  val zip_entry_filename : string

  val from_zip : feed:Feed.t -> t
end

val to_seq : 'a option Eio.Stream.t -> 'a Seq.t

(**
   Returns an [Lwt_stream.t] of fully parsed XLSX rows, with one caveat: every cell value is wrapped in ['a status].
   The [status] can be either [Available of 'a] or [Delayed of delayed_string].

   This function will not wait on delayed string cells (see README).

   Returns [stream * sst_promise * success_promise].

   Await [success_promise] before awaiting the completion of any promise resulting from the stream.

   See README.md for examples.
*)
val stream_rows :
  ?only_sheet:int ->
  ?skip_sst:bool ->
  sw:Switch.t ->
  feed:Feed.t ->
  'a cell_parser ->
  'a status row option Eio.Stream.t * SST.t Promise.or_exn

val stream_rows_double_pass :
  ?only_sheet:int -> sw:Switch.t -> #Eio.File.ro -> 'a cell_parser -> 'a row option Eio.Stream.t

val with_minimal_buffering :
  'a option Eio.Stream.t -> SST.t Promise.or_exn -> parse:(sst:SST.t -> 'a -> 'c) -> 'c Seq.t

(**
  Same as [stream_rows] but automatically resolves string references.
  Warning: This function can result in linear (as opposed to constant) memory usage.
  See README for more information.
*)
val stream_rows_buffer : ?only_sheet:int -> sw:Switch.t -> feed:Feed.t -> 'a cell_parser -> 'a row Seq.t

(**
  Same as [stream_rows] but returns raw XML elements instead of parsed XLSX rows.
  This function can be useful to filter out uninteresting rows at a lower cost.
*)
val stream_rows_unparsed :
  ?only_sheet:int ->
  ?skip_sst:bool ->
  sw:Switch.t ->
  feed:Feed.t ->
  unit ->
  Xml.DOM.element row option Eio.Stream.t * SST.t Promise.or_exn

(** Convenience cell_parser to read rows as JSON (Yojson) *)
val yojson_cell_parser : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_parser

(** Convert an XML element as returned by [stream_rows_unparsed] into a nicer ['a row] as returned by [stream_rows_buffer] *)
val parse_row_with_sst : SST.t -> 'a cell_parser -> Xml.DOM.element row -> 'a row

(** Convert an XML element as returned by [stream_rows_unparsed] into a nicer ['a status row] as returned by [stream_rows] *)
val parse_row_without_sst : 'a cell_parser -> Xml.DOM.element row -> 'a status row

(**
   Unwrap a single row, resolving all SST references.

   A common workflow is to call [Lwt_stream.filter] on the stream returned by [stream_rows],
   discarding uninteresting rows in order to buffer as few rows as possible,
   then await the [sst Lwt.t], and finally consume the stream, calling [unwrap_status] on each row to get the String data.
*)
val unwrap_status : 'a cell_parser -> SST.t -> 'a status row -> 'a row

(** Resolve a single reference into the Shared Strings Table. *)
val resolve_sst_index : SST.t -> sst_index:string -> string option

(** XLSX dates are stored as floats. Convert from a [float] to a [Date.t] *)
val parse_date : float -> Date.t

(** XLSX datetimes are stored as floats. Convert from a [float] to a [Time_float.t] *)
val parse_datetime : zone:Time_float.Zone.t -> float -> Time_float.t

(** Convert from a column reference such as ["D7"] or ["AA2"] to a 0-based column index *)
val index_of_column : string -> int
