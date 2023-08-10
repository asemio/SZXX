open! Core
open Eio.Std

type location = {
  sheet_number: int;
  row_number: int;
  col_index: int;
}
[@@deriving sexp_of]

(** A cell parser converts from XLSX types to your own data type (usually a variant).
    Use [SZXX.Xlsx.string_cell_parser] or [SZXX.Xlsx.yojson_cell_parser] to get started quickly, then make your own. *)
type 'a cell_parser = {
  string: location -> string -> 'a;
  formula: location -> formula:string -> string -> 'a;
  error: location -> formula:string -> string -> 'a;
  boolean: location -> string -> 'a;  (** "1" for [true] *)
  number: location -> string -> 'a;  (** May contain a decimal part *)
  date: location -> string -> 'a;  (** ISO-8601 format *)
  null: 'a;
}

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a list;
}
[@@deriving sexp_of]

(** Convenience cell_parser to convert from XLSX types to String *)
val string_cell_parser : string cell_parser

(** Convenience cell_parser to convert from XLSX types to JSON *)
val yojson_cell_parser : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_parser

(** XLSX dates are stored as floats. Convert from a [float] to a [Date.t] *)
val parse_date : float -> Date.t

(** XLSX datetimes are stored as floats. Convert from a [float] to a [Time_float.t] *)
val parse_datetime : zone:Time_float.Zone.t -> float -> Time_float.t

(** Convert from a column reference such as "D7" or "AA2" to a 0-based column index *)
val index_of_column : string -> int

(** Stream parsed rows from an XLSX file.
    This functions is GUARANTEED to run in constant memory, without buffering.

    [SZXX.Xlsx.stream_rows_double_pass ?only_sheet ~sw file cell_parser]

    [filter_sheets]: Default: all sheets. Sheet IDs start at 1.
      Note: it does not necessarily match the order of the sheets in Excel.

    [sw]: A regular [Eio.Switch.t]

    [file]: A file opened with [Eio.Path.open_in] or [Eio.Path.with_open_in].
      If your XLSX document is not a file (e.g. an HTTP transfer), then use [SZXX.Xlsx.stream_rows_single_pass]

    [cell_parser]: A cell parser converts from XLSX types to your own data type (usually a variant).
      Use [SZXX.Xlsx.string_cell_parser] or [SZXX.Xlsx.yojson_cell_parser] to get started quickly, then make your own.

    SZXX will wait for you to consume rows from the Sequence before extracting more. *)
val stream_rows_double_pass :
  ?filter_sheets:(sheet_id:int -> raw_size:Byte_units.t -> bool) ->
  sw:Switch.t ->
  _ Eio.File.ro ->
  'a cell_parser ->
  'a row Sequence.t

(** Stream parsed rows from an XLSX document.
    This function will only buffer rows encountered before the SST (see [README.md]).
    Consider using [SZXX.Xlsx.stream_rows_double_pass] if your XLSX is stored as a file.

    [SZXX.Xlsx.stream_rows_single_pass ?max_buffering ?filter ?only_sheet ~sw ~feed cell_parser]

    [max_buffering]: Default: unlimited. Sets a limit to the number of rows that may be buffered.
      Raises an exception if it runs out of buffer space before reaching the SST.

    [filter]: Use this filter to drop uninteresting rows and reduce the number of rows that must be buffered.
      If necessary, use [SZXX.Xlsx.Expert.parse_row_without_sst] to access cell-level data.
      This function is called on every row of every sheet (unless [?only_sheet] limits extraction to a single sheet).

    [filter_sheets]: Default: all sheets. Sheet IDs start at 1.
      Note: it does not necessarily match the order of the sheets in Excel.

    [sw]: A regular [Eio.Switch.t]

    [feed]: A producer of raw input data. Create a [feed] by using the [SZXX.Feed] module.

    [cell_parser]: A cell parser converts from XLSX types to your own data type (usually a variant).
      Use [SZXX.Xlsx.string_cell_parser] or [SZXX.Xlsx.yojson_cell_parser] to get started quickly, then make your own.

    As much as possible, SZXX will wait for you to consume rows from the Sequence before extracting more. *)
val stream_rows_single_pass :
  ?max_buffering:int ->
  ?filter:(Xml.DOM.element row -> bool) ->
  ?filter_sheets:(sheet_id:int -> raw_size:Byte_units.t -> bool) ->
  sw:Switch.t ->
  feed:Feed.t ->
  'a cell_parser ->
  'a row Sequence.t

module Expert : sig
  module SST : sig
    type t

    (** Advanced: for use with [SZXX.Xml.SAX.Expert.Stream.folder] *)
    val filter_path : string list

    (** Advanced: for use with [SZXX.Zip.stream_files] *)
    val zip_entry_filename : string

    (** Extract the SST from an XLSX document.
        This function will stop reading from the Feed as soon as it has retrieved the SST. *)
    val from_feed : Feed.t -> t

    (** Extract the SST from an XLSX file.
        This function does not advance the file cursor.
        It jumps around the file to only extract the SST while reading as few bytes as necessary. *)
    val from_file : _ Eio.File.ro -> t

    (** Resolve a single reference into the Shared Strings Table. *)
    val resolve_sst_index : t -> sst_index:string -> string option
  end

  type delayed_string = {
    location: location;
    sst_index: string;
  }
  [@@deriving sexp_of]

  type 'a status =
    | Available of 'a
    | Delayed of delayed_string
  [@@deriving sexp_of]

  (** Convert a raw unparsed row into a more usable "wrapped" row *)
  val parse_row_without_sst : 'a cell_parser -> Xml.DOM.element row -> 'a status row

  (** Unwrap a row, resolving all SST references *)
  val unwrap_status : 'a cell_parser -> SST.t -> 'a status row -> 'a row

  (* Single-pass equivalent of [parse_row_without_sst] followed by [unwrap_status] *)
  val parse_row_with_sst : SST.t -> 'a cell_parser -> Xml.DOM.element row -> 'a row
end
