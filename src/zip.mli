open! Core
open Eio.Std

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of]

type version =
  | Zip_2_0
  | Zip_4_5
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: Int64.t;
  uncompressed_size: Int64.t;
}
[@@deriving sexp_of]

type extra_field = {
  id: int;
  size: int;
  data: string;
}
[@@deriving sexp_of]

type entry = {
  version_needed: version;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra_fields: extra_field list;
}
[@@deriving sexp_of]

module Action : sig
  type 'a t =
    | Skip
    | String
    | Fold_string of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstring.t -> 'a -> 'a;
      }
    | Parse of 'a Angstrom.t
    | Parse_many of {
        parser: 'a Angstrom.t;
        on_parse: 'a -> unit;
      }
end

module Data : sig
  type 'a parser_state =
    | Success of 'a
    | Failed of {
        error: string;
        unconsumed: string;
      }
    | Terminated_early of { unconsumed: string }
    | Incomplete

  val parser_state_to_result : 'a parser_state -> ('a, string) result

  type 'a t =
    | Skip
    | String of string
    | Fold_string of 'a
    | Fold_bigstring of 'a
    | Parse of 'a parser_state
    | Parse_many of unit parser_state
end

type feed =
  | String of (unit -> string option)
  | Bigstring of (unit -> Bigstring.t option)

(**
   Stream files.

   [SZXX.Zip.stream_files ~feed callback]

   [feed]: Produces data for the parser. This data can be simple strings or bigstrings. Return [None] to indicate EOF.

   [callback]: function called on every file found within the ZIP archive.
   You must choose an Action for SZXX to perform over each file encountered within the ZIP archive.

   Return [Action.Skip] to skip over the compressed bytes of this file without attempting to uncompress them.
   Return [Action.String] to collect the whole uncompressed file into a single string.
   Return [Action.Fold_string] to fold this file into a final state, in string chunks of ~1k-5k.
   Return [Action.Fold_bigstring] to fold this file into a final state, in bigstring chunks of ~1k-5k.
   Return [Action.Parse] to apply an [Angstrom.t] parser to the file while it is being uncompressed without having to fully uncompress it first.

   This function returns [stream * success_promise]
   [stream] contains all files in the same order they were found in the archive.
   [success_promise] is a promise that resolves once the entire zip archive has been processed.

   Important: bind to/await [success_promise] in order to capture any errors encountered while processing the file.

   See README.md for examples on how to use it.
*)
val stream_files :
  sw:Switch.t ->
  feed:feed ->
  ?dispatcher:Dispatcher.t ->
  (entry -> 'a Action.t) ->
  (entry * 'a Data.t) option Eio.Stream.t
