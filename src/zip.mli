open! Core
open Eio.Std

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of, compare, equal]

type version =
  | Zip_2_0
  | Zip_4_5
[@@deriving sexp_of, compare, equal]

type descriptor = {
  crc: Int32.t;
  compressed_size: Int64.t;
  uncompressed_size: Int64.t;
  offset: Int64.t option;
}
[@@deriving sexp_of, compare, equal]

type extra_field = {
  id: int;
  size: int;
  data: string;
}
[@@deriving sexp_of, compare, equal]

type entry = {
  version_needed: version;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra_fields: extra_field list;
  comment: string;
}
[@@deriving sexp_of, compare, equal]

module Action : sig
  type 'a t =
    | Skip  (** Skip the compressed bytes but still validate the file's integrity *)
    | Fast_skip  (** Skip over the compressed bytes without attempting to decompress or validate them *)
    | String  (** Collect the whole decompressed file into a single string *)
    | Bigstring  (** Collect the whole decompressed file into a single bigstring *)
    | Fold_string of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }  (** Fold the file into a final state, in string chunks of ~8192 bytes *)
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstring.t -> 'a -> 'a;
      }
        (** Fold the file into a final state, in bigstring chunks of ~8192 bytes.
          IMPORTANT: this [Bigstring.t] is volatile! It's only safe to read from it until the end of function [f].
          If you need to access the data again later, copy it in some way before the end of function [f]. *)
    | Parse of 'a Angstrom.t
        (** Apply an [Angstrom.t] parser to the file while it is being decompressed without having to fully decompress it first.
           [Parse] expects the parser to consume all bytes and leave no trailing junk bytes after a successful parse. *)
    | Parse_many of {
        parser: 'a Angstrom.t;
        on_parse: 'a -> unit;
      }
        (** Repeatedly apply an [Angstrom.t] parser to the file while it is being decompressed without having to fully decompress it first.
            Call [on_parse] on each parsed value. [Parse_many] expects the file to end with a complete parse, without trailing junk bytes. *)
    | Terminate
        (** Abruptly terminate processing of the ZIP archive.
            SZXX stops reading from the [Feed.t] immediately, without even skipping over the bytes of that entry. *)
end

module Data : sig
  type 'a parser_state =
    | Success of 'a
    | Failed of {
        error: string;
        unconsumed: string;
          (** This string contains the start of the data not consumed by the parser.
              More data might have been present but it was not kept to prevent unbounded memory usage. *)
      }
    | Terminated_early of {
        unconsumed: string;
          (** This string contains the start of the data not consumed by the parser.
              More data might have been present but it was not kept to prevent unbounded memory usage. *)
      }
    | Incomplete
  [@@deriving sexp_of, compare, equal]

  val parser_state_to_result : 'a parser_state -> ('a, string) result

  type 'a t =
    | Skip
    | Fast_skip
    | String of string
    | Bigstring of Bigstring.t
    | Fold_string of 'a
    | Fold_bigstring of 'a
    | Parse of 'a parser_state
    | Parse_many of unit parser_state
    | Terminate
  [@@deriving sexp_of, compare, equal]
end

(** Stream files from a ZIP archive.
    This function operates in a single pass.

    [SZXX.Zip.stream_files ~sw ~feed callback]

    [sw]: A regular [Eio.Switch.t].

    [feed]: A producer of raw input data. Create a [feed] by using the [SZXX.Feed] module.

    [callback]: A function called on every file found within the ZIP archive.
    You must choose an Action ([SZXX.Zip.Action.t]) to perform over each file encountered within the ZIP archive.

    Return [Action.Skip] to skip over the compressed bytes of this file without attempting to decompress them. The file's integrity is still validated as usual.
    Return [Action.Fast_skip] to skip over the compressed bytes without attempting to decompress or validate them
    Return [Action.String] to collect the whole decompressed file into a single string.
    Return [Action.Bigstring] to collect the whole decompressed file into a single bigstring. More efficient than [Action.String] if you don't need to convert the result into a string.
    Return [Action.Fold_string] to fold this file into a final state, in string chunks of ~8192 bytes.
    Return [Action.Fold_bigstring] to fold this file into a final state, in bigstring chunks of ~8192 bytes. IMPORTANT: this [Bigstring.t] is volatile! It's only safe to read from it until the end of function [f] (the "folder"). If you need to access the data again later, copy it in some way before the end of function [f].
    Return [Action.Parse] to apply an [Angstrom.t] parser to the file while it is being decompressed without having to fully decompress it first. [Parse] expects the parser to consume all bytes and leave no trailing junk bytes after a successful parse.
    Return [Action.Parse_many] to repeatedly apply an [Angstrom.t] parser to the file while it is being decompressed without having to fully decompress it first. Call [on_parse] on each parsed value. [Parse_many] expects the file to end with a complete parse and leave no trailing junk bytes.
    Return [Action.Terminate] to abruptly terminate processing of the ZIP archive. The output Sequence will finish with a [Data.Terminate] element. SZXX stops reading from the [Feed.t] immediately, without even skipping over the bytes of that entry.

    This function returns a Sequence of all files in the archive.
    The order of the files passed to the [callback] and on the Sequence matches the arrangement of the files within the ZIP.

    SZXX will wait for you to consume from the Sequence before extracting more. *)
val stream_files : sw:Switch.t -> feed:Feed.t -> (entry -> 'a Action.t) -> (entry * 'a Data.t) Sequence.t

(** Return a list of all entries in a ZIP file.
    This function is more efficient than the [SZXX.Zip.stream_files] equivalent, but it can only operate on files. *)
val index_entries : #Eio.File.ro -> entry list

(** Given an entry extracted from [SZXX.Zip.index_entries], this function extracts a single specific file.
    This function is more efficient than the [SZXX.Zip.stream_files] equivalent, but it can only operate on files.  *)
val extract_from_index : #Eio.File.ro -> entry -> 'a Action.t -> 'a Data.t
