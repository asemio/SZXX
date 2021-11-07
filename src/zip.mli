open! Core_kernel

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
    | Fold_string    of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstring.t -> len:int -> 'a -> 'a;
      }
    | Parse          of 'a Angstrom.t
end

module Data : sig
  type 'a t =
    | Skip
    | String         of string
    | Fold_string    of 'a
    | Fold_bigstring of 'a
    | Parse          of ('a, string) result
end

type 'a slice = {
  buf: 'a;
  pos: int;
  len: int;
}

type feed =
  | String    of (unit -> string option Lwt.t)
  | Bigstring of (unit -> Bigstring.t slice option Lwt.t)

(**
   Stream files.

   [SZXX.Zip.stream_files ~read callback]

   [read]: Produces data for the parser. Return [None] to indicate EOF.

   [callback]: function called on every file found within the zip archive.
   Returning [Action.Skip] will skip over the compressed bytes of this file without attempting to uncompress them.
   Returning [Action.String] will collect the whole uncompressed file into a single string.
   Returning [Action.Chunk] will call the user-defined function for every small chunk (~1-5Kb) comprising the uncompressed file.
   Returning [Action.Parse] will apply an [Angstrom.t] parser to the file while it is being uncompressed without having to fully uncompress it first.

   The final stream returns all files in the same order they were found in the archive.

   [unit promise]: A promise resolved once the entire zip archive has been processed.
   It is important to bind to/await this promise in order to capture any errors encountered while processing the file.
*)
val stream_files : feed:feed -> (entry -> 'a Action.t) -> (entry * 'a Data.t) Lwt_stream.t * unit Lwt.t
