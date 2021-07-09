open! Core_kernel

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: int;
  uncompressed_size: int;
}
[@@deriving sexp_of]

type entry = {
  version_needed: int;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra: string;
}
[@@deriving sexp_of]

module Action : sig
  type 'a t =
    | Skip
    | String
    | Chunk  of (string -> unit)
    | Parse  of 'a Angstrom.t
end

module Data : sig
  type 'a t =
    | Skip
    | String of string
    | Chunk
    | Parse  of ('a, string) result
end

(**
   Stream rows from an [Lwt_io.input_channel].

   [SZXX.Zip.stream_files ic callback]

   [ic]: The channel to read from

   [callback]: function called on every file found within the zip archive.
   Returning [Action.Skip] will skip over the compressed bytes of this file without attempting to uncompress them.
   Returning [Action.String] will collect the whole uncompressed file into a single string.
   Returning [Action.Chunk] will call the user-defined function for every small chunk (~1Kb) comprising the uncompressed file.
   Returning [Action.Parse] will apply an [Angstrom.t] parser to the file while it is being uncompressed without having to fully uncompress it first.

   The final stream returns all files in the same order they were found in the archive.

   [unit promise]: A promise resolved once the entire zip archive has been processed.
   It is important to bind to/await this promise in order to capture any errors encountered while processing the file.
*)
val stream_files :
  Lwt_io.input_channel -> (entry -> 'a Action.t) -> (entry * 'a Data.t) Lwt_stream.t * unit Lwt.t
