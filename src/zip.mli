open! Core_kernel

type methd =
| Stored
| Deflated
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: int;
  uncompressed_size: int;
} [@@deriving sexp_of]

type entry = {
  version_needed: int;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra: string;
} [@@deriving sexp_of]

type 'a action =
| Skip
| String
| Parse of ('a Angstrom.t)

type 'a data =
| Skipped
| As_string of string
| Parsed of ('a, string) result

(**
   Stream rows from an [Lwt_io.input_channel].

   [SZXX.Zip.stream_files ic callback]

   [ic]: The channel to read from

   [callback]: function called on every file found within the zip archive.
   Returning [Skip] will skip over the compressed bytes of this file without attempting to uncompress them.
   Returning [String] will collect the whole uncompressed file into a single string.
   Returning [Parse] will apply an [Angstrom] parser to the file while it is being uncompressed without having to fully uncompress it first.

   The final stream returns all files in the same order they were found in the archive.
*)
val stream_files: Lwt_io.input_channel -> (entry -> 'a action) -> (entry * 'a data) Lwt_stream.t
