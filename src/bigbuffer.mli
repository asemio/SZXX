open! Base

type t

val length : t -> int

val create : int -> t

val contents : t -> string

val volatile_contents : t -> Bigstringaf.t

val add_char : t -> char -> unit

val nth : t -> int -> char

val clear : t -> unit

val reset : t -> unit

val add_substring : t -> string -> pos:int -> len:int -> unit

val add_subbytes : t -> bytes -> pos:int -> len:int -> unit

val add_bigstring : t -> Bigstringaf.t -> unit

val add_string : t -> string -> unit

val add_bytes : t -> bytes -> unit

val add_buffer : t -> t -> unit
