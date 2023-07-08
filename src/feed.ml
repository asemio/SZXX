open! Core

(** Feeds can also be created manually without going through the convenience functions in this module *)
type t = unit -> [ `String of string | `Bigstring of Bigstring.t | `Eof ]

(** Create a [Feed.t] from a standard [Eio.Flow.source] *)
let of_flow (src : #Eio.Flow.source) : t =
  let buf = Cstruct.create 4096 in
  fun () ->
    match Eio.Flow.single_read src buf with
    | len -> `Bigstring (Bigstringaf.sub ~off:0 ~len buf.buffer)
    | exception End_of_file -> `Eof

(** Return None to indicate End Of File *)
let of_string_dispenser f : t =
 fun () ->
  match f () with
  | Some s -> `String s
  | None -> `Eof

(** Return None to indicate End Of File *)
let of_bigstring_dispenser f : t =
 fun () ->
  match f () with
  | Some bs -> `Bigstring bs
  | None -> `Eof
