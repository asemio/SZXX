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

(** Same as [of_flow], but the resulting [Feed.t] does not advance the file cursor.
    In other words, even after processing, the flow will still appear to be "unread".
    Note: this function requires a flow that supports seeking, such as files. *)
let of_flow_non_seeking (src : #Eio.File.ro) : t =
  let buf = Cstruct.create 4096 in
  let pos = ref Optint.Int63.zero in
  fun () ->
    (* TODO: Use [Eio.File.pread] once https://github.com/ocaml-multicore/eio/issues/579 is fixed *)
    match src#pread [ buf ] ~file_offset:!pos with
    | 0 -> `Eof
    | len ->
      (pos := Optint.Int63.(add !pos (of_int len)));
      `Bigstring (Bigstringaf.sub ~off:0 ~len buf.buffer)
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

(** Note: not ideal for extremely large strings. For that, use the other functions in this module. *)
let of_string s : t =
  let size = String.length s in
  let slice_size = 4096 in
  (* By copying the string into a sequence of Bigstrings,
     we can reclaim memory as soon as the large string goes out of scope
     and the sequence starts being processed. *)
  let seq =
    if size < slice_size
    then Sequence.singleton (Bigstring.of_string s)
    else (
      let n = size / slice_size in
      let rem = size - (slice_size * n) in
      let seq1 =
        List.init n ~f:(fun i -> Bigstringaf.of_string s ~off:(i * slice_size) ~len:slice_size)
        |> Sequence.of_list
      in
      let seq2 =
        if rem = 0
        then Sequence.empty
        else Sequence.singleton (Bigstringaf.of_string s ~off:(n * slice_size) ~len:rem)
      in
      Sequence.append seq1 seq2 )
  in
  fun () ->
    match Sequence.hd seq with
    | None -> `Eof
    | Some bs -> `Bigstring bs
