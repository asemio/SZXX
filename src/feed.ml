open! Base

(** Feeds can also be created manually without going through the convenience functions in this module *)
type t = unit -> [ `String of string | `Bigstring of Bigstringaf.t | `Eof ]

(** Create a [Feed.t] from a standard [Eio.Flow.source] *)
let of_flow ?(slice_size = 4096) (src : _ Eio.Flow.source) : t =
  let buf = Cstruct.create slice_size in
  fun () ->
    try
      let len = Eio.Flow.single_read src buf in
      `Bigstring (Bigstringaf.sub ~off:0 ~len buf.buffer)
    with
    | End_of_file -> `Eof

(** Same as [of_flow], but the resulting [Feed.t] does not advance the file cursor.
    In other words, even after processing, the flow will still appear to be "unread".
    Note: this function requires a flow that supports seeking, such as files. *)
let of_flow_seekable ?(slice_size = 4096) ?(offset = 0) (src : _ Eio.File.ro) : t =
  let buf = Cstruct.create slice_size in
  let pos = ref (Optint.Int63.of_int offset) in
  fun () ->
    try
      let len = Eio.File.pread src [ buf ] ~file_offset:!pos in
      (pos := Optint.Int63.(add !pos (of_int len)));
      `Bigstring (Bigstringaf.sub ~off:0 ~len buf.buffer)
    with
    | End_of_file -> `Eof

(** Return [None] to indicate End Of File *)
let of_string_dispenser f : t =
 fun () ->
  match f () with
  | Some s -> `String s
  | None -> `Eof

(** Return [None] to indicate End Of File *)
let of_bigstring_dispenser f : t =
 fun () ->
  match f () with
  | Some bs -> `Bigstring bs
  | None -> `Eof

(** Note: not ideal for extremely large strings. For that, use the other functions in this module. *)
let of_string ?(slice_size = 4096) s : t =
  let size = String.length s in
  (* By copying the string into a sequence of Bigstrings,
     we can reclaim memory as soon as the large string goes out of scope
     and the sequence starts being processed. *)
  let chunks =
    if size < slice_size
    then [ Bigstringaf.of_string s ~off:0 ~len:(String.length s) ]
    else (
      let n = size / slice_size in
      let rem = size - (slice_size * n) in
      let init = if rem = 0 then [] else [ Bigstringaf.of_string s ~off:(n * slice_size) ~len:rem ] in
      Array.init n ~f:Fn.id
      |> Array.fold_right ~init ~f:(fun i acc ->
           Bigstringaf.of_string s ~off:(i * slice_size) ~len:slice_size :: acc ) )
  in
  let acc = ref chunks in
  fun () ->
    match !acc with
    | [] -> `Eof
    | bs :: rest ->
      acc := rest;
      `Bigstring bs
