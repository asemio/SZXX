open! Core
open Angstrom

type storage = {
  add: Bigstring.t -> int -> unit;
  finalize: unit -> unit;
}

let slice_size = 1024

let slice_bits = 10

(* Boyer–Moore–Horspool algorithm *)
type bmh =
  | Found
  | Shift of int
  | Restart

let make_table ~pattern len =
  let table = Array.create ~len:256 Restart in
  String.foldi pattern ~init:() ~f:(fun i () c ->
    if i < len - 1 then table.(Char.to_int c) <- Shift (len - (i + 1)) );
  table

let same ~pattern table s =
  let rec loop s = function
    | -1 -> Found
    | i when Char.( = ) s.[i] pattern.[i] -> loop s (i - 1)
    | _ -> table.(Char.to_int s.[String.length pattern - 1])
  in
  loop s

let skip_until_pattern ~pattern =
  let len = String.length pattern in
  let table = make_table ~pattern len in
  let rec loop window =
    match same ~pattern table window (len - 1) with
    | Found -> return ()
    | Shift by -> take by >>= fun more -> (loop [@tailcall]) (String.drop_prefix window by ^ more)
    | Restart -> take len >>= fun s -> (loop [@tailcall]) s
  in
  take len >>= loop

let take_until_pattern ~pattern =
  let buf = Buffer.create 32 in
  let len = String.length pattern in
  let table = make_table ~pattern len in
  let rec loop window =
    match same ~pattern table window (len - 1) with
    | Found ->
      let s = Buffer.contents buf in
      if Buffer.length buf > slice_size then Buffer.reset buf else Buffer.clear buf;
      return s
    | Shift by ->
      take by >>= fun more ->
      Buffer.add_substring buf window ~pos:0 ~len:by;
      (loop [@tailcall]) (String.drop_prefix window by ^ more)
    | Restart ->
      take len >>= fun more ->
      Buffer.add_string buf window;
      (loop [@tailcall]) more
  in
  take len >>= loop

let bounded_file_reader ~pattern { add; finalize } =
  let len = String.length pattern in
  let buf = Bigstring.create (slice_size + len) in
  let partial = Bytes.create len in
  let pos = ref 0 in
  let table = make_table ~pattern len in
  let flush ~src_len src =
    Bigstring.From_string.unsafe_blit ~src ~src_pos:0 ~dst:buf ~dst_pos:!pos ~len:src_len;
    let len = !pos + src_len in
    pos := len;
    if len > slice_size
    then (
      add buf len;
      pos := 0 )
  in
  let rec loop window =
    match same ~pattern table window (len - 1) with
    | Found ->
      if !pos > 0 then add buf !pos;
      pos := 0;
      finalize ();
      commit
    | Shift by ->
      take by >>= fun more ->
      flush ~src_len:by window;
      commit >>= fun () ->
      let diff = len - by in
      Bytes.From_string.unsafe_blit ~src:window ~src_pos:by ~dst:partial ~dst_pos:0 ~len:diff;
      Bytes.From_string.unsafe_blit ~src:more ~src_pos:0 ~dst:partial ~dst_pos:diff ~len:by;
      (loop [@tailcall]) (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:partial)
    | Restart ->
      take len >>= fun more ->
      flush ~src_len:len window;
      commit >>= fun () -> (loop [@tailcall]) more
  in
  take len >>= loop
