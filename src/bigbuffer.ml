(* Code adapted from https://github.com/janestreet/core/blob/f5a0268dca26861e9c00e847c98238877998e813/core/src/bigbuffer.ml *)

open! Base

type t = {
  mutable bstr: Bigstringaf.t;
  mutable pos: int;
  mutable len: int;
  init: Bigstringaf.t;
}

let resize buf more =
  let min_len = buf.len + more in
  let new_len = min_len + min_len in
  let new_buf = Bigstringaf.create new_len in
  Bigstringaf.blit buf.bstr ~src_off:0 new_buf ~dst_off:0 ~len:buf.pos;
  buf.bstr <- new_buf;
  buf.len <- new_len

let length t = t.pos

let create n =
  let n = max 1 n in
  let bstr = Bigstringaf.create n in
  { bstr; pos = 0; len = n; init = bstr }

let contents buf = Bigstringaf.substring buf.bstr ~off:0 ~len:buf.pos

let volatile_contents buf = buf.bstr

let add_char buf c =
  let pos = buf.pos in
  if pos >= buf.len then resize buf 1;
  buf.bstr.{pos} <- c;
  buf.pos <- pos + 1

let nth buf pos = if pos < 0 || pos >= buf.pos then invalid_arg "Bigbuffer.nth" else buf.bstr.{pos}

let clear buf = buf.pos <- 0

let reset buf =
  buf.pos <- 0;
  buf.bstr <- buf.init;
  buf.len <- Bigstringaf.length buf.bstr

let add_substring buf src ~pos:src_pos ~len =
  if src_pos < 0 || len < 0 || src_pos > String.length src - len
  then invalid_arg "Bigbuffer.add_substring";
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.blit_from_string src ~src_off:src_pos buf.bstr ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let add_subbytes buf src ~pos:src_pos ~len =
  if src_pos < 0 || len < 0 || src_pos > Bytes.length src - len then invalid_arg "Bigbuffer.add_subbytes";
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.blit_from_bytes src ~src_off:src_pos buf.bstr ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let add_bigstring buf src =
  let len = Bigstringaf.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.blit src ~src_off:0 buf.bstr ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let add_string buf src =
  let len = String.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.blit_from_string src ~src_off:0 buf.bstr ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let add_bytes buf src =
  let len = Bytes.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstringaf.blit_from_bytes src ~src_off:0 buf.bstr ~dst_off:buf.pos ~len;
  buf.pos <- new_pos

let add_buffer buf_dst buf_src =
  let len = buf_src.pos in
  let dst_pos = buf_dst.pos in
  let new_pos = dst_pos + len in
  if new_pos > buf_dst.len then resize buf_dst len;
  Bigstringaf.blit buf_src.bstr ~src_off:0 buf_dst.bstr ~dst_off:dst_pos ~len;
  buf_dst.pos <- new_pos
