open! Core
open Angstrom

let ( .*{} ) bs pos = Bigstring.unsafe_get_uint8 bs ~pos

let return_true = return true

let return_false = return false

let return_none = return None

let return_unit = return ()

let fail_pattern = fail "pattern"

let fail_backtrack = fail "backtrack"

(** More efficient than [Angstrom.skip_many]. See https://github.com/inhabitedtype/angstrom/pull/219 *)
let skip_many p =
  fix (fun m ->
    p >>= (fun _ -> return_true) <|> return_false >>= function
    | true -> m
    | false -> return_unit )

let skip_find_backtrack p =
  fix (fun m ->
    let found = ref None in
    p
    >>= (function
          | None -> return_true
          | Some _ as x ->
            found := x;
            fail_backtrack)
    <|> return_false
    >>= function
    | true -> m
    | false -> return !found )

let skip_find p =
  fix (fun m ->
    let found = ref None in
    p
    >>= (function
          | None -> return_true
          | Some _ as x ->
            found := x;
            return_false)
    <|> return_false
    >>= function
    | true -> m
    | false -> return !found )

let skip_n_times n p =
  if n <= 0
  then return_unit
  else
    fix (fun m ->
      let count = ref 0 in
      let* _ = p in
      incr count;
      if !count < n then m else return_unit )

let peek_with len f =
  Unsafe.peek len f <|> (advance len *> fail_backtrack <|> return_unit) *> Unsafe.peek len f

let equal_sub buf1 ~pos1 buf2 ~pos2 ~len =
  let i = ref 0 in
  while !i < len && buf1.*{pos1 + !i} = buf2.*{pos2 + !i} do
    incr i
  done;
  !i = len

let string s =
  let len = String.length s in
  let bs = Bigstringaf.of_string s ~off:0 ~len in
  Unsafe.take len (fun buf ~off ~len ->
    if equal_sub buf ~pos1:off bs ~pos2:0 ~len then return_unit else fail_pattern )
  >>= Fn.id

let bigstring bs =
  Unsafe.take (Bigstring.length bs) (fun buf ~off ~len ->
    if equal_sub buf ~pos1:off bs ~pos2:0 ~len then return_unit else fail_pattern )
  >>= Fn.id

let drop p = p *> return_unit

(** Fastest benchmarked way to do this. Inspired by [Cstruct.ffind_sub] *)
let sub_index ~pattern ~patlen bs ~off ~len =
  if patlen > len
  then None
  else (
    let max_zidx_sub = patlen - 1 in
    let max_zidx_s = off + len - patlen in
    let rec loop i k =
      if i > max_zidx_s
      then None
      else if k > max_zidx_sub
      then Some (i - off)
      else if k > 0
      then if pattern.*{k} = bs.*{i + k} then loop i (k + 1) else loop (i + 1) 0
      else if pattern.*{0} = bs.*{i}
      then loop i 1
      else loop (i + 1) 0
    in
    loop off 0 )

module Storage = struct
  type t = {
    add: Bigstring.t -> off:int -> len:int -> unit;
    finalize: unit -> unit;
    commit: unit Angstrom.t;
  }

  let noop_backtrack =
    { add = (fun _ ~off:_ ~len:_ -> ()); finalize = (fun () -> ()); commit = return_unit }

  let noop = { add = (fun _ ~off:_ ~len:_ -> ()); finalize = (fun () -> ()); commit }
end

(* Boyer–Moore–Horspool algorithm *)
module BMH = struct
  type t =
    | Found
    | Shift of int
    | Restart

  let make_table ~pattern ~patlen =
    let table = Array.create ~len:256 Restart in
    for i = 0 to patlen - 2 do
      table.(pattern.*{i}) <- Shift (patlen - (i + 1))
    done;
    table

  let run ~pattern ~patlen table bs =
    let rec loop = function
      | -1 -> Found
      | i when bs.*{i} = pattern.*{i} -> (loop [@tailcall]) (i - 1)
      | _ -> table.(bs.*{patlen - 1})
    in
    loop (patlen - 1)
end

let bounded_file_reader ~slice_size ~pattern Storage.{ add; finalize; commit } =
  let patlen = String.length pattern in
  let pattern = Bigstringaf.of_string pattern ~off:0 ~len:patlen in
  let rec slow_path table window =
    match BMH.run ~pattern ~patlen table window with
    | Found ->
      finalize ();
      commit
    | Shift by ->
      add ~off:0 ~len:by window;
      let diff = patlen - by in
      Bigstringaf.unsafe_blit window ~src_off:by window ~dst_off:0 ~len:diff;
      let* () =
        Unsafe.take by (fun buf ~off ~len ->
          Bigstringaf.unsafe_blit buf ~src_off:off window ~dst_off:diff ~len )
      in
      (slow_path [@tailcall]) table window
    | Restart ->
      add ~off:0 ~len:patlen window;
      let* () =
        Unsafe.take patlen (fun buf ~off ~len ->
          Bigstringaf.unsafe_blit buf ~src_off:off window ~dst_off:0 ~len )
      in
      (slow_path [@tailcall]) table window
  in

  let fast_path =
    peek_with (slice_size + patlen) (sub_index ~pattern ~patlen) >>= function
    | Some _ as x -> return x
    | None -> Unsafe.take slice_size add *> commit *> return_none
  in

  skip_find_backtrack fast_path >>= function
  | None ->
    (* This branch is hit when the fast path fails due to the remaining
       length being less than the slice_size. That's why the slow_path
       cannot be using a large Unsafe.peek/take: we don't know how much
       is left. *)
    take_bigstring patlen >>= slow_path (BMH.make_table ~pattern ~patlen)
  | Some at ->
    ((if at > 0 then Unsafe.take at add else return_unit) >>| finalize) *> commit *> bigstring pattern

let take_until_pattern ~slice_size ~pattern =
  let storage buf partial staged =
    Storage.
      {
        add =
          (fun bs ~off ~len ->
            if !staged + len > slice_size
            then (
              Buffer.add_subbytes buf partial ~pos:0 ~len:!staged;
              staged := 0 );
            Bigstringaf.unsafe_blit_to_bytes bs ~src_off:off partial ~dst_off:!staged ~len;
            staged := !staged + len);
        finalize = (fun () -> if !staged > 0 then Buffer.add_subbytes buf partial ~pos:0 ~len:!staged);
        commit;
      }
  in
  let* () = return_unit in
  let buf = Buffer.create 32 in
  let partial = Bytes.create slice_size in
  let staged = ref 0 in
  let+ () = bounded_file_reader ~slice_size ~pattern (storage buf partial staged) in
  Buffer.contents buf
