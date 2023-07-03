open! Core
open Angstrom

(** More efficient than [Angstrom.skip_many]. See https://github.com/inhabitedtype/angstrom/pull/219 *)
let skip_many p =
  fix (fun m ->
    p >>| (fun _ -> true) <|> return false >>= function
    | true -> m
    | false -> return () )

module Storage = struct
  type t = {
    add: Bigstring.t -> len:int -> unit Angstrom.t;
    finalize: unit -> unit Angstrom.t;
  }

  let noop = { add = (fun _ ~len:_ -> return ()); finalize = (fun () -> return ()) }
end

(* let slice_size = Int.(2 ** 10) *)

let slice_size = 2000

let ( .*{} ) bs pos = Bigstring.unsafe_get_uint8 bs ~pos

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

  let bmh_exact ~pattern ~patlen table bs =
    let rec loop = function
      | -1 -> Found
      | i when bs.*{i} = pattern.*{i} -> (loop [@tailcall]) (i - 1)
      | _ -> table.(bs.*{patlen - 1})
    in
    loop (patlen - 1)

  let bmh_index ~pattern ~patlen table bs ~pos =
    let rec loop = function
      | -1 -> Found
      | i when bs.*{pos + i} = pattern.*{i} -> (loop [@tailcall]) (i - 1)
      | _ -> table.(bs.*{pos + patlen - 1})
    in
    loop (patlen - 1)
end

(** [~pos2] is always 0 *)
let equal_sub buf1 ~pos1 buf2 ~len =
  let rec aux acc = function
    | -1 -> acc
    | i -> (aux [@tailcall]) (acc && buf1.*{pos1 + i} = buf2.*{i}) (i - 1)
  in
  aux true (len - 1)
  [@@hot]

(** Fastest benchmarked way to do this. Inspired by [Cstruct.is_infix] *)
let is_sub ~pattern ~patlen bs =
  let len = Bigstring.length bs in
  if patlen > len
  then false
  else (
    let max_zidx_a = patlen - 1 in
    let max_zidx_s = len - patlen in
    let rec loop i k =
      if i > max_zidx_s
      then false
      else if k > max_zidx_a
      then true
      else if k > 0
      then if pattern.*{k} = bs.*{i + k} then loop i (succ k) else loop (succ i) 0
      else if pattern.*{0} = bs.*{i}
      then loop i 1
      else loop (succ i) 0
    in
    loop 0 0 )

let%expect_test "is_sub" =
  let bos = Bigstring.of_string in
  let test pattern s =
    let patlen = String.length pattern in
    let pattern = bos pattern in
    is_sub ~pattern ~patlen (bos s) |> Bool.to_string |> print_endline
  in
  test "abc" "def";
  [%expect {| false |}];
  test "abc" "abcdef";
  [%expect {| true |}];
  test "abc" "defabc";
  [%expect {| true |}];
  test "abc" "defasabdillceabbabbcabczz";
  [%expect {| true |}];
  test "abc" "defasabdillceabbabbccbczz";
  [%expect {| false |}];
  test "]]>" "a]]>";
  [%expect {| true |}];
  test "]]>" "ab]]>";
  [%expect {| true |}];
  test "]]>" "abc]]>";
  [%expect {| true |}]

let bounded_file_reader ~pattern Storage.{ add; finalize } =
  let pattern = Bigstring.of_string pattern in
  (* let cpattern = Cstruct.of_bigarray pattern in *)
  let patlen = Bigstring.length pattern in
  let table = BMH.make_table ~pattern ~patlen in
  let rec slow_path window =
    match BMH.bmh_exact ~pattern ~patlen table window with
    | Found -> finalize ()
    | Shift by ->
      let* more = take by in
      let* () = add ~len:by window in
      let diff = patlen - by in
      Bigstring.unsafe_blit ~src:window ~src_pos:by ~dst:window ~dst_pos:0 ~len:diff;
      Bigstring.From_string.unsafe_blit ~src:more ~src_pos:0 ~dst:window ~dst_pos:diff ~len:by;
      (slow_path [@tailcall]) window
    | Restart ->
      let* more = take_bigstring patlen in
      let* () = add ~len:patlen window in
      (slow_path [@tailcall]) more
  in

  let do_fast_path = function
    | window when is_sub ~pattern ~patlen window -> fail "Switch to slow path (1)"
    | window -> (
      match BMH.bmh_index ~pattern ~patlen table window ~pos:(slice_size - patlen) with
      | Found -> fail "Impossible case: bounded_file_reader do_fast_path found"
      | Shift by when equal_sub window ~pos1:(slice_size - (patlen - by)) pattern ~len:(patlen - by) -> (
        let* more = take_bigstring by in
        match equal_sub pattern ~pos1:(patlen - by) more ~len:by with
        | true -> fail "Switch to slow path (2)"
        | false ->
          let* () = add ~len:slice_size window in
          add ~len:by more )
      | Shift _
       |Restart ->
        add ~len:slice_size window )
  in

  let fast_path = take_bigstring slice_size >>= do_fast_path in

  let* () = skip_many fast_path in
  (* let* xx = pos in
     Eio.Std.traceln !"ON SLOW PATH after: %d (%d)" !nfast xx; *)
  take_bigstring patlen >>= slow_path

let take_until_pattern ~pattern =
  let storage buf partial staged =
    Storage.
      {
        add =
          (fun bs ~len ->
            if !staged + len > slice_size
            then (
              Buffer.add_subbytes buf partial ~pos:0 ~len:!staged;
              staged := 0 );
            Bigstring.To_bytes.unsafe_blit ~src:bs ~src_pos:0 ~dst:partial ~dst_pos:!staged ~len;
            staged := !staged + len;
            return ());
        finalize =
          (fun () ->
            if !staged > 0 then Buffer.add_subbytes buf partial ~pos:0 ~len:!staged;
            return ());
      }
  in
  let* () = return () in
  let buf = Buffer.create 32 in
  let partial = Bytes.create slice_size in
  let staged = ref 0 in
  let+ () = bounded_file_reader ~pattern (storage buf partial staged) in
  Buffer.contents buf
