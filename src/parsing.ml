open! Core
open Angstrom

module Storage = struct
  type t = {
    add: Bigstring.t -> len:int -> unit Angstrom.t;
    finalize: unit -> unit Angstrom.t;
  }

  let dummy = { add = (fun _ ~len:_ -> return ()); finalize = (fun () -> return ()) }
end

let slice_bits = 10

let slice_size = Int.(2 ** slice_bits)

(* Boyer–Moore–Horspool algorithm *)
module BMH = struct
  type t =
    | Found
    | Shift of int
    | Restart

  let make_table ~pattern ~patlen =
    let table = Array.create ~len:256 Restart in
    for i = 0 to patlen - 2 do
      table.(Bigstring.unsafe_get_uint8 pattern ~pos:i) <- Shift (patlen - (i + 1))
    done;
    table

  let bmh_exact ~pattern ~patlen table bs =
    let rec loop = function
      | -1 -> Found
      | i when Char.( = ) bs.{i} pattern.{i} -> (loop [@tailcall]) (i - 1)
      | _ -> table.(Char.to_int bs.{patlen - 1})
    in
    loop (patlen - 1)

  let bmh_suffix ~pattern ~patlen:plen table bs =
    let wlen = Bigstring.length bs in
    let rec loop = function
      | off when Char.( = ) bs.{wlen - off} pattern.{plen - off} -> (loop [@tailcall]) (off + 1)
      | _ -> table.(Char.to_int bs.{wlen - 1})
    in
    loop 1

  let bmh_index ~pattern ~patlen table bs ~pos =
    let rec loop = function
      | -1 -> Found
      | i when Char.( = ) bs.{pos + i} pattern.{i} -> (loop [@tailcall]) (i - 1)
      | _ -> table.(Char.to_int bs.{pos + patlen - 1})
    in
    loop (patlen - 1)

  let is_sub ~pattern ~patlen table bs =
    let maxlen = Bigstring.length bs in
    let rec loop i =
      if i + patlen > maxlen
      then false
      else (
        match bmh_index ~pattern ~patlen table bs ~pos:i with
        | Found -> true
        | Shift by
          when i + patlen + by < maxlen
               && Bigstring.memcmp bs ~pos1:(i + patlen - (patlen - by)) pattern ~pos2:0 ~len:patlen = 0
          ->
          true
        | Shift _
         |Restart ->
          loop (i + patlen) )
    in
    loop 0

  let%expect_test "test name" =
    let bos = Bigstring.of_string in
    let test pattern s =
      let patlen = String.length pattern in
      let pattern = bos pattern in
      is_sub ~pattern ~patlen (make_table ~pattern ~patlen) (bos s) |> Bool.to_string |> print_endline
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
    [%expect {| false |}]
end

let bounded_file_reader ~pattern:spattern Storage.{ add; finalize } =
  let patlen = String.length spattern in
  let pattern = Bigstring.of_string spattern in
  let partial = Bigstring.create patlen in
  let table = BMH.make_table ~pattern ~patlen in
  let rec slow_path window =
    match BMH.bmh_exact ~pattern ~patlen table window with
    | Found -> finalize ()
    | Shift by ->
      let* more = take by in
      let* () = add ~len:by window in
      let diff = patlen - by in
      Bigstring.unsafe_blit ~src:window ~src_pos:by ~dst:partial ~dst_pos:0 ~len:diff;
      Bigstring.From_string.unsafe_blit ~src:more ~src_pos:0 ~dst:partial ~dst_pos:diff ~len:by;
      (slow_path [@tailcall]) partial
    | Restart ->
      let* more = take_bigstring patlen in
      let* () = add ~len:patlen window in
      (slow_path [@tailcall]) more
  in

  let do_fast_path = function
    | window when BMH.is_sub ~pattern ~patlen table window -> fail "Switch to slow path (1)"
    | window -> (
      match BMH.bmh_suffix ~pattern ~patlen table window with
      | Found -> fail "Impossible case: bounded_file_reader do_fast_path found"
      | Shift by -> (
        let* more = peek_string by in
        match String.is_suffix spattern ~suffix:more with
        | true ->
          let* () = advance by in
          (* We're done *)
          let+ () = add ~len:(slice_size - by) window in
          false
        | false ->
          let+ () = add ~len:slice_size window in
          true )
      | Restart ->
        let+ () = add ~len:slice_size window in
        true )
  in

  let fast_path acc =
    match !acc with
    | false -> fail "Switch to slow path (2)"
    | true ->
      let* window = take_bigstring slice_size in
      let+ next = do_fast_path window in
      acc := next
  in

  let* () = return () in
  let acc = ref true in
  let* () = skip_many (fast_path acc) in
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
