open! Core
open Lwt.Syntax
open Lwt.Infix

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of]

type version =
  | Zip_2_0
  | Zip_4_5
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: Int64.t;
  uncompressed_size: Int64.t;
}
[@@deriving sexp_of]

type extra_field = {
  id: int;
  size: int;
  data: string;
}
[@@deriving sexp_of]

type entry = {
  version_needed: version;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra_fields: extra_field list;
}
[@@deriving sexp_of]

module Action = struct
  type 'a t =
    | Skip
    | String
    | Fold_string    of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstring.t -> len:int -> 'a -> 'a;
      }
    | Parse          of 'a Angstrom.t
end

module Data = struct
  type 'a t =
    | Skip
    | String         of string
    | Fold_string    of 'a
    | Fold_bigstring of 'a
    | Parse          of ('a, string) result
end

let slice_size = Parsing.slice_size

let slice_bits = 10

module Storage = struct
  type t = Parsing.storage = {
    add: Bigstring.t -> int -> unit;
    finalize: unit -> unit;
  }

  let deflated flush =
    let finished = ref true in
    let w = De.make_window ~bits:slice_bits in
    let outbs = Bigstring.create slice_size in
    let decoder = De.Inf.decoder `Manual ~o:outbs ~w in
    let rec do_uncompress () =
      match De.Inf.decode decoder with
      | `Await -> false
      | `End ->
        let len = slice_size - De.Inf.dst_rem decoder in
        if len > 0 then flush outbs len;
        true
      | `Flush ->
        let len = slice_size - De.Inf.dst_rem decoder in
        flush outbs len;
        De.Inf.flush decoder;
        do_uncompress ()
      | `Malformed err -> failwith err
    in
    let uncompress bs len =
      De.Inf.src decoder bs 0 len;
      do_uncompress ()
    in
    let add bs len = finished := uncompress bs len in
    let finalize () = if not !finished then ignore (uncompress outbs 0 : bool) in
    { add; finalize }

  let stored flush =
    let add = flush in
    let finalize () = () in
    { add; finalize }
end

module Mode = struct
  type 'a t = {
    flush: Bigstring.t -> int -> unit;
    complete: unit -> 'a Data.t * int * Optint.t;
  }

  let skip () =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let flush bs len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc
    in
    let complete () = Data.Skip, !bytes_processed, !crc in
    { flush; complete }

  let string ~buffer_size =
    let res = Bigbuffer.create buffer_size in
    let flush bs len = Bigbuffer.add_bigstring res (Bigstring.sub_shared bs ~pos:0 ~len) in
    let complete () =
      let str = Bigbuffer.contents res in
      let len = String.length str in
      Data.String str, len, Checkseum.Crc32.digest_string str 0 len Optint.zero
    in
    { flush; complete }

  let fold_string ~init ~f entry =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let acc = ref init in
    let flush bs len =
      let s = Bigstring.to_string bs ~pos:0 ~len in
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_string s 0 len !crc;
      acc := f entry s !acc
    in
    let complete () = Data.Fold_string !acc, !bytes_processed, !crc in
    { flush; complete }

  let fold_bigstring ~init ~f entry =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let acc = ref init in
    let flush bs len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc;
      acc := f entry bs ~len !acc
    in
    let complete () = Data.Fold_bigstring !acc, !bytes_processed, !crc in
    { flush; complete }

  let parse angstrom =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let open Angstrom.Buffered in
    let state = ref (parse angstrom) in
    let flush bs len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc;
      match !state with
      | Done _
       |Fail _ ->
        ()
      | Partial feed -> state := feed (`Bigstring (Bigstring.sub_shared bs ~pos:0 ~len))
    in
    let complete () =
      let final_state =
        match !state with
        | (Done _ as x)
         |(Fail _ as x) ->
          x
        | Partial feed -> feed `Eof
      in
      Data.Parse (state_to_result final_state), !bytes_processed, !crc
    in
    { flush; complete }
end

let fixed_size_reader size Storage.{ add; finalize } =
  let open Angstrom in
  let rec loop = function
    | n when n > slice_size ->
      take_bigstring slice_size >>= fun bs ->
      add bs slice_size;
      (loop [@tailcall]) (n - slice_size)
    | 0 ->
      finalize ();
      commit
    | n ->
      take_bigstring n <* commit >>= fun bs ->
      add bs n;
      (loop [@tailcall]) 0
  in
  loop size

let ( << ) = Int64.( lsl )

let ( ||* ) = Int64.( lor )

let ( .*[] ) s i = s.[i] |> Char.to_int |> Int64.of_int_exn

let parse_le_uint64 ?(offset = 0) s =
  (1, s.*[offset]) |> Fn.apply_n_times ~n:7 (fun (i, x) -> i + 1, s.*[i + offset] << i * 8 ||* x) |> snd

let parser cb =
  let open Angstrom in
  let local_file_header_signature = string "PK\003\004" in
  let descriptor_parser =
    lift3
      (fun crc compressed_size uncompressed_size -> { crc; compressed_size; uncompressed_size })
      LE.any_int32 (LE.any_int32 >>| Int32.to_int64) (LE.any_int32 >>| Int32.to_int64)
  in
  let rec extra_fields_parser acc = function
    | 0 -> return acc
    | left when left < 0 -> failwith "Mismatch between reported and actual extra fields size"
    | left ->
      lift2 Tuple2.create LE.any_uint16 LE.any_uint16 >>= fun (id, size) ->
      take size >>= fun data ->
      (extra_fields_parser [@tailcall]) ({ id; size; data } :: acc) (left - (size + 4))
  in
  let dynamic_len_fields_parser =
    lift2 Tuple2.create LE.any_uint16 LE.any_uint16 >>= fun (filename_len, extra_fields_len) ->
    lift2 Tuple2.create (take filename_len) (extra_fields_parser [] extra_fields_len)
  in
  let flags_methd_parser =
    lift2
      (fun flags methd ->
        if flags land 0x001 <> 0 then failwith "Encrypted entries not supported";
        let methd =
          match methd with
          | 0 -> Stored
          | 8 -> Deflated
          | x -> failwithf "Unsupported compression method %d" x ()
        in
        flags, methd)
      LE.any_uint16 LE.any_uint16
  in
  let get_zip64_descriptor ~crc extra_fields =
    List.find_map extra_fields ~f:(function
      | { id = 1; size = 16; data } ->
        Some
          {
            crc;
            uncompressed_size = parse_le_uint64 data;
            compressed_size = parse_le_uint64 ~offset:8 data;
          }
      | { id = 1; size; _ } ->
        failwithf "Expected 16 bytes for ZIP64 extra field length but found %d" size ()
      | _ -> None)
  in
  let entry_parser =
    Parsing.skip_until_pattern ~pattern:"PK\003\004"
    *> (LE.any_uint16 >>| function
        | 20 -> Zip_2_0
        | 45 -> Zip_4_5
        | x -> failwithf "Unsupported version: %d. Please report this bug." x ())
    >>= fun version_needed ->
    lift3
      (fun (flags, methd) descriptor (filename, extra_fields) ->
        let descriptor =
          match version_needed with
          | Zip_2_0 -> descriptor
          | Zip_4_5 ->
            Option.value_exn ~message:"Missing ZIP64 extra field"
              (get_zip64_descriptor ~crc:descriptor.crc extra_fields)
        in
        {
          version_needed;
          flags;
          trailing_descriptor_present =
            (flags land 0x008 <> 0
            || Int64.(descriptor.compressed_size = 0L)
            || Int64.(descriptor.uncompressed_size = 0L));
          methd;
          descriptor;
          filename;
          extra_fields;
        })
      (flags_methd_parser
      <* LE.any_uint16 (* last modified time *)
      <* LE.any_uint16 (* last modified date *))
      descriptor_parser dynamic_len_fields_parser
  in
  lift2 const entry_parser commit >>= fun entry ->
  let reader ~buffer_size () =
    let Mode.{ flush; complete } =
      match cb entry with
      | Action.Skip -> Mode.skip ()
      | Action.String -> Mode.string ~buffer_size
      | Action.Fold_string { init; f } -> Mode.fold_string ~init ~f entry
      | Action.Fold_bigstring { init; f } -> Mode.fold_bigstring ~init ~f entry
      | Action.Parse angstrom -> Mode.parse angstrom
    in
    let storage_method, zipped_length =
      match entry.methd with
      | Stored -> Storage.stored flush, entry.descriptor.uncompressed_size
      | Deflated -> Storage.deflated flush, entry.descriptor.compressed_size
    in
    let file_reader =
      if Int64.(entry.descriptor.compressed_size = 0L) || Int64.(entry.descriptor.uncompressed_size = 0L)
      then Parsing.bounded_file_reader ~pattern:"PK\007\008"
      else fixed_size_reader (Int64.to_int_exn zipped_length)
    in
    file_reader storage_method >>| complete
  in
  let file =
    match entry.trailing_descriptor_present with
    | false ->
      reader ~buffer_size:(Int64.to_int_exn entry.descriptor.uncompressed_size) () >>| fun x -> x, entry
    | true ->
      lift3
        (fun data_size_crc _header descriptor -> data_size_crc, { entry with descriptor })
        (reader ~buffer_size:(slice_size * 4) ())
        (option "" local_file_header_signature)
        descriptor_parser
  in
  file >>| fun data ->
  match data with
  | (_data, size, _crc), entry when Int64.(entry.descriptor.uncompressed_size <> of_int size) ->
    failwithf "%s: Size mismatch: %d but expected %Ld" entry.filename size
      entry.descriptor.uncompressed_size ()
  | (_data, _size, crc), entry when Int32.(entry.descriptor.crc <> Optint.to_int32 crc) ->
    failwithf "%s: CRC mismatch" entry.filename ()
  | (data, _size, _crc), entry -> entry, data

type 'a slice = {
  buf: 'a;
  pos: int;
  len: int;
}

type feed =
  | String    of (unit -> string option Lwt.t)
  | Bigstring of (unit -> Bigstring.t slice option Lwt.t)

let stream_files ~feed:read cb =
  let read =
    let open Lwt.Infix in
    match read with
    | String f -> (fun () -> f () >|= Option.map ~f:(fun s -> `String s))
    | Bigstring f ->
      fun () ->
        f () >|= Option.map ~f:(fun { buf; pos; len } -> `Bigstring (Bigstring.sub_shared buf ~pos ~len))
  in
  let stream, bounded = Lwt_stream.create_bounded 1 in
  let mutex = Lwt_mutex.create () in
  let open Angstrom.Buffered in
  let rec loop = function
    | Fail (_, [], err) -> failwith err
    | Fail (_, marks, err) -> failwithf "%s: %s" (String.concat ~sep:" > " marks) err ()
    | Done ({ buf; off = pos; len }, pair) -> (
      let* () = Lwt_mutex.with_lock mutex (fun () -> bounded#push pair) in
      match parse (parser cb) with
      | Partial feed -> (loop [@tailcall]) (feed (`Bigstring (Bigstring.sub_shared buf ~pos ~len)))
      | state -> (loop [@tailcall]) state)
    | Partial feed -> (
      read () >>= function
      | None -> (
        match feed `Eof with
        | Done (_, pair) -> Lwt_mutex.with_lock mutex (fun () -> bounded#push pair)
        | _ -> Lwt.return_unit)
      | Some chunk -> (loop [@tailcall]) (feed chunk))
  in
  let p =
    Lwt.finalize
      (fun () -> loop (parse (parser cb)))
      (fun () ->
        bounded#close;
        Lwt.return_unit)
  in
  stream, p
