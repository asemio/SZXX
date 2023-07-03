open! Core

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
    | Fold_string of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstring.t -> 'a -> 'a;
      }
    | Parse of 'a Angstrom.t
    | Parse_many of {
        parser: 'a Angstrom.t;
        on_parse: 'a -> unit;
      }
end

module Data = struct
  type 'a parser_state =
    | Success of 'a
    | Failed of {
        error: string;
        unconsumed: string;
      }
    | Terminated_early of { unconsumed: string }
    | Incomplete

  let parser_state_to_result = function
  | Success x -> Ok x
  | Failed { error; unconsumed = _ } -> Error (sprintf "Parsing error: %s" error)
  | Terminated_early { unconsumed = _ } -> Error "Could not parse all input"
  | Incomplete -> Error "File incomplete, encountered EOF too early"

  type 'a t =
    | Skip
    | String of string
    | Fold_string of 'a
    | Fold_bigstring of 'a
    | Parse of 'a parser_state
    | Parse_many of unit parser_state
end

let slice_bits = 13

let slice_size = Int.(2 ** slice_bits)

module Storage = struct
  type t = Parsing.Storage.t = {
    add: Bigstring.t -> len:int -> unit Angstrom.t;
    finalize: unit -> unit Angstrom.t;
  }

  let deflated flush =
    let finished = ref true in
    let w = De.make_window ~bits:slice_bits in
    let outbs = De.bigstring_create slice_size in
    let decoder = De.Inf.decoder `Manual ~o:outbs ~w in
    let rec do_uncompress () =
      match De.Inf.decode decoder with
      | `Await -> false
      | `End ->
        let len = slice_size - De.Inf.dst_rem decoder in
        if len > 0 then flush outbs ~len;
        true
      | `Flush ->
        let len = slice_size - De.Inf.dst_rem decoder in
        flush outbs ~len;
        De.Inf.flush decoder;
        do_uncompress ()
      | `Malformed err -> failwith err
    in
    let uncompress bs len =
      De.Inf.src decoder bs 0 len;
      do_uncompress ()
    in
    let add bs ~len =
      finished := uncompress bs len;
      Angstrom.commit
    in
    let finalize () =
      if not !finished then ignore (uncompress outbs 0 : bool);
      Angstrom.commit
    in
    { add; finalize }

  let stored flush =
    let add bs ~len =
      flush bs ~len;
      Angstrom.commit
    in
    let finalize () = Angstrom.commit in
    { add; finalize }
end

module Mode = struct
  type 'a t = {
    flush: Bigstring.t -> len:int -> unit;
    complete: unit -> 'a Data.t * int * Optint.t;
  }

  let skip () =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let flush bs ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc
    in
    let complete () = Data.Skip, !bytes_processed, !crc in
    { flush; complete }

  let string ~buffer_size =
    let res = Bigbuffer.create buffer_size in
    let flush bs ~len = Bigbuffer.add_bigstring res (Bigstringaf.sub bs ~off:0 ~len) in
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
    let flush bs ~len =
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
    let flush bs ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc;
      acc := f entry (Bigstringaf.sub bs ~off:0 ~len) !acc
    in
    let complete () = Data.Fold_bigstring !acc, !bytes_processed, !crc in
    { flush; complete }

  let handle_parse_result state : 'a Data.parser_state =
    let final =
      match state with
      | Angstrom.Buffered.Partial feed -> feed `Eof
      | x -> x
    in
    let unconsumed Angstrom.Buffered.{ buf; off; len } = Bigstringaf.substring buf ~off ~len in
    match final with
    | Done ({ len = 0; _ }, x) -> Success x
    | Done (u, _) -> Terminated_early { unconsumed = unconsumed u }
    | Fail (u, ll, msg) ->
      Failed { error = sprintf "%s: %s" (String.concat ~sep:" > " ll) msg; unconsumed = unconsumed u }
    | Partial _ -> Incomplete

  let parse parser =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let open Angstrom.Buffered in
    let state = ref (parse parser) in
    let flush bs ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc;
      match !state with
      | Done ({ len = 0; _ }, x) -> state := Done ({ buf = Bigstring.copy bs; len; off = 0 }, x)
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = Bigstring.copy bs; len; off = 0 }, x, y)
      | Done _
       |Fail _ ->
        ()
      | Partial feed -> state := feed (`Bigstring (Bigstringaf.sub bs ~off:0 ~len))
    in
    let complete () = Data.Parse (handle_parse_result !state), !bytes_processed, !crc in
    { flush; complete }

  let parse_many parser on_parse =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let open Angstrom.Buffered in
    let async_many p f = Parsing.skip_many Angstrom.(p <* commit >>| f) in
    let wait = ref () in
    let k x = wait := on_parse x in
    let pushback () = !wait in
    let state = ref (parse (async_many parser k)) in

    let flush bs ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc;
      match !state with
      | Done ({ len = 0; _ }, ()) -> state := Done ({ buf = Bigstring.copy bs; len; off = 0 }, ())
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = Bigstring.copy bs; len; off = 0 }, x, y)
      | Done _
       |Fail _ ->
        ()
      | Partial feed ->
        let next = feed (`Bigstring (Bigstring.sub_shared bs ~pos:0 ~len)) in
        pushback ();
        state := next
    in
    let complete () = Data.Parse_many (handle_parse_result !state), !bytes_processed, !crc in
    { flush; complete }
end

let fixed_size_reader size Storage.{ add; finalize } =
  let open Angstrom in
  let acc = ref size in
  let read_slice =
    let* () = return () in
    if !acc < slice_size
    then fail "Done"
    else
      let* bs = take_bigstring slice_size in
      acc := !acc - slice_size;
      let* () = add bs ~len:slice_size in
      commit
  in
  let* () = Parsing.skip_many read_slice in
  let* () =
    match !acc with
    | 0 -> return ()
    | len ->
      let* bs = take_bigstring len in
      add bs ~len
  in
  let* () = finalize () in
  commit

let ( << ) = Int64.( lsl )

let ( ||| ) = Int64.( lor )

let ( .*[] ) s i = s.[i] |> Char.to_int |> Int64.of_int_exn

let parse_le_uint64 ?(offset = 0) s =
  (1, s.*[offset]) |> Fn.apply_n_times ~n:7 (fun (i, x) -> i + 1, s.*[i + offset] << i * 8 ||| x) |> snd

let parse_one cb =
  let open Angstrom in
  let local_file_header_signature = string "PK\003\004" *> return () in
  let descriptor_parser =
    lift3
      (fun crc compressed_size uncompressed_size -> { crc; compressed_size; uncompressed_size })
      LE.any_int32 (LE.any_int32 >>| Int32.to_int64) (LE.any_int32 >>| Int32.to_int64)
  in
  let rec extra_fields_parser acc = function
    | 0 -> return acc
    | left when left < 0 ->
      failwith "SZXX: Corrupted file. Mismatch between reported and actual extra fields size"
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
        if flags land 0x001 <> 0 then failwith "SZXX: Encrypted ZIP entries not supported";
        let methd =
          match methd with
          | 0 -> Stored
          | 8 -> Deflated
          | x -> failwithf "SZXX: Unsupported ZIP compression method %d" x ()
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
        failwithf "SZXX: Corrupted file. Expected 16 bytes for ZIP64 extra field length but found %d" size
          ()
      | _ -> None )
  in
  let entry_parser =
    let* () = Parsing.bounded_file_reader ~pattern:"PK\003\004" Parsing.Storage.noop in
    let* version_needed =
      LE.any_uint16 >>| function
      | 20 -> Zip_2_0
      | 45 -> Zip_4_5
      | x -> failwithf "SZXX: Unsupported ZIP version: %d. Please report this bug." x ()
    in
    let+ flags, methd =
      flags_methd_parser
      <* LE.any_uint16 (* last modified time *)
      <* LE.any_uint16 (* last modified date *)
    and+ descriptor = descriptor_parser
    and+ filename, extra_fields = dynamic_len_fields_parser in
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
        ( flags land 0x008 <> 0
        || Int64.(descriptor.compressed_size = 0L)
        || Int64.(descriptor.uncompressed_size = 0L) );
      methd;
      descriptor;
      filename;
      extra_fields;
    }
  in
  let* entry = entry_parser <* commit in
  let reader ~buffer_size () =
    let Mode.{ flush; complete } =
      match cb entry with
      | Action.Skip -> Mode.skip ()
      | Action.String -> Mode.string ~buffer_size
      | Action.Fold_string { init; f } -> Mode.fold_string ~init ~f entry
      | Action.Fold_bigstring { init; f } -> Mode.fold_bigstring ~init ~f entry
      | Action.Parse parser -> Mode.parse parser
      | Action.Parse_many { parser; on_parse } -> Mode.parse_many parser on_parse
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
  let+ (data, size, crc), entry =
    match entry.trailing_descriptor_present with
    | false ->
      let+ data_size_crc = reader ~buffer_size:(Int64.to_int_exn entry.descriptor.uncompressed_size) () in
      data_size_crc, entry
    | true ->
      let+ data_size_crc = reader ~buffer_size:slice_size ()
      and+ () = option () local_file_header_signature
      and+ descriptor = descriptor_parser in
      data_size_crc, { entry with descriptor }
  in
  let valid_length = Int64.(entry.descriptor.uncompressed_size = of_int size) in
  let valid_crc = Int32.(entry.descriptor.crc = Optint.to_int32 crc) in
  match valid_length, valid_crc with
  | false, false ->
    failwithf "SZXX: File '%s': Size and CRC mismatch: %d bytes but expected %Ld bytes" entry.filename
      size entry.descriptor.uncompressed_size ()
  | false, true ->
    failwithf "SZXX: File '%s': Size mismatch: %d bytes but expected %Ld bytes" entry.filename size
      entry.descriptor.uncompressed_size ()
  | true, false -> failwithf "SZXX: File '%s': CRC mismatch" entry.filename ()
  | true, true -> entry, data

let parse_trailing_central_directory =
  let open Angstrom in
  let* () = Parsing.bounded_file_reader ~pattern:"PK\005\006" Parsing.Storage.noop in
  take 18 *> end_of_input

type feed =
  | String of (unit -> string option)
  | Bigstring of (unit -> Bigstring.t option)

let stream_files ~sw ~feed:read ?dispatcher cb =
  let open Eio.Std in
  let read =
    match read with
    | String f -> (fun () -> f () |> Option.map ~f:(fun s -> `String s))
    | Bigstring f -> (fun () -> f () |> Option.map ~f:(fun buf -> `Bigstring buf))
  in
  let stream = Eio.Stream.create 0 in
  let parser =
    let open Angstrom in
    Parsing.skip_many (parse_one cb >>| fun pair -> Eio.Stream.add stream (Some pair))
    *> parse_trailing_central_directory
  in
  let open Angstrom.Buffered in
  let rec loop = function
    | Fail _ as state -> state_to_result state |> Result.ok_or_failwith
    | Done (_, ()) -> failwith "SZXX: ZIP processing completed before reaching end of input"
    | Partial feed -> (
      match read () with
      | None -> (
        match feed `Eof with
        | Fail _ as state -> state_to_result state |> Result.ok_or_failwith
        | _ -> () )
      | Some chunk -> (loop [@tailcall]) (feed chunk) )
  in
  Fiber.fork_daemon ~sw (fun () ->
    let do_work () =
      loop (parse parser);
      Eio.Stream.add stream None
    in
    (match dispatcher with
    | None -> do_work ()
    | Some d -> Dispatcher.run_exn d ~f:do_work);
    `Stop_daemon );
  stream
