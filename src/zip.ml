module P = Parsing
open! Base
open Angstrom
open P

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of, compare, equal]

type version =
  | Zip_2_0
  | Zip_4_5
[@@deriving sexp_of, compare, equal]

type descriptor = {
  crc: Int32.t;
  compressed_size: Int64.t;
  uncompressed_size: Int64.t;
  offset: Int64.t option;
}
[@@deriving sexp_of, compare, equal]

type extra_field = {
  id: int;
  size: int;
  data: string;
}
[@@deriving sexp_of, compare, equal]

type entry = {
  version_needed: version;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra_fields: extra_field list;
  comment: string;
}
[@@deriving sexp_of, compare, equal]

module Action = struct
  type 'a t =
    | Skip
    | Fast_skip
    | String
    | Bigstring
    | Fold_string of {
        init: 'a;
        f: entry -> string -> 'a -> 'a;
      }
    | Fold_bigstring of {
        init: 'a;
        f: entry -> Bigstringaf.t -> 'a -> 'a;
      }
    | Parse of 'a Angstrom.t
    | Parse_many of {
        parser: 'a Angstrom.t;
        on_parse: 'a -> unit;
      }
    | Terminate
end

let bs_copy bs = Bigstringaf.copy bs ~off:0 ~len:(Bigstringaf.length bs)

module Data = struct
  type 'a parser_state =
    | Success of 'a
    | Failed of {
        error: string;
        unconsumed: string;
      }
    | Terminated_early of { unconsumed: string }
    | Incomplete
  [@@deriving sexp_of, compare, equal]

  let parser_state_to_result = function
  | Success x -> Ok x
  | Failed { error; unconsumed = _ } -> Error (Printf.sprintf "Parsing error: %s" error)
  | Terminated_early { unconsumed = _ } -> Error "Could not parse all input"
  | Incomplete -> Error "File incomplete, encountered EOF too early"

  type bigstringaf = Bigstringaf.t

  let sexp_of_bigstringaf bs = Sexp.Atom (Bigstringaf.to_string bs)

  let compare_bigstringaf a b =
    let len1 = Bigstringaf.length a in
    let len2 = Bigstringaf.length b in
    match Bigstringaf.unsafe_memcmp a 0 b 0 (min len1 len2) with
    | 0 -> Int.compare len1 len2
    | x -> x

  let equal_bigstringaf a b = compare_bigstringaf a b = 0

  type 'a t =
    | Skip
    | Fast_skip
    | String of string
    | Bigstring of bigstringaf
    | Fold_string of 'a
    | Fold_bigstring of 'a
    | Parse of 'a parser_state
    | Parse_many of unit parser_state
    | Terminate
  [@@deriving sexp_of, compare, equal]
end

let slice_bits = 13

let slice_size = Int.(2 ** slice_bits)

module Storage = struct
  type t = P.Storage.t = {
    add: Bigstringaf.t -> off:int -> len:int -> unit;
    finalize: unit -> unit;
    commit: unit Angstrom.t;
  }

  let deflated flush =
    let finished = ref true in
    let w = De.make_window ~bits:slice_bits in
    let outbs = De.bigstring_create slice_size in
    let decoder = De.Inf.decoder `Manual ~o:outbs ~w in
    let rec do_decompress () =
      match De.Inf.decode decoder with
      | `Await -> false
      | `End ->
        let len = slice_size - De.Inf.dst_rem decoder in
        if len > 0 then flush outbs ~off:0 ~len;
        true
      | `Flush ->
        let len = slice_size - De.Inf.dst_rem decoder in
        flush outbs ~off:0 ~len;
        De.Inf.flush decoder;
        do_decompress ()
      | `Malformed err ->
        Printf.failwithf "SZXX: Corrupted file. Malformed deflate data. Error: %s" err ()
    in
    let decompress bs ~off ~len =
      De.Inf.src decoder bs off len;
      do_decompress ()
    in
    let add bs ~off ~len = finished := decompress bs ~off ~len in
    let finalize () = if not !finished then ignore (decompress outbs ~off:0 ~len:0 : bool) in
    { add; finalize; commit }

  let stored flush =
    let add = flush in
    let finalize () = () in
    { add; finalize; commit }
end

module Mode = struct
  type 'a t = {
    flush: Bigstringaf.t -> off:int -> len:int -> unit;
    complete: unit -> 'a Data.t * int * Optint.t;
  }

  let skip () =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let flush bs ~off ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs off len !crc
    in
    let complete () = Data.Skip, !bytes_processed, !crc in
    { flush; complete }

  let fast_skip () =
    let flush _bs ~off:_ ~len:_ = () in
    let complete () = Data.Fast_skip, 0, Optint.zero in
    { flush; complete }

  let string ~buffer_size =
    let res = Bigbuffer.create buffer_size in
    let flush bs ~off ~len = Bigbuffer.add_bigstring res (Bigstringaf.sub bs ~off ~len) in
    let complete () =
      let len = Bigbuffer.length res in
      let str = Bigbuffer.contents res in
      Data.String str, len, Checkseum.Crc32.digest_string str 0 len Optint.zero
    in
    { flush; complete }

  let bigstring ~buffer_size =
    let res = Bigbuffer.create buffer_size in
    let flush bs ~off ~len = Bigbuffer.add_bigstring res (Bigstringaf.sub bs ~off ~len) in
    let complete () =
      let len = Bigbuffer.length res in
      let bs = Bigbuffer.volatile_contents res in
      Data.Bigstring bs, len, Checkseum.Crc32.digest_bigstring bs 0 len Optint.zero
    in
    { flush; complete }

  let fold_string ~init ~f entry =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let acc = ref init in
    let flush bs ~off ~len =
      let s = Bigstringaf.substring bs ~off ~len in
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_string s off len !crc;
      acc := f entry s !acc
    in
    let complete () = Data.Fold_string !acc, !bytes_processed, !crc in
    { flush; complete }

  let fold_bigstring ~init ~f entry =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let acc = ref init in
    let flush bs ~off ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs off len !crc;
      acc := f entry (Bigstringaf.sub bs ~off ~len) !acc
    in
    let complete () = Data.Fold_bigstring !acc, !bytes_processed, !crc in
    { flush; complete }

  let handle_parse_result state : 'a Data.parser_state =
    let final =
      match state with
      | Buffered.Partial feed -> feed `Eof
      | x -> x
    in
    let unconsumed Buffered.{ buf; off; len } = Bigstringaf.substring buf ~off ~len in
    match final with
    | Done ({ len = 0; _ }, x) -> Success x
    | Done (u, _) -> Terminated_early { unconsumed = unconsumed u }
    | Fail (u, ll, msg) ->
      Failed
        { error = Printf.sprintf "%s: %s" (String.concat ~sep:" > " ll) msg; unconsumed = unconsumed u }
    | Partial _ -> Incomplete

  let parse parser =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let open Buffered in
    let state = ref (parse parser) in
    let flush bs ~off ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs off len !crc;
      match !state with
      | Done ({ len = 0; _ }, x) -> state := Done ({ buf = bs_copy bs; len; off }, x)
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = bs_copy bs; len; off }, x, y)
      | Done _
       |Fail _ ->
        ()
      | Partial feed -> state := feed (`Bigstring (Bigstringaf.sub bs ~off ~len))
    in
    let complete () = Data.Parse (handle_parse_result !state), !bytes_processed, !crc in
    { flush; complete }

  let parse_many parser on_parse =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let open Buffered in
    let async_many p f = skip_many (p <* commit >>| f) in
    let wait = ref () in
    let k x = wait := on_parse x in
    let pushback () = !wait in
    let state = ref (parse (async_many parser k)) in

    let flush bs ~off ~len =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs off len !crc;
      match !state with
      | Done ({ len = 0; _ }, ()) -> state := Done ({ buf = bs_copy bs; len; off }, ())
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = bs_copy bs; len; off }, x, y)
      | Done _
       |Fail _ ->
        ()
      | Partial feed ->
        let next = feed (`Bigstring (Bigstringaf.sub bs ~off ~len)) in
        pushback ();
        state := next
    in
    let complete () = Data.Parse_many (handle_parse_result !state), !bytes_processed, !crc in
    { flush; complete }
end

let fixed_size_reader size Storage.{ add; finalize; commit } =
  let n = size / slice_size in
  let rem = size - (slice_size * n) in
  let read_slice = Unsafe.take slice_size add *> commit in
  skip_n_times n read_slice
  *> ((if rem > 0 then Unsafe.take rem add else return_unit) >>| finalize)
  *> commit

let ( << ) = Int64.( lsl )

let ( ||| ) = Int64.( lor )

let ( .*[] ) s i = s.[i] |> Char.to_int |> Int64.of_int_exn

let parse_le_uint64 ?(offset = 0) s =
  (1, s.*[offset]) |> Fn.apply_n_times ~n:7 (fun (i, x) -> i + 1, s.*[i + offset] << i * 8 ||| x) |> snd

module Parsers = struct
  let version_needed =
    LE.any_uint16 >>| function
    | x when x < 45 -> Zip_2_0
    | _ -> Zip_4_5

  let flags_methd_parser =
    let+ flags = LE.any_uint16
    and+ methd = LE.any_uint16 in
    if flags land 0x001 <> 0 then failwith "SZXX: Encrypted ZIP entries not supported";
    let methd =
      match methd with
      | 0 -> Stored
      | 8 -> Deflated
      | x ->
        Printf.failwithf
          "SZXX: Unsupported ZIP compression method %d. You are welcome to open a feature request." x ()
    in
    flags, methd

  let rec extra_fields_parser acc = function
  | 0 -> return (List.rev acc)
  | left when left < 0 ->
    failwith "SZXX: Corrupted file. Mismatch between reported and actual extra fields size"
  | left ->
    let* id = LE.any_uint16 in
    let* size = LE.any_uint16 in
    let* data = take size in
    (extra_fields_parser [@tailcall]) ({ id; size; data } :: acc) (left - (size + 4))

  let parse_initial_offset =
    let+ offset = LE.any_int32 in
    Option.some_if Int32.(offset <> -1l) (Int32.to_int64 offset)

  type initial_descriptor = {
    crc: Int32.t;
    compressed_size: Int64.t option;
    uncompressed_size: Int64.t option;
  }

  let parse_initial_descriptor =
    let+ crc = LE.any_int32
    and+ c_size = LE.any_int32
    and+ r_size = LE.any_int32 in
    let compressed_size = if Int32.(c_size = -1l) then None else Some (Int32.to_int64 c_size) in
    let uncompressed_size = if Int32.(r_size = -1l) then None else Some (Int32.to_int64 r_size) in
    { crc; compressed_size; uncompressed_size }

  let make_descriptor ~init_offset version extra_fields initial =
    match version with
    | Zip_2_0 -> (
      match initial with
      | { crc; compressed_size = Some compressed_size; uncompressed_size = Some uncompressed_size } ->
        { crc; compressed_size; uncompressed_size; offset = init_offset }
      | _ -> failwith "SZXX: Corrupted file. Invalid file lengths for Zip2.0" )
    | Zip_4_5 -> (
      (* Section 4.5.3 of https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.3.9.TXT *)
      let data =
        match List.find extra_fields ~f:(fun { id; _ } -> id = 1) with
        | None -> ""
        | Some { data; _ } -> data
      in
      let len = String.length data in
      let needed =
        (if Option.is_none initial.compressed_size then 8 else 0)
        + if Option.is_none initial.uncompressed_size then 8 else 0
      in
      let offset =
        match init_offset with
        | Some _ as x -> x
        | None when len >= needed + 8 -> Some (parse_le_uint64 data ~offset:needed)
        | None -> None
      in

      match initial with
      | { compressed_size = Some compressed_size; uncompressed_size = Some uncompressed_size; crc } ->
        { compressed_size; uncompressed_size; crc; offset }
      | { compressed_size = Some compressed_size; uncompressed_size = None; crc } when len >= 8 ->
        { compressed_size; uncompressed_size = parse_le_uint64 data; crc; offset }
      | { compressed_size = None; uncompressed_size = Some uncompressed_size; crc } when len >= 8 ->
        { compressed_size = parse_le_uint64 data; uncompressed_size; crc; offset }
      | { compressed_size = None; uncompressed_size = None; crc } when len >= 16 ->
        {
          uncompressed_size = parse_le_uint64 data;
          compressed_size = parse_le_uint64 ~offset:8 data;
          crc;
          offset;
        }
      | _ ->
        Printf.failwithf "SZXX: Corrupted file. Unexpected length of ZIP64 extra field data: %d" len () )
end

module Magic = struct
  let start_local_header = "PK\003\004"

  let end_bounded_file = "PK\007\008"

  let start_cd_header = "PK\001\002"

  let start_eocd = "PK\005\006"

  let start_eocd64 = "PK\006\006"

  let start_eocd64_locator = "PK\006\007"
end

let parse_entry =
  let parser =
    let+ version_needed = string Magic.start_local_header *> Parsers.version_needed
    and+ flags, methd = Parsers.flags_methd_parser
    and+ initial_descriptor = advance 4 *> Parsers.parse_initial_descriptor
    and+ filename, extra_fields =
      let* filename_len = LE.any_uint16 in
      let* extra_fields_len = LE.any_uint16 in
      let+ filename = take filename_len
      and+ extra_fields = Parsers.extra_fields_parser [] extra_fields_len in
      filename, extra_fields
    in
    let descriptor =
      Parsers.make_descriptor ~init_offset:None version_needed extra_fields initial_descriptor
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
      comment = "";
    }
  in
  parser <* commit

let parse_cd_entry =
  let parser =
    let* version_needed = string Magic.start_cd_header *> advance 2 *> Parsers.version_needed in
    let* flags, methd = Parsers.flags_methd_parser in
    let* initial_descriptor = advance 4 *> Parsers.parse_initial_descriptor in
    let+ descriptor, filename, extra_fields, comment =
      let* filename_len = LE.any_uint16 in
      let* extra_fields_len = LE.any_uint16 in
      let* comment_len = LE.any_uint16 in
      let+ offset = advance 8 *> Parsers.parse_initial_offset
      and+ filename = take filename_len
      and+ extra_fields = Parsers.extra_fields_parser [] extra_fields_len
      and+ comment = take comment_len in
      let descriptor =
        Parsers.make_descriptor ~init_offset:offset version_needed extra_fields initial_descriptor
      in
      descriptor, filename, extra_fields, comment
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
      comment;
    }
  in
  parser <* commit

let parse_one cb =
  let* entry = parse_entry in
  let reader ~buffer_size action =
    let Mode.{ flush; complete } =
      match action with
      | Action.Skip -> Mode.skip ()
      | Action.Fast_skip -> Mode.fast_skip ()
      | Action.String -> Mode.string ~buffer_size
      | Action.Bigstring -> Mode.bigstring ~buffer_size
      | Action.Fold_string { init; f } -> Mode.fold_string ~init ~f entry
      | Action.Fold_bigstring { init; f } -> Mode.fold_bigstring ~init ~f entry
      | Action.Parse parser -> Mode.parse parser
      | Action.Parse_many { parser; on_parse } -> Mode.parse_many parser on_parse
      | Action.Terminate -> assert false
    in
    let storage_method, zipped_length =
      match entry.methd, action with
      | Deflated, Fast_skip -> Storage.stored flush, entry.descriptor.compressed_size
      | Deflated, _ -> Storage.deflated flush, entry.descriptor.compressed_size
      | Stored, _ -> Storage.stored flush, entry.descriptor.uncompressed_size
    in
    let file_reader =
      if Int64.(entry.descriptor.compressed_size = 0L) || Int64.(entry.descriptor.uncompressed_size = 0L)
      then bounded_file_reader ~slice_size:Int.(2 ** 11) ~pattern:Magic.end_bounded_file
      else fixed_size_reader (Int64.to_int_exn zipped_length)
    in
    file_reader storage_method >>| complete
  in
  match cb entry with
  | Action.Terminate -> return (entry, Data.Terminate)
  | action -> (
    let+ (data, size, crc), entry =
      match entry.trailing_descriptor_present with
      | false ->
        let+ data_size_crc =
          reader ~buffer_size:(Int64.to_int_exn entry.descriptor.uncompressed_size) action
        in
        data_size_crc, entry
      | true ->
        let+ data_size_crc = reader ~buffer_size:slice_size action
        and+ initial_descriptor = Parsers.parse_initial_descriptor in
        let descriptor =
          Parsers.make_descriptor ~init_offset:None entry.version_needed entry.extra_fields
            initial_descriptor
        in
        data_size_crc, { entry with descriptor }
    in
    let valid_length = Int64.(entry.descriptor.uncompressed_size = of_int size) in
    let valid_crc = Int32.(entry.descriptor.crc = Optint.to_int32 crc) in
    match data, valid_length, valid_crc with
    | Fast_skip, _, _
     |_, true, true ->
      entry, data
    | _, false, false ->
      Printf.failwithf "SZXX: File '%s': Size and CRC mismatch: %d bytes but expected %Ld bytes"
        entry.filename size entry.descriptor.uncompressed_size ()
    | _, false, true ->
      Printf.failwithf "SZXX: File '%s': Size mismatch: %d bytes but expected %Ld bytes" entry.filename
        size entry.descriptor.uncompressed_size ()
    | _, true, false -> Printf.failwithf "SZXX: File '%s': CRC mismatch" entry.filename () )

let skip_over_cd_and_drain = string Magic.start_cd_header *> skip_while (fun _ -> true) *> end_of_input

let invalid = function
| Ok x -> x
| Error msg -> Printf.failwithf "SZXX: Corrupted file. Invalid ZIP structure. Error: %s" msg ()

let stream_files ~sw ~feed:(read : Feed.t) cb =
  Sequence.of_seq
  @@ Eio.Fiber.fork_seq ~sw
  @@ fun yield ->
  let parser =
    skip_find
      ( parse_one cb >>= fun pair ->
        yield pair;
        match pair with
        | _, Terminate -> return (Some ())
        | _ -> return_none )
    >>= function
    | None -> skip_over_cd_and_drain *> return `Reached_end
    | Some () -> return `Terminated
  in
  let open Buffered in
  let rec loop = function
    | Fail _ as state -> state_to_result state |> invalid
    | Done (_, `Reached_end) -> failwith "SZXX: ZIP processing completed before reaching end of input"
    | Done (_, `Terminated) -> ()
    | Partial feed -> (
      match read () with
      | `Eof as eof -> (
        match feed eof with
        | Fail _ as state -> state_to_result state |> invalid
        | _ -> () )
      | chunk -> (loop [@tailcall]) (feed chunk) )
  in
  loop (parse parser)

let parse_eocd =
  let* offset =
    bounded_file_reader ~slice_size:Int.(2 ** 10) ~pattern:Magic.start_eocd P.Storage.noop
    *> advance 12
    *> Parsers.parse_initial_offset
  in
  (* comment length (2) + comment *)
  let+ () = LE.any_int16 >>= advance in
  offset

let parse_eocd64 =
  let+ raw =
    bounded_file_reader ~slice_size:Int.(2 ** 10) ~pattern:Magic.start_eocd64 P.Storage.noop
    *> advance 44
    *> take 8
  in
  Some (parse_le_uint64 raw)

let parse_eocd64_locator =
  let+ raw =
    bounded_file_reader ~slice_size:Int.(2 ** 10) ~pattern:Magic.start_eocd64_locator P.Storage.noop
    *> advance 4
    *> take 8
  in
  Some (parse_le_uint64 raw)

let read_to_eof ~slice_size ~pos file =
  let buf = Buffer.create slice_size in
  let cs = Cstruct.create slice_size in
  let rec read_slice pos =
    let len = Eio.File.pread file [ cs ] ~file_offset:pos in
    Buffer.add_string buf (Cstruct.to_string cs ~len);
    read_slice Optint.Int63.(add pos (of_int len))
  in
  try read_slice (Optint.Int63.of_int pos) with
  | End_of_file -> Buffer.contents buf

let index_entries (file : _ Eio.File.ro) =
  (* First make sure the start is valid *)
  let () =
    let cs = Cstruct.create (String.length Magic.start_local_header) in
    Eio.File.pread_exact file ~file_offset:Optint.Int63.zero [ cs ];
    if String.(Cstruct.to_string cs <> Magic.start_local_header)
    then failwith "SZXX: Corrupted file. Invalid file marker."
  in
  let do_parse ~kind parser raw =
    match Angstrom.parse_string ~consume:Prefix parser raw with
    | Error msg -> Printf.failwithf "SZXX: Corrupted file. Unable to parse %s. Error: %s" kind msg ()
    | Ok x -> x
  in
  (* Find start of central dictory *)
  let offset =
    let size = (Eio.File.stat file).size |> Optint.Int63.to_int in
    let rec try_find_cd_offset rev_offset =
      let raw = read_to_eof file ~slice_size:4096 ~pos:(max 0 (size - rev_offset)) in

      let found =
        if String.is_substring raw ~substring:Magic.start_eocd64
        then do_parse parse_eocd64 raw ~kind:"EOCD64"
        else if String.is_substring raw ~substring:Magic.start_eocd64_locator
        then
          do_parse parse_eocd64_locator raw ~kind:"EOCD64_LOC"
          |> Option.map ~f:(fun eocd64_offset ->
               try_find_cd_offset (size - Int64.to_int_exn eocd64_offset) )
        else if (* Intentionally overshoot to make sure we don't miss an eocd64 or eocd64_locator *)
                String.is_substring raw ~substring:Magic.start_cd_header
        then do_parse parse_eocd raw ~kind:"EOCD"
        else if rev_offset = 0
        then failwith "SZXX: Corrupted file. No central directory."
        else None
      in
      match found, rev_offset with
      | Some x, _ -> x
      | None, 128 -> (try_find_cd_offset [@tailcall]) 512
      | None, 512 -> (try_find_cd_offset [@tailcall]) 4096
      | None, _ -> (try_find_cd_offset [@tailcall]) (rev_offset * 2)
    in
    try_find_cd_offset 128
  in
  (* Parse central directory *)
  let raw = read_to_eof ~slice_size:4096 ~pos:(Int64.to_int_exn offset) file in
  do_parse (Angstrom.many1 parse_cd_entry) raw ~kind:"CD"

let extract_from_index file entry action =
  let offset =
    match entry with
    | { descriptor = { offset = None; _ }; _ } ->
      failwith "SZXX: cannot extract entry because it did not come from Zip.index_contents"
    | { descriptor = { offset = Some x; _ }; _ } -> Int64.to_int_exn x
  in
  let read = Feed.of_flow_seekable ~offset file in
  let _entry, data =
    let open Buffered in
    let rec loop = function
      | (Fail _ | Done _) as state -> state_to_result state |> invalid
      | Partial feed -> (loop [@tailcall]) (feed (read ()))
    in
    loop (parse (parse_one (fun _ -> action)))
  in
  data
