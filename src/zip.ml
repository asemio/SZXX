open! Core
open Angstrom
open Parsing

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
}
[@@deriving sexp_of, compare, equal]

module Action = struct
  type 'a t =
    | Skip
    | String
    | Bigstring
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
    | Terminate
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
  [@@deriving sexp_of, compare, equal]

  let parser_state_to_result = function
  | Success x -> Ok x
  | Failed { error; unconsumed = _ } -> Error (sprintf "Parsing error: %s" error)
  | Terminated_early { unconsumed = _ } -> Error "Could not parse all input"
  | Incomplete -> Error "File incomplete, encountered EOF too early"

  type 'a t =
    | Skip
    | String of string
    | Bigstring of Bigstring.t
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
  type t = Parsing.Storage.t = {
    add: Bigstring.t -> off:int -> len:int -> unit;
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
      | `Malformed err -> failwithf "SZXX: Corrupted file. Malformed deflate data. Error: %s" err ()
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
    flush: Bigstring.t -> off:int -> len:int -> unit;
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
      Failed { error = sprintf "%s: %s" (String.concat ~sep:" > " ll) msg; unconsumed = unconsumed u }
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
      | Done ({ len = 0; _ }, x) -> state := Done ({ buf = Bigstring.copy bs; len; off }, x)
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = Bigstring.copy bs; len; off }, x, y)
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
      | Done ({ len = 0; _ }, ()) -> state := Done ({ buf = Bigstring.copy bs; len; off }, ())
      | Fail ({ len = 0; _ }, x, y) -> state := Fail ({ buf = Bigstring.copy bs; len; off }, x, y)
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

let parse_descriptor =
  let+ crc = LE.any_int32
  and+ c_size = LE.any_int32
  and+ r_size = LE.any_int32 in
  { crc; compressed_size = Int32.to_int64 c_size; uncompressed_size = Int32.to_int64 r_size }

let parse_entry =
  let rec extra_fields_parser acc = function
    | 0 -> return acc
    | left when left < 0 ->
      failwith "SZXX: Corrupted file. Mismatch between reported and actual extra fields size"
    | left ->
      let* id = LE.any_uint16 in
      let* size = LE.any_uint16 in
      let* data = take size in
      (extra_fields_parser [@tailcall]) ({ id; size; data } :: acc) (left - (size + 4))
  in
  let dynamic_len_fields_parser =
    let* filename_len = LE.any_uint16 in
    let* extra_fields_len = LE.any_uint16 in
    let+ filename = take filename_len
    and+ extra_fields = extra_fields_parser [] extra_fields_len in
    filename, extra_fields
  in
  let flags_methd_parser =
    let+ flags = LE.any_uint16
    and+ methd = LE.any_uint16 in
    if flags land 0x001 <> 0 then failwith "SZXX: Encrypted ZIP entries not supported";
    let methd =
      match methd with
      | 0 -> Stored
      | 8 -> Deflated
      | x -> failwithf "SZXX: Unsupported ZIP compression method %d" x ()
    in
    flags, methd
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
    let* () =
      bounded_file_reader ~slice_size:Int.(2 ** 10) ~pattern:"PK\003\004" Parsing.Storage.noop_backtrack
    in
    let+ version_needed =
      LE.any_uint16 >>| function
      | 20 -> Zip_2_0
      | 45 -> Zip_4_5
      | x -> failwithf "SZXX: Corrupted file. Invalid ZIP version: %d" x ()
    and+ flags, methd =
      flags_methd_parser
      <* LE.any_uint16 (* last modified time *)
      <* LE.any_uint16 (* last modified date *)
    and+ descriptor = parse_descriptor
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
  entry_parser <* commit

let parse_one cb =
  let local_file_header_signature = string "PK\003\004" in
  let* entry = parse_entry in
  let reader ~buffer_size (action : 'a Action.t) =
    let Mode.{ flush; complete } =
      match action with
      | Action.Skip -> Mode.skip ()
      | Action.String -> Mode.string ~buffer_size
      | Action.Bigstring -> Mode.bigstring ~buffer_size
      | Action.Fold_string { init; f } -> Mode.fold_string ~init ~f entry
      | Action.Fold_bigstring { init; f } -> Mode.fold_bigstring ~init ~f entry
      | Action.Parse parser -> Mode.parse parser
      | Action.Parse_many { parser; on_parse } -> Mode.parse_many parser on_parse
      | Action.Terminate -> assert false
    in
    let storage_method, zipped_length =
      match entry.methd with
      | Stored -> Storage.stored flush, entry.descriptor.uncompressed_size
      | Deflated -> Storage.deflated flush, entry.descriptor.compressed_size
    in
    let file_reader =
      if Int64.(entry.descriptor.compressed_size = 0L) || Int64.(entry.descriptor.uncompressed_size = 0L)
      then bounded_file_reader ~slice_size:Int.(2 ** 11) ~pattern:"PK\007\008"
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
        and+ descriptor = option () local_file_header_signature *> parse_descriptor in
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
    | true, true -> entry, data )

let parse_trailing_central_directory =
  let* () = bounded_file_reader ~slice_size:Int.(2 ** 10) ~pattern:"PK\005\006" Parsing.Storage.noop in
  advance 18 *> end_of_input

let to_sequence stream = Seq.of_dispenser (fun () -> Eio.Stream.take stream) |> Sequence.of_seq

let stream_files ~sw ~feed:(read : Feed.t) cb =
  let open Eio.Std in
  let stream = Eio.Stream.create 0 in
  let parser =
    skip_find
      (parse_one cb >>= function
       | (_, Terminate) as pair ->
         Eio.Stream.add stream (Some pair);
         return (Some ())
       | pair ->
         Eio.Stream.add stream (Some pair);
         return_none )
    >>= function
    | None -> parse_trailing_central_directory *> return `Reached_end
    | Some () -> return `Terminated
  in
  let open Buffered in
  let invalid = function
    | Ok x -> x
    | Error msg -> failwithf "SZXX: Corrupted file. Invalid ZIP structure. Error: %s" msg ()
  in
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
  Fiber.fork_daemon ~sw (fun () ->
    loop (parse parser);
    (* Keep adding [None] in the background to allow the user to use
       the Sequence again even after it's been iterated over. It doesn't
       memoize the elements, but it does allow [Sequence.is_empty] to return [true]
       without deadlocking, and [Sequence.fold] to return its initial value
       without deadlocking, etc. *)
    while true do
      Eio.Stream.add stream None
    done;
    `Stop_daemon );
  to_sequence stream
