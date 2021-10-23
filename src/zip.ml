open! Core_kernel

type methd =
  | Stored
  | Deflated
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: int;
  uncompressed_size: int;
}
[@@deriving sexp_of]

type entry = {
  version_needed: int;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra: string;
}
[@@deriving sexp_of]

type chunk = Bigstring.t * int

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

let double x y = x, y

let triple x y z = x, y, z

let slice_size = 1024

let slice_bits = 10

module Storage = struct
  type t = {
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
        if len > 0 then flush (outbs, len);
        true
      | `Flush ->
        let len = slice_size - De.Inf.dst_rem decoder in
        flush (outbs, len);
        De.Inf.flush decoder;
        do_uncompress ()
      | `Malformed err -> failwith err
    in
    let uncompress bs len =
      De.Inf.src decoder bs 0 len;
      do_uncompress ()
    in
    let add bs len = finished := uncompress bs len in
    let finalize () = if not !finished then ignore (uncompress outbs 0) in
    { add; finalize }

  let stored flush =
    let add bs len = flush (bs, len) in
    let finalize () = () in
    { add; finalize }
end

module Mode = struct
  type 'a t = {
    flush: chunk -> unit;
    complete: unit -> 'a Data.t * int * Optint.t;
  }

  let skip () =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let flush (bs, len) =
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_bigstring bs 0 len !crc
    in
    let complete () = Data.Skip, !bytes_processed, !crc in
    { flush; complete }

  let string size =
    let res = Bigbuffer.create size in
    let flush (bs, len) = Bigbuffer.add_bigstring res (Bigstring.sub_shared bs ~pos:0 ~len) in
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
    let flush (bs, len) =
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
    let flush (bs, len) =
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
    let flush (bs, len) =
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

let parser cb =
  let open Angstrom in
  let header_parser = string "PK\003\004" >>| const () in
  let descriptor_parser =
    lift3
      (fun crc compressed_size uncompressed_size -> { crc; compressed_size; uncompressed_size })
      LE.any_int32 (LE.any_int32 >>| Int32.to_int_exn) (LE.any_int32 >>| Int32.to_int_exn)
  in
  let bounded_file_reader Storage.{ add; finalize } =
    let buf = Bigbuffer.create slice_size in
    let add_char c =
      Bigbuffer.add_char buf c;
      let len = Bigbuffer.length buf in
      if len = slice_size
      then begin
        add (Bigbuffer.volatile_contents buf) len;
        Bigbuffer.clear buf
      end
    in
    scan_state 0 (fun n c ->
        match n, c with
        | 0, 'P' -> Some 1
        | 1, 'K' -> Some 2
        | 2, '\007' -> Some 3
        | 3, '\008' ->
          let len = Bigbuffer.length buf in
          if len > 0 then add (Bigbuffer.volatile_contents buf) len;
          finalize ();
          None
        | 0, c ->
          add_char c;
          Some 0
        | 1, c ->
          add_char 'P';
          add_char c;
          Some 0
        | 2, c ->
          add_char 'P';
          add_char 'K';
          add_char c;
          Some 0
        | _, c ->
          add_char 'P';
          add_char 'K';
          add_char '\007';
          add_char c;
          Some 0)
    >>= fun _ -> advance 1
  in
  let fixed_size_reader size Storage.{ add; finalize } =
    let rec loop = function
      | n when n > slice_size ->
        take_bigstring slice_size >>= fun bs ->
        add bs slice_size;
        (loop [@tailcall]) (n - slice_size)
      | 0 ->
        finalize ();
        return ()
      | n ->
        take_bigstring n >>= fun bs ->
        add bs n;
        (loop [@tailcall]) 0
    in
    loop size
  in
  let entry_parser =
    lift4
      (fun () (version_needed, flags, methd) descriptor (filename, extra) ->
        {
          version_needed;
          flags;
          trailing_descriptor_present =
            flags land 0x008 <> 0 || descriptor.compressed_size = 0 || descriptor.uncompressed_size = 0;
          methd;
          descriptor;
          filename;
          extra;
        })
      (let rec loop n =
         any_char >>= fun c ->
         match c, n with
         | 'P', 0 -> (loop [@tailcall]) 1
         | 'K', 1 -> (loop [@tailcall]) 2
         | '\003', 2 -> (loop [@tailcall]) 3
         | '\004', 3 -> return ()
         | _ -> (loop [@tailcall]) 0
       in
       loop 0)
      (lift3 triple
         (LE.any_uint16 (* version_needed *) >>| function
          | 20 -> 20
          | x -> failwithf "Unsupported version: %d. Please report this bug." x ())
         ( LE.any_uint16 (* flags *) >>| fun flags ->
           if flags land 0x001 <> 0 then failwith "Encrypted entries not supported";
           flags )
         (LE.any_uint16 (* methd *) >>| function
          | 0 -> Stored
          | 8 -> Deflated
          | x -> failwithf "Unsupported compression method %d" x ())
      <* LE.any_uint16 (* last modified time *)
      <* LE.any_uint16 (* last modified date *)
      )
      descriptor_parser
      ( lift2 double LE.any_uint16 (* filename length *) LE.any_uint16 (* extra length *)
      >>= fun (len1, len2) -> lift2 double (take len1) (take len2) )
  in
  lift2 const entry_parser commit >>= fun entry ->
  let reader ?(size = 4096) () =
    let Mode.{ flush; complete } =
      match cb entry with
      | Action.Skip -> Mode.skip ()
      | Action.String -> Mode.string size
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
      if entry.descriptor.compressed_size = 0 || entry.descriptor.uncompressed_size = 0
      then bounded_file_reader
      else fixed_size_reader zipped_length
    in
    file_reader storage_method >>| complete
  in
  let file =
    match entry.trailing_descriptor_present with
    | false -> reader ~size:entry.descriptor.uncompressed_size () >>| fun x -> x, entry
    | true ->
      lift3
        (fun data_size_crc () descriptor -> data_size_crc, { entry with descriptor })
        (reader ()) (option () header_parser) descriptor_parser
  in
  lift2
    (fun data () ->
      match data with
      | (_data, size, _crc), entry when entry.descriptor.uncompressed_size <> size ->
        failwithf "%s: Size mismatch: %d but expected %d" entry.filename size
          entry.descriptor.uncompressed_size ()
      | (_data, _size, crc), entry when Int32.(entry.descriptor.crc <> Optint.to_int32 crc) ->
        failwithf "%s: CRC mismatch" entry.filename ()
      | (data, _size, _crc), entry -> entry, data)
    file commit

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
      let%lwt () = Lwt_mutex.with_lock mutex (fun () -> bounded#push pair) in
      match parse (parser cb) with
      | Partial feed -> (loop [@tailcall]) (feed (`Bigstring (Bigstring.sub_shared buf ~pos ~len)))
      | state -> (loop [@tailcall]) state
    )
    | Partial feed -> (
      match%lwt read () with
      | None -> (
        match feed `Eof with
        | Done (_, pair) -> Lwt_mutex.with_lock mutex (fun () -> bounded#push pair)
        | _ -> Lwt.return_unit
      )
      | Some chunk -> (loop [@tailcall]) (feed chunk)
    )
  in
  let p =
    Lwt.finalize
      (fun () -> loop (parse (parser cb)))
      (fun () ->
        bounded#close;
        Lwt.return_unit)
  in
  stream, p
