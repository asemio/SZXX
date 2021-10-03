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
    | Chunk  of (entry * string -> unit)
    | Parse  of 'a Angstrom.t
end

module Data = struct
  type 'a t =
    | Skip
    | String of string
    | Chunk
    | Parse  of ('a, string) result
end

let double x y = x, y

let triple x y z = x, y, z

let maybe p = Angstrom.(option None (p >>| Option.return))

module Storage = struct
  type t = {
    add: char -> unit;
    finalize: unit -> unit;
  }

  let deflated flush =
    let w = De.make_window ~bits:10 in
    let inbs_pos = ref 0 in
    let inbs = Bigstring.create 1024 in
    let outbs = Bigstring.create 1024 in
    let decoder = De.Inf.decoder `Manual ~o:outbs ~w in
    let rec do_uncompress () =
      match De.Inf.decode decoder with
      | `Await -> false
      | `End ->
        let len = Bigstring.length outbs - De.Inf.dst_rem decoder in
        if len > 0 then flush (outbs, len);
        true
      | `Flush ->
        let len = Bigstring.length outbs - De.Inf.dst_rem decoder in
        flush (outbs, len);
        De.Inf.flush decoder;
        do_uncompress ()
      | `Malformed err -> failwith err
    in
    let uncompress () =
      De.Inf.src decoder inbs 0 !inbs_pos;
      inbs_pos := 0;
      do_uncompress ()
    in
    let add c =
      Bigstring.set inbs !inbs_pos c;
      incr inbs_pos;
      if Int.(!inbs_pos = 1024) then ignore (uncompress ())
    in
    let finalize () = if not (uncompress ()) then ignore (uncompress ()) in
    { add; finalize }

  let stored flush =
    let bs = Bigstring.create 1024 in
    let pos = ref 0 in
    let add c =
      Bigstring.set bs !pos c;
      incr pos;
      if !pos = 1024
      then begin
        flush (bs, 1024);
        pos := 0
      end
    in
    let finalize () =
      let len = !pos in
      if len > 0 then flush (bs, len)
    in
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

  let chunk entry write =
    let crc = ref Optint.zero in
    let bytes_processed = ref 0 in
    let process s =
      let len = String.length s in
      bytes_processed := !bytes_processed + len;
      crc := Checkseum.Crc32.digest_string s 0 len !crc;
      write (entry, s)
    in
    let flush (bs, len) = process (Bigstring.to_string bs ~pos:0 ~len) in
    let complete () = Data.Chunk, !bytes_processed, !crc in
    { flush; complete }

  let parser angstrom =
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
  let header_parser = string "PK\003\004" in
  let descriptor_parser =
    lift3
      (fun crc compressed_size uncompressed_size -> { crc; compressed_size; uncompressed_size })
      LE.any_int32 (* crc *)
      (LE.any_int32 >>| Int32.to_int_exn) (* compressed_size *)
      (LE.any_int32 >>| Int32.to_int_exn)
    (* uncompressed_size *)
  in
  let bounded_file_reader Storage.{ add; finalize } =
    let rec loop n ll =
      any_char >>= fun c ->
      match c, n with
      | 'P', 0 -> loop 1 ('P' :: ll)
      | 'K', 1 -> loop 2 ('K' :: ll)
      | '\007', 2 -> loop 3 ('\007' :: ll)
      | '\008', 3 ->
        finalize ();
        return ()
      | c, 0 ->
        add c;
        loop 0 ll
      | c, _ ->
        List.fold_right (c :: ll) ~init:() ~f:(fun x () -> add x);
        loop 0 []
    in
    loop 0 []
  in
  let fixed_size_reader size Storage.{ add; finalize } =
    let rec loop = function
      | 0 ->
        finalize ();
        return ()
      | n ->
        any_char >>= fun c ->
        add c;
        loop (pred n)
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
         | 'P', 0 -> loop 1
         | 'K', 1 -> loop 2
         | '\003', 2 -> loop 3
         | '\004', 3 -> return ()
         | _ -> loop 0
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
  entry_parser >>= fun entry ->
  let reader ?(size = 4096) () =
    let Mode.{ flush; complete } =
      match cb entry with
      | Action.Skip -> Mode.skip ()
      | Action.String -> Mode.string size
      | Action.Chunk write -> Mode.chunk entry write
      | Action.Parse angstrom -> Mode.parser angstrom
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
  begin
    match entry.trailing_descriptor_present with
    | false -> lift2 double (reader ~size:entry.descriptor.uncompressed_size ()) (return entry)
    | true ->
      lift2 double (reader ())
        (maybe header_parser *> descriptor_parser >>| fun descriptor -> { entry with descriptor })
  end
  >>| function
  | (_data, size, _crc), entry when entry.descriptor.uncompressed_size <> size ->
    failwithf "%s: Size mismatch: %d <> %d" entry.filename entry.descriptor.uncompressed_size size ()
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
      let%lwt () = Lwt_mutex.with_lock mutex (fun () -> bounded#push pair) in
      match parse (parser cb) with
      | Partial feed -> loop (feed (`Bigstring (Bigstring.sub_shared buf ~pos ~len)))
      | state -> loop state
    )
    | Partial feed -> (
      match%lwt read () with
      | None -> (
        match feed `Eof with
        | Done (_, pair) -> Lwt_mutex.with_lock mutex (fun () -> bounded#push pair)
        | _ -> Lwt.return_unit
      )
      | Some chunk -> loop (feed chunk)
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
