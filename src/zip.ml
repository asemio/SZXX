(* Inspired by Xavier Leroy's https://github.com/xavierleroy/camlzip/blob/75917e4c41983b730f4ae21ae841de63132b42a2/zip.ml *)
(* Original copyright notice: https://github.com/xavierleroy/camlzip/blob/75917e4c41983b730f4ae21ae841de63132b42a2/zip.ml#L1-L12 *)

open! Core_kernel
open Angstrom

type methd =
| Stored
| Deflated
[@@deriving sexp_of]

type descriptor = {
  crc: Int32.t;
  compressed_size: int;
  uncompressed_size: int;
} [@@deriving sexp_of]

type entry = {
  version_needed: int;
  flags: int;
  trailing_descriptor_present: bool;
  methd: methd;
  descriptor: descriptor;
  filename: string;
  extra: string;
} [@@deriving sexp_of]

type chunk =
| CBuffer of Buffer.t
| CBytes of (bytes * int)

type 'a action =
| Skip
| String
| Parse of ('a Angstrom.t)

type 'a data =
| Skipped
| As_string of string
| Parsed of ('a, string) result

let double x y = x, y
let triple x y z = x, y, z
let maybe p = option None (p >>| Option.return)

let parser cb =
  let header_parser = string "PK\003\004" in
  let descriptor_parser = lift3 (fun crc compressed_size uncompressed_size -> {
        crc; compressed_size; uncompressed_size;
      })
      LE.any_int32 (* crc *)
      (LE.any_int32 >>| Int32.to_int_exn) (* compressed_size *)
      (LE.any_int32 >>| Int32.to_int_exn) (* uncompressed_size *)
  in
  let deflated_fns flush =
    let zs = Zlib.inflate_init false in
    let buf = Buffer.create 1024 in
    let inbuf = Bytes.create 1024 in
    let outbuf = Bytes.create 1024 in
    let rec do_uncompress inpos inavail =
      let finished, used_in, used_out = Zlib.inflate zs inbuf inpos inavail outbuf 0 1024 Z_SYNC_FLUSH in
      if used_out > 0 then flush (CBytes (outbuf, used_out));
      if not finished && used_in > 0 then do_uncompress (inpos + used_in) (inavail - used_in)
    in
    let uncompress () =
      let len = Buffer.length buf in
      if Int.(len = 0) then () else
      Buffer.blit ~src:buf ~src_pos:0 ~dst:inbuf ~dst_pos:0 ~len;
      do_uncompress 0 len;
      Buffer.clear buf
    in
    let add c =
      Buffer.add_char buf c;
      if Int.((Buffer.length buf) = 1024) then uncompress ();
    in
    let finalize () =
      uncompress ();
      (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
         after the compressed stream in order to complete decompression
         and return finished = true. *)
      let _finished, _, used_out = Zlib.inflate zs inbuf 0 1 outbuf 0 1024 Z_SYNC_FLUSH in
      if used_out > 0 then flush (CBytes (outbuf, used_out));
      Zlib.inflate_end zs
    in
    add, finalize
  in
  let stored_fns flush =
    let buf = Buffer.create 1024 in
    let add c =
      Buffer.add_char buf c;
      if Int.((Buffer.length buf) = 1024)
      then begin
        flush (CBuffer buf);
        Buffer.clear buf
      end
    in
    add, (fun () -> ())
  in
  let bounded_file_reader add finalize = (
    let rec loop n ll =
      any_char >>= (fun c -> begin match c, n with
        | 'P', 0 -> loop 1 ('P'::ll)
        | 'K', 1 -> loop 2 ('K'::ll)
        | '\007', 2 -> loop 3 ('\007'::ll)
        | '\008', 3 -> finalize (); return ()
        | c, 0 ->
          add c;
          loop 0 ll
        | c, _ ->
          List.fold_right (c::ll) ~init:() ~f:(fun x () -> add x);
          loop 0 []
        end)
    in
    loop 0 []
  )
  in
  let fixed_size_reader size add finalize = (
    let rec loop = function
    | 0 -> finalize (); return ()
    | n ->
      any_char >>= (fun c ->
        add c;
        loop (pred n)
      )
    in
    loop size
  )
  in
  let entry_parser =
    (lift4 (fun () (vn, flags, methd) descriptor (filename, extra) -> {
            version_needed = vn;
            flags;
            trailing_descriptor_present = (flags land 0x008) <> 0 || descriptor.compressed_size = 0 || descriptor.uncompressed_size = 0;
            methd;
            descriptor;
            filename;
            extra;
          })
          (
            let rec loop n =
              any_char >>= (fun c ->
                begin match c, n with
                | 'P', 0 -> loop 1
                | 'K', 1 -> loop 2
                | '\003', 2 -> loop 3
                | '\004', 3 -> return ()
                | _ -> loop 0
                end
              )
            in
            loop 0
          )
          (lift3 triple
              (LE.any_uint16 (* version_needed *)
                >>| function
                | 20 -> 20
                | x -> failwithf "Unsupported version: %d. Please report this bug." x ())
              (LE.any_uint16 (* flags *)
                >>| (fun flags ->
                  if flags land 0x001 <> 0 then failwith "Encrypted entries not supported";
                  flags
                ))
              (LE.any_uint16 (* methd *)
                >>| function
                | 0 -> Stored
                | 8 -> Deflated
                | x -> failwithf "Unsupported compression method %d" x ())
            <* LE.any_uint16 (* last modified time *)
            <* LE.any_uint16 (* last modified date *)
          )
          descriptor_parser
          (
            (lift2 double
                LE.any_uint16 (* filename length *)
                LE.any_uint16 (* extra length *)
            ) >>= (fun (len1, len2) -> lift2 double (take len1) (take len2))
          )
    )
  in
  let skip_mode () =
    let crc = ref Int32.zero in
    let bytes_processed = ref 0 in
    let flush = begin function
    | CBuffer buf ->
      let len = Buffer.length buf in
      bytes_processed := !bytes_processed + len;
      crc := Zlib.update_crc_string !crc (Buffer.contents buf) 0 len
    | CBytes (bytes, len) ->
      bytes_processed := !bytes_processed + len;
      crc := Zlib.update_crc !crc bytes 0 len
    end
    in
    let complete () = Skipped, !bytes_processed, !crc in
    flush, complete
  in
  let string_mode size =
    let res = Buffer.create size in
    let flush = begin function
    | CBuffer buf -> Buffer.add_buffer res buf
    | CBytes (bytes, len) -> Buffer.add_subbytes res bytes ~pos:0 ~len
    end
    in
    let complete () =
      let str = Buffer.contents res in
      let len = String.length str in
      (As_string str), len, (Zlib.update_crc_string Int32.zero str 0 len)
    in
    flush, complete
  in
  let parser_mode angstrom =
    let crc = ref Int32.zero in
    let bytes_processed = ref 0 in
    let open Buffered in
    let state = ref (parse angstrom) in
    let process s =
      let len = String.length s in
      bytes_processed := !bytes_processed + len;
      crc := Zlib.update_crc_string !crc s 0 len;
      begin match !state with
      | Done _ | Fail _ -> ()
      | Partial feed -> state := (feed (`String s))
      end
    in
    let flush = begin function
    | CBuffer buf -> process (Buffer.contents buf)
    | CBytes (bytes, len) -> process (Bytes.To_string.sub bytes ~pos:0 ~len)
    end
    in
    let complete () =
      let final_state = begin match !state with
      | (Done _ as x) | (Fail _ as x) -> x
      | Partial feed -> feed `Eof
      end
      in
      (Parsed (state_to_result final_state)), !bytes_processed, !crc
    in
    flush, complete
  in
  let file_parser =
    entry_parser
    >>= (fun entry ->
      let reader ?(size = 4096) () =
        let flush, complete = begin match cb entry with
        | Skip -> skip_mode ()
        | String -> string_mode size
        | Parse angstrom -> parser_mode angstrom
        end
        in
        let (add, finalize), zipped_length = begin match entry.methd with
        | Stored -> (stored_fns flush), entry.descriptor.uncompressed_size
        | Deflated -> (deflated_fns flush), entry.descriptor.compressed_size
        end
        in
        let file_reader = if entry.descriptor.compressed_size = 0 || entry.descriptor.uncompressed_size = 0
          then bounded_file_reader else (fixed_size_reader zipped_length)
        in
        (file_reader add finalize) >>| complete
      in
      begin match entry.trailing_descriptor_present with
      | false -> lift2 double (reader ~size:entry.descriptor.uncompressed_size ()) (return entry)
      | true -> lift2 double (reader ()) (((maybe header_parser) *> descriptor_parser) >>| (fun descriptor -> { entry with descriptor }))
      end
      >>| (begin function
      | (_data, size, _crc), entry when entry.descriptor.uncompressed_size <> size -> failwithf "%s: Size mismatch" entry.filename ()
      | ((_data, _size, crc), entry) when Int32.(entry.descriptor.crc <> crc) -> failwithf "%s: CRC mismatch" entry.filename ()
      | ((data, _size, _crc), entry) -> entry, data
      end
      )
    )
  in
  file_parser

let mutex = Lwt_mutex.create ()

let stream_files input_channel cb =
  let stream, bounded = Lwt_stream.create_bounded 1 in
  Lwt.async (fun () ->
    Lwt.finalize (fun () ->
      let%lwt _unconsumed, result = Angstrom_lwt_unix.parse_many
          (parser cb)
          (fun pair ->
              Lwt_mutex.with_lock mutex (fun () ->
                bounded#push pair
              )
          )
          input_channel
      in
      begin match result with
      | Ok () -> Lwt.return_unit
      | Error err -> failwithf "Syntax Error: %s" err ()
      end
    ) (fun () ->
      if not (Lwt_stream.is_closed stream) then bounded#close;
      if not (Lwt_io.is_closed input_channel) then Lwt_io.close input_channel else Lwt.return_unit
    )
  );
  stream
