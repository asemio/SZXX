let flags_read = Unix.[ O_RDONLY; O_NONBLOCK ]

let flags_overwrite = Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]

open! Core_kernel
open Lwt.Syntax
open SZXX

let string_readers : string Xlsx.cell_parser =
  {
    string = (fun _location s -> s);
    error = (fun _location s -> s);
    boolean = (fun _location s -> s);
    number = (fun _location s -> s);
    null = "";
  }

let feed_string ic =
  let open Lwt.Infix in
  let open SZXX.Zip in
  let count = Lwt_io.buffer_size ic in
  String
    (fun () ->
      Lwt_io.read ~count ic >|= function
      | "" -> None
      | chunk -> Some chunk)

let feed_bigstring ic =
  let open Lwt.Infix in
  let open SZXX.Zip in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })

let count xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let t0 = Time_now.nanoseconds_since_unix_epoch () in
      let stream, _sst_p, processed =
        Xlsx.stream_rows_unparsed ~feed:(feed_bigstring ic) ~skip_sst:true ()
      in

      let* n = Lwt_stream.fold (fun _x acc -> acc + 1) stream 0 in
      let t1 = Time_now.nanoseconds_since_unix_epoch () in

      let* () =
        Lwt_io.printlf "Row count: %d (%Ldms)" n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64)
      in

      processed)

let length xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let stream, p = Zip.stream_files ~feed:(feed_bigstring ic) (const Zip.Action.String) in
      let* () =
        Lwt_stream.iter
          (function
            | ({ filename; _ } : Zip.entry), Zip.Data.String raw ->
              print_endline (sprintf "%s: %d" filename (String.length raw))
            | _ -> failwith "Expected Zip.Data.String")
          stream
      in
      p)

let extract_sst xlsx_path =
  Lwt_io.with_file ~flags:flags_overwrite ~mode:Output (sprintf "%s.sst.xml" xlsx_path) (fun oc ->
      Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
          let files, files_p =
            Zip.stream_files ~feed:(feed_bigstring ic) (function
              | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
              | _ -> Zip.Action.Skip)
          in
          let* () =
            Lwt_stream.iter_s
              (function
                | _, Zip.Data.String s -> Lwt_io.write oc s
                | _ -> Lwt.return_unit)
              files
          in
          files_p))

let show_json xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let open Lwt.Syntax in
      (* yojson_cell_parser is an easy way to quickly inspect a file by mapping XLSX's data types to JSON *)
      let stream, success =
        SZXX.Xlsx.stream_rows_buffer ~feed:(feed_string ic) SZXX.Xlsx.yojson_cell_parser
      in
      let processed =
        Lwt_stream.iter
          (fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
            `List (Array.to_list row.data) |> Yojson.Basic.pretty_to_string |> print_endline)
          stream
      in
      let* () = success in
      (* bind to/await the `success` promise to catch any error that may have terminated the stream early *)
      let* () = processed in
      (* ... *)
      Lwt.return_unit)

let () =
  Sys.argv |> function
  | [| _; "extract_sst"; file |] -> Lwt_main.run (extract_sst file)
  | [| _; "count"; file |] -> Lwt_main.run (count file)
  | [| _; "length"; file |] -> Lwt_main.run (length file)
  | [| _; "show_json"; file |] -> Lwt_main.run (show_json file)
  | _ -> failwith "Invalid arguments"
