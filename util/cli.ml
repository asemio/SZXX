let flags_read = Unix.[ O_RDONLY; O_NONBLOCK ]

let flags_overwrite = Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]

open! Core_kernel
open SZXX

let string_readers : string Xlsx.cell_of_string =
  {
    string = (fun _location s -> s);
    error = (fun _location s -> s);
    boolean = (fun _location s -> s);
    number = (fun _location s -> s);
    null = "";
  }

let count xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let stream, _sst_p, processed = Xlsx.stream_rows string_readers ic in
      let t0 = Time_now.nanoseconds_since_unix_epoch () in

      let%lwt n = Lwt_stream.fold (fun _x acc -> acc + 1) stream 0 in
      let t1 = Time_now.nanoseconds_since_unix_epoch () in

      let%lwt () =
        Lwt_io.printlf "Row count: %d (%Ldms)" n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64)
      in

      let%lwt () = processed in
      Lwt.return_unit)

let extract xlsx_path =
  Lwt_io.with_file ~flags:flags_overwrite ~mode:Output (sprintf "%s.sst.xml" xlsx_path) (fun oc ->
      Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
          let files, files_p =
            Zip.stream_files ic (function
              | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
              | _ -> Zip.Action.Skip)
          in
          let%lwt () =
            Lwt_stream.iter_s
              (function
                | _, Zip.Data.String s -> Lwt_io.write oc s
                | _ -> Lwt.return_unit)
              files
          in
          files_p))

let () =
  Sys.argv |> function
  | [| _; "extract"; file |] -> Lwt_main.run (extract file)
  | [| _; "count"; file |] -> Lwt_main.run (count file)
  | _ -> failwith "Invalid arguments"
