open! Core_kernel

open SZXX.Xlsx

let count xlsx_path =
  Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
    let stream, processed = stream_rows yojson_readers ic in
    let t0 = Time_now.nanoseconds_since_unix_epoch () in

    let n = ref 0 in
    let%lwt () = try%lwt
      Lwt_stream.junk_while (fun _x -> incr n; true) stream
    with _ -> Lwt.return_unit
    in
    let%lwt () = processed in
    let t1 = Time_now.nanoseconds_since_unix_epoch () in


    let%lwt () = Lwt_io.printlf "Row count: %d (%Ldms)" !n Int63.(((t1 - t0) / (of_int 1_000_000)) |> to_int64) in
    Lwt.return_unit
  )

let () =
  Sys.argv |> function
  | [| _; file |] -> Lwt_main.run (count file)
  | _ -> failwith "Invalid arguments"
