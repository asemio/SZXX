let flags = Unix.[O_RDONLY; O_NONBLOCK]
open! Core_kernel

open SZXX.Xlsx

let string_readers = {
  string = (fun _location s -> s);
  error = (fun _location s -> s);
  boolean = (fun _location s -> s);
  number = (fun _location s -> s);
  null = "";
}

let count xlsx_path =
  Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
    let stream, _sst_p, processed = stream_rows string_readers ic in
    let t0 = Time_now.nanoseconds_since_unix_epoch () in

    let%lwt n =
      Lwt_stream.fold (fun _x acc -> acc + 1) stream 0
    in
    let t1 = Time_now.nanoseconds_since_unix_epoch () in

    let%lwt () = Lwt_io.printlf "Row count: %d (%Ldms)" n Int63.(((t1 - t0) / (of_int 1_000_000)) |> to_int64) in

    let%lwt () = processed in
    Lwt.return_unit
  )

let () =
  Sys.argv |> function
  | [| _; file |] -> Lwt_main.run (count file)
  | _ -> failwith "Invalid arguments"
