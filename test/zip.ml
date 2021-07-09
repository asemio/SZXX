let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core_kernel

let chunk () =
  let xlsx_path = "../../../test/files/financial.xlsx" in
  let json_path = "../../../test/files/chunks.json" in
  let%lwt against =
    Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
        let%lwt contents = Lwt_io.read ic in
        Lwt.return (Yojson.Safe.from_string contents))
  in
  let%lwt parsed =
    let queue = Queue.create () in
    Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
        let stream, processed =
          SZXX.Zip.stream_files ic (fun _ -> Chunk (fun s -> Queue.enqueue queue (`String s)))
        in
        let%lwt () = Lwt.join [ Lwt_stream.iter (const ()) stream; processed ] in
        Lwt.return (`Assoc [ "data", `List (List.rev (Queue.to_list queue)) ]))
  in

  Json_diff.check parsed against;
  (* let%lwt () =
       Lwt_io.with_file ~flags:[ O_WRONLY; O_NONBLOCK; O_TRUNC ] ~mode:Output json_path (fun oc ->
           Lwt_io.write oc (Yojson.Safe.to_string parsed))
     in *)
  Lwt.return_unit

let () = Lwt_main.run @@ Alcotest_lwt.run "SZXX ZIP" [ "ZIP", [ "financial.xlsx", `Quick, chunk ] ]
