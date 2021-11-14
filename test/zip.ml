let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core_kernel
open Lwt.Syntax
open Lwt.Infix

let fold xlsx_filename json_filename () =
  let xlsx_path = sprintf "../../../test/files/%s" xlsx_filename in
  let json_path = sprintf "../../../test/files/%s" json_filename in
  let* against =
    Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
        let+ contents = Lwt_io.read ic in
        Yojson.Safe.from_string contents)
  in
  let* parsed =
    let queue = Queue.create () in
    Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
        let len = Lwt_io.buffer_size ic in
        let buf = Bigstring.create len in
        let feed () =
          Lwt_io.read_into_bigstring ic buf 0 len >|= function
          | 0 -> None
          | len -> Some SZXX.Zip.{ buf; pos = 0; len }
        in
        let stream, processed =
          SZXX.Zip.stream_files ~feed:(Bigstring feed) (fun _ ->
              Fold_string { init = (); f = (fun _entry s () -> Queue.enqueue queue (`String s)) })
        in
        let+ () = Lwt.join [ Lwt_stream.iter (const ()) stream; processed ] in
        `Assoc [ "data", `List (List.rev (Queue.to_list queue)) ])
  in

  Json_diff.check parsed against;
  (* let* () =
       Lwt_io.with_file ~flags:[ O_WRONLY; O_NONBLOCK; O_TRUNC ] ~mode:Output json_path (fun oc ->
           Lwt_io.write oc (Yojson.Safe.to_string parsed))
     in *)
  Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "SZXX ZIP"
       [ "ZIP", [ "financial.xlsx", `Quick, fold "financial.xlsx" "chunks.json" ] ]
