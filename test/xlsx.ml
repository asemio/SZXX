let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core_kernel
open Lwt.Infix

let extractors =
  SZXX.Xlsx.
    {
      string = (fun _location s -> `String (SZXX.Xml.unescape s));
      error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
      boolean = (fun _location s -> `Bool String.(s = "1"));
      number = (fun _location s -> `Float (Float.of_string s));
      null = `Null;
    }

let feed_string ic =
  let open SZXX.Zip in
  let count = Lwt_io.buffer_size ic in
  String
    (fun () ->
      Lwt_io.read ~count ic >|= function
      | "" -> None
      | chunk -> Some chunk)

let feed_bigstring ic =
  let open SZXX.Zip in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })

let readme_example filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
      let open SZXX.Xlsx in
      (* yojson_readers is an easy way to quickly inspect a file *)
      let stream, processed = stream_rows_buffer ~feed:(feed_string ic) yojson_cell_parser in
      let%lwt () =
        Lwt_stream.iter
          (fun row -> `List (Array.to_list row.data) |> Yojson.Basic.pretty_to_string |> print_endline)
          stream
      in
      (* bind to/await the `processed` promise to catch any error that may have terminated the stream early *)
      processed)

let xlsx filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = sprintf "../../../test/files/%s.json" filename in
  let%lwt against =
    Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
        let%lwt contents = Lwt_io.read ic in
        Lwt.return (Yojson.Safe.from_string contents))
  in
  let%lwt parsed =
    Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
        let open SZXX.Xlsx in
        let stream, processed = stream_rows_buffer ~feed:(feed_bigstring ic) extractors in
        let json_p = Lwt_stream.fold (fun row acc -> `List (Array.to_list row.data) :: acc) stream [] in
        let%lwt () = processed in
        let%lwt json = json_p in
        Lwt.return (`Assoc [ "data", `List (List.rev json) ]))
  in

  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against;

  Lwt.return_unit

let stream_rows filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = "../../../test/files/streamed.json" in
  let%lwt against =
    Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
        let%lwt contents = Lwt_io.read ic in
        Lwt.return (Yojson.Safe.from_string contents))
  in
  let%lwt parsed =
    Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
        let open SZXX.Xlsx in
        let stream, sst, processed = stream_rows ~feed:(feed_bigstring ic) yojson_cell_parser in
        let%lwt sst = sst in
        let json_list_p =
          Lwt_stream.map
            (fun status ->
              let row = await_delayed yojson_cell_parser sst status in
              `List (Array.to_list row.data))
            stream
          |> Lwt_stream.to_list
        in
        let%lwt () = processed in
        let%lwt json_list = json_list_p in
        Lwt.return (`Assoc [ "data", `List json_list ]))
  in
  (* let%lwt () =
       Lwt_io.with_file ~flags:[ O_WRONLY; O_NONBLOCK; O_TRUNC ] ~mode:Output json_path (fun oc ->
           Lwt_io.write oc (Yojson.Safe.to_string parsed))
     in *)
  Json_diff.check parsed against;
  Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "SZXX XLSX"
       [
         ( "XLSX",
           [
             "simple.xlsx", `Quick, xlsx "simple";
             "financial.xlsx", `Quick, xlsx "financial";
             "Readme example", `Quick, readme_example "financial";
             "Unbuffed stream", `Quick, stream_rows "financial";
           ] );
       ]
