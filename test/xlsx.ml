let flags = Unix.[O_RDONLY; O_NONBLOCK]
open! Core_kernel

let extractors = SZXX.Xlsx.{
    string = (fun _location s -> `String (SZXX.Xml.unescape s));
    error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    null = `Null;
  }

let readme_example filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
    let open SZXX.Xlsx in
    (* yojson_readers is an easy way to quickly inspect a file *)
    let stream, processed = stream_rows_buffer yojson_readers ic in
    let%lwt () = Lwt_stream.iter (fun row ->
        (`List (Array.to_list row.data))
        |> Yojson.Basic.pretty_to_string
        |> print_endline
      ) stream
    in
    (* bind to/await the `processed` promise to catch any error that may have terminated the stream early *)
    processed
  )

let xlsx filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = sprintf "../../../test/files/%s.json" filename in
  let%lwt against = Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
      let%lwt contents = Lwt_io.read ic in
      Lwt.return (Yojson.Safe.from_string contents)
    )
  in
  let%lwt parsed = Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
      let open SZXX.Xlsx in
      let stream, processed = stream_rows_buffer extractors ic in
      let%lwt json = Lwt_stream.fold (fun row acc ->
          (`List (Array.to_list row.data)) :: acc
        ) stream []
      in
      let%lwt () = processed in
      Lwt.return (`Assoc ["data", `List (List.rev json)])
    )
  in

  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against;

  Lwt.return_unit

let () =
  Lwt_main.run @@ Alcotest_lwt.run "SZXX XLSX" [
    "XLSX", [
      "simple.xlsx", `Quick, xlsx "simple";
      "financial.xlsx", `Quick, xlsx "financial";
      "Readme example", `Quick, readme_example "financial";
    ];
  ]
