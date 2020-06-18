open! Core_kernel

let extractors = SZXX.Xlsx.{
    string = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `String s);
    error = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `String s);
    boolean = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `Bool String.(s = "1"));
    number = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `Float (Float.of_string s));
    null = `Null;
  }

let readme_example filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
    let open SZXX.Xlsx in
    let stream, processed = stream_rows yojson_readers ic in
    let%lwt () = Lwt_stream.iter (fun row ->
        (`List (Array.to_list row.data))
        |> Yojson.Basic.pretty_to_string
        |> print_endline
      ) stream
    in
    processed
  )

let xlsx filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = sprintf "../../../test/files/%s.json" filename in
  let%lwt against = Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input json_path (fun ic ->
      let%lwt contents = Lwt_io.read ic in
      Lwt.return (Yojson.Safe.from_string contents)
    )
  in
  let%lwt parsed = Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
      let open SZXX.Xlsx in
      let stream, processed = stream_rows yojson_readers ic in
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
    ];
  ]
