let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core
open Eio.Std

let extractors =
  SZXX.Xlsx.
    {
      string = (fun _location s -> `String (SZXX.Xml.DOM.unescape s));
      formula =
        (fun _location ~formula s -> `String (sprintf !"=(%s) -> %{SZXX.Xml.DOM.unescape}" formula s));
      error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
      boolean = (fun _location s -> `Bool String.(s = "1"));
      number = (fun _location s -> `Float (Float.of_string s));
      date = (fun _location s -> `String s);
      null = `Null;
    }

let get_xlsx_path = sprintf "../../../test/files/%s.xlsx"

let get_json_path = sprintf "../../../test/files/%s.json"

let readme_example1 env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  SZXX.Xlsx.stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src) SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iter ~f:(fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       `List row.data |> Yojson.Basic.to_string |> print_endline )

let readme_example2 env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let open SZXX.Xlsx in
  let seq =
    stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src)
      ~filter:(fun raw ->
        let row = Expert.parse_row_without_sst yojson_cell_parser raw in
        match List.nth_exn row.data 10 with
        | _ when row.row_number > 1000 -> false
        | Available (`Float x) -> Float.(x >= 100.0)
        | Available _
         |Delayed _ ->
          false)
      yojson_cell_parser
  in
  let count = Sequence.fold seq ~init:0 ~f:(fun acc _row -> acc + 1) in
  if count <> 700 then failwithf "Wrong count: %d" count ()

let xlsx_buffer env filename () =
  let xlsx_path = get_xlsx_path filename in
  let json_path = get_json_path filename in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  let parsed =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
    let open SZXX.Xlsx in
    let seq = stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    let json = Sequence.fold seq ~init:[] ~f:(fun acc row -> `List row.data :: acc) in
    `Assoc [ "data", `List (List.rev json) ]
  in

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(env#fs / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against

let xlsx_double_pass env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  let json_path = get_json_path filename in
  Eio.Path.with_open_in Eio.Path.(env#fs / xlsx_path) @@ fun src ->
  Switch.run @@ fun sw ->
  let stream = Xlsx.stream_rows_double_pass ~sw src Xlsx.yojson_cell_parser in
  let parsed =
    `Assoc
      [
        ( "data",
          `List
            ( Xlsx.to_sequence stream
            |> Sequence.map ~f:(fun { data; _ } -> `List data)
            |> Sequence.to_list ) );
      ]
  in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(env#fs / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check parsed against

let sst_from_zip env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Eio.Path.with_open_in Eio.Path.(env#fs / xlsx_path) @@ fun src ->
  let sst = Xlsx.Expert.SST.from_zip ~feed:(SZXX.Feed.of_flow src) in
  match Xlsx.Expert.SST.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let buffering ~overflow ~max_buffering env filename () =
  let xlsx_path = get_xlsx_path filename in
  try
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
    let open SZXX.Xlsx in
    let _seq = stream_rows_buffer ~max_buffering ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    raise Exit
  with
  | Failure _ when overflow -> ()
  | Exit when not overflow -> ()

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "SZXX XLSX"
    [
      ( "XLSX",
        [
          "SST from ZIP", `Quick, sst_from_zip env "financial";
          "simple.xlsx", `Quick, xlsx_buffer env "simple";
          "financial.xlsx", `Quick, xlsx_buffer env "financial";
          "zip64.xlsx", `Quick, xlsx_buffer env "zip64";
          "formatting.xlsx", `Quick, xlsx_buffer env "formatting";
          "inline.xlsx", `Quick, xlsx_buffer env "inline";
          "Readme example 1", `Quick, readme_example1 env "financial";
          "Readme example 2", `Quick, readme_example2 env "financial";
          "Double pass", `Quick, xlsx_double_pass env "financial";
          "Double pass (no SST)", `Quick, xlsx_double_pass env "inline";
          "Buffering (overflow)", `Quick, buffering env "cols" ~max_buffering:10 ~overflow:true;
          "Buffering (fits)", `Quick, buffering env "simple" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, fits)", `Quick, buffering env "inline" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, overflow)", `Quick, buffering env "inline" ~max_buffering:1 ~overflow:true;
        ] );
    ]
