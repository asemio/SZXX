open! Core
open Eio.Std

let extractors =
  let unescape = SZXX.Xml.DOM.unescape in
  SZXX.Xlsx.
    {
      string = (fun _location s -> `String (unescape s));
      formula = (fun _location ~formula s -> `String (sprintf !"=(%{unescape}) -> %{unescape}" formula s));
      error =
        (fun _location ~formula s -> `String (sprintf !"#ERROR# =(%{unescape}) -> %{unescape}" formula s));
      boolean = (fun _location s -> `Bool String.(s = "1"));
      number = (fun _location s -> `Float (Float.of_string s));
      date = (fun _location s -> `String s);
      null = `Null;
    }

let get_xlsx_path = sprintf "../../../test/files/%s.xlsx"

let get_json_path = sprintf "../../../test/files/%s.json"

let readme_example1 env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / xlsx_path)) in
  Xlsx.stream_rows_double_pass ~sw file Xlsx.yojson_cell_parser
  |> Sequence.iter ~f:(fun row -> (* Print each row *)
                                  `List row.data |> Yojson.Basic.to_string |> ignore )

let readme_example1b env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  SZXX.Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iter ~f:(fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       `List row.data |> Yojson.Basic.to_string |> ignore )

let readme_example2 env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let open SZXX in
  let filter raw =
    let open Xlsx in
    if raw.row_number > 1000
    then false
    else (
      let row = Expert.parse_row_without_sst yojson_cell_parser raw in
      match List.nth row.data 10 with
      | Some (Available (`Float x)) -> Float.(x >= 100.0)
      | Some (Available _)
       |Some (Delayed _)
       |None ->
        false )
  in
  let seq =
    Xlsx.stream_rows_single_pass
      ~filter_sheets:(fun ~sheet_id ~raw_size:_ -> sheet_id = 1)
      ~sw ~feed:(SZXX.Feed.of_flow src) ~filter Xlsx.yojson_cell_parser
  in
  let count = Sequence.fold seq ~init:0 ~f:(fun acc _row -> acc + 1) in
  if count <> 700 then failwithf "Wrong count: %d" count ()

let readme_example3 env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let open SZXX in
  let count = ref 0 in
  let filter _ =
    incr count;
    false
  in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) ~filter Xlsx.yojson_cell_parser
  in
  (* For good measure let's still drain the Sequence *)
  Sequence.iter seq ~f:(fun _ -> assert false);
  if !count <> 701 then failwithf "Wrong count: %d" !count ()

let single_pass env filename () =
  let xlsx_path = get_xlsx_path filename in
  let json_path = get_json_path filename in
  let against = Eio.Path.(load (Eio.Stdenv.fs env / json_path)) |> Yojson.Safe.from_string in
  let parsed =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX.Xlsx in
    let seq = stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    let json = Sequence.fold seq ~init:[] ~f:(fun acc row -> `List row.data :: acc) in

    (* Test deadlocks: *)
    Sequence.iter seq ~f:(fun _ -> assert false);

    `Assoc [ "data", `List (List.rev json) ]
  in

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.((Eio.Stdenv.fs env) / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against

let double_pass env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  let json_path = get_json_path filename in
  Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / xlsx_path) @@ fun src ->
  Switch.run @@ fun sw ->
  let seq = Xlsx.stream_rows_double_pass ~sw src Xlsx.yojson_cell_parser in
  let parsed =
    `Assoc [ "data", `List (Sequence.map seq ~f:(fun { data; _ } -> `List data) |> Sequence.to_list) ]
  in

  (* Test deadlocks: *)
  Sequence.iter seq ~f:(fun _ -> assert false);

  let against = Eio.Path.(load (Eio.Stdenv.fs env / json_path)) |> Yojson.Safe.from_string in
  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(Eio.Stdenv.fs env / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check parsed against

let sst_from_feed env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / xlsx_path) @@ fun src ->
  let sst = Xlsx.Expert.SST.from_feed (SZXX.Feed.of_flow src) in
  match Xlsx.Expert.SST.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let sst_from_file env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / xlsx_path) @@ fun file ->
  let sst = Xlsx.Expert.SST.from_file file in
  match Xlsx.Expert.SST.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let buffering ~overflow ~max_buffering env filename () =
  let xlsx_path = get_xlsx_path filename in
  try
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX.Xlsx in
    let _seq = stream_rows_single_pass ~max_buffering ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    raise Exit
  with
  | Failure _ when overflow -> ()
  | Exit when not overflow -> ()

let corrupted env filename ~f () =
  try
    f env filename ();
    raise Exit
  with
  | Failure msg when String.is_prefix msg ~prefix:"SZXX: Corrupted file." -> ()

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "SZXX XLSX"
    [
      ( "XLSX",
        [
          "SST from feed", `Quick, sst_from_feed env "financial";
          "SST from file", `Quick, sst_from_file env "financial";
          "simple.xlsx", `Quick, single_pass env "simple";
          "financial.xlsx", `Quick, single_pass env "financial";
          "hybrid.xlsx", `Quick, single_pass env "hybrid";
          "formatting.xlsx", `Quick, single_pass env "formatting";
          "inline.xlsx", `Quick, single_pass env "inline";
          "Readme example 1", `Quick, readme_example1 env "financial";
          "Readme example 1b", `Quick, readme_example1b env "financial";
          "Readme example 2", `Quick, readme_example2 env "financial";
          "Readme example 3", `Quick, readme_example3 env "financial";
          "Double pass", `Quick, double_pass env "financial";
          "Double pass (no SST)", `Quick, double_pass env "inline";
          "Buffering (overflow)", `Quick, buffering env "cols" ~max_buffering:10 ~overflow:true;
          "Buffering (fits)", `Quick, buffering env "simple" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, fits)", `Quick, buffering env "inline" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, overflow)", `Quick, buffering env "inline" ~max_buffering:1 ~overflow:true;
          "Corrupted (double_pass)", `Quick, corrupted env "invalid" ~f:double_pass;
          "Corrupted (sst_from_feed)", `Quick, corrupted env "invalid" ~f:sst_from_feed;
          "Corrupted (sst_from_file)", `Quick, corrupted env "invalid" ~f:sst_from_file;
        ] );
    ]
