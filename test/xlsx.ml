open! Base
open Eio.Std

exception Exit = Stdlib.Exit

let extractors =
  let unescape = SZXX.Xml.DOM.unescape in
  SZXX.Xlsx.
    {
      string = (fun _location s -> `String (unescape s));
      formula =
        (fun _location ~formula s -> `String (Printf.sprintf !"=(%{unescape}) -> %{unescape}" formula s));
      error =
        (fun _location ~formula s ->
          `String (Printf.sprintf !"#ERROR# =(%{unescape}) -> %{unescape}" formula s));
      boolean = (fun _location s -> `Bool String.(s = "1"));
      number = (fun _location s -> `Float (Float.of_string s));
      date = (fun _location s -> `String s);
      null = `Null;
    }

let get_xlsx_path = Printf.sprintf "../../../test/files/%s.xlsx"

let get_json_path = Printf.sprintf "../../../test/files/%s.json"

let readme_example1 env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / xlsx_path)) in
  Xlsx.stream_rows_double_pass ~sw file Xlsx.yojson_cell_parser
  |> Sequence.iter ~f:(fun (row : Yojson.Basic.t Xlsx.row) ->
       (* Print each row *)
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
  if count <> 700 then Printf.failwithf "Wrong count: %d" count ()

let readme_example3 env filename () =
  let xlsx_path = get_xlsx_path filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let open SZXX in
  let count = ref 0 in
  let filter _ =
    Int.incr count;
    false
  in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) ~filter Xlsx.yojson_cell_parser
  in
  (* For good measure let's still drain the Sequence *)
  Sequence.iter seq ~f:(fun _ -> assert false);
  if !count <> 701 then Printf.failwithf "Wrong count: %d" !count ()

let single_pass env filename () =
  let xlsx_path = get_xlsx_path filename in
  let json_path = get_json_path filename in
  let parsed =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX.Xlsx in
    let seq = stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    let json = Sequence.fold seq ~init:[] ~f:(fun acc row -> `List row.data :: acc) in

    (* Test deadlocks: *)
    (try Sequence.iter seq ~f:(fun _ -> raise Exit) with
    | Invalid_argument _ -> ()
    | _ -> failwith "Unexpected exception from reusing Sequence");

    `Assoc [ "data", `List (List.rev json) ]
  in

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.((Eio.Stdenv.fs env) / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  let against = Eio.Path.(load (Eio.Stdenv.fs env / json_path)) |> Yojson.Safe.from_string in
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
  (try Sequence.iter seq ~f:(fun _ -> raise Exit) with
  | Invalid_argument _ -> ()
  | _ -> failwith "Unexpected exception from reusing Sequence");

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(Eio.Stdenv.fs env / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  let against = Eio.Path.(load (Eio.Stdenv.fs env / json_path)) |> Yojson.Safe.from_string in
  Json_diff.check parsed against

let read_after_switch_single env filename () =
  let xlsx_path = get_xlsx_path filename in
  let count = ref 0 in
  let seq =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX in
    let filter _ =
      Int.incr count;
      false
    in
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) ~filter Xlsx.yojson_cell_parser
  in
  try Sequence.iter seq ~f:(fun _ -> raise Exit) with
  | Invalid_argument _ -> ()
  | _ -> failwith "Unexpected exception from reusing Sequence"

let read_after_switch_double env filename () =
  let xlsx_path = get_xlsx_path filename in
  let seq =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX in
    Xlsx.stream_rows_double_pass ~sw src Xlsx.yojson_cell_parser
  in
  try Sequence.iter seq ~f:(fun _ -> raise Exit) with
  | Invalid_argument _ -> ()
  | _ -> failwith "Unexpected exception from reusing Sequence"

let sst_from_feed env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / xlsx_path) @@ fun src ->
  let sst = Xlsx.Expert.SST.from_feed (SZXX.Feed.of_flow src) in
  match Xlsx.Expert.SST.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> Printf.failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let sst_from_file env filename () =
  let open SZXX in
  let xlsx_path = get_xlsx_path filename in
  Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / xlsx_path) @@ fun file ->
  let sst = Xlsx.Expert.SST.from_file file in
  match Xlsx.Expert.SST.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> Printf.failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let buffering ~overflow ~max_buffering env filename () =
  let xlsx_path = get_xlsx_path filename in
  try
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
    let open SZXX.Xlsx in
    let seq = stream_rows_single_pass ~max_buffering ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    Sequence.iter seq ~f:(fun _ -> ());
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

let parse_datetime () =
  let dt = SZXX.Xlsx.parse_datetime 32643.4388657407 in
  let s = Ptime.to_rfc3339 dt in
  let expected = "1989-05-15T10:31:57-00:00" in
  if String.( <> ) s expected then Printf.failwithf "%s <> %s" s expected ()

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
          "spaces.xlsx", `Quick, single_pass env "spaces";
          "Readme example 1", `Quick, readme_example1 env "financial";
          "Readme example 1b", `Quick, readme_example1b env "financial";
          "Readme example 2", `Quick, readme_example2 env "financial";
          "Readme example 3", `Quick, readme_example3 env "financial";
          "Read after Switch (single)", `Quick, read_after_switch_single env "financial";
          "Read after Switch (double)", `Quick, read_after_switch_double env "financial";
          "Double pass", `Quick, double_pass env "financial";
          "Double pass (no SST)", `Quick, double_pass env "inline";
          "Buffering (overflow)", `Quick, buffering env "cols" ~max_buffering:10 ~overflow:true;
          "Buffering (fits)", `Quick, buffering env "simple" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, fits)", `Quick, buffering env "inline" ~max_buffering:10 ~overflow:false;
          "Buffering (no SST, overflow)", `Quick, buffering env "inline" ~max_buffering:0 ~overflow:true;
          "Corrupted (double_pass)", `Quick, corrupted env "invalid" ~f:double_pass;
          "Corrupted (sst_from_feed)", `Quick, corrupted env "invalid" ~f:sst_from_feed;
          "Corrupted (sst_from_file)", `Quick, corrupted env "invalid" ~f:sst_from_file;
          "parse_datetime", `Quick, parse_datetime;
        ] );
    ]
