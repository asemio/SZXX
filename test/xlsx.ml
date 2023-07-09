let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core
open Eio.Std

let extractors =
  SZXX.Xlsx.
    {
      string = (fun _location s -> `String (SZXX.Xml.unescape s));
      formula = (fun _location ~formula s -> `String (sprintf !"=(%s) -> %{SZXX.Xml.unescape}" formula s));
      error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
      boolean = (fun _location s -> `Bool String.(s = "1"));
      number = (fun _location s -> `Float (Float.of_string s));
      date = (fun _location s -> `String s);
      null = `Null;
    }

let readme_example1 env filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  (* yojson_cell_parser is an easy way to quickly inspect a file by mapping XLSX's data types to JSON *)
  let seq = SZXX.Xlsx.stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src) SZXX.Xlsx.yojson_cell_parser in
  Seq.iter
    (fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
      `List row.data |> Yojson.Basic.to_string |> print_endline)
    seq
(* bind to/await the `success` promise to catch any error that may have terminated the stream early *)
(* ... *)

let readme_example2 env filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let open SZXX.Xlsx in
  let stream, sst_p = stream_rows ~sw ~feed:(SZXX.Feed.of_flow src) yojson_cell_parser in
  let get_filtered () =
    to_seq stream
    |> Seq.filter (fun row ->
         match List.nth_exn row.data 3 with
         | _ when row.row_number > 1000 -> false
         | Available (`Bool x) -> x
         | Available _
          |Delayed _ ->
           false )
    |> Sequence.of_seq
    (* TODO *)
    |> Sequence.force_eagerly
  in
  let filtered, sst = Fiber.pair get_filtered (fun () -> Promise.await_exn sst_p) in
  let count =
    Sequence.fold filtered ~init:0 ~f:(fun acc row ->
      let _fully_available_row = unwrap_status yojson_cell_parser sst row in
      (* do something with [fully_available_row] *)
      acc + 1 )
  in
  print_endline (sprintf "Count: %d" count)

let xlsx env filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = sprintf "../../../test/files/%s.json" filename in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  let parsed =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
    let open SZXX.Xlsx in
    let seq = stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src) extractors in
    let json = Seq.fold_left (fun acc row -> `List row.data :: acc) [] seq in
    `Assoc [ "data", `List (List.rev json) ]
  in

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(env#fs / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against

let xlsx_unparsed env filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = sprintf "../../../test/files/%s.json" filename in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  let parsed =
    Switch.run @@ fun sw ->
    let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
    let open SZXX.Xlsx in
    let stream, sst_p = stream_rows_unparsed ~sw ~feed:(SZXX.Feed.of_flow src) () in
    let parsed =
      with_minimal_buffering stream sst_p ~parse:(fun ~sst element ->
        let row = parse_row_with_sst sst yojson_cell_parser element in
        `List row.data )
    in
    `Assoc [ "data", `List (Sequence.of_seq parsed |> Sequence.to_list) ]
  in

  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(env#fs / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check (parsed : Yojson.Basic.t :> Yojson.Safe.t) against

let stream_rows env filename () =
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  let json_path = "../../../test/files/streamed.json" in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let parsed =
    let open SZXX.Xlsx in
    let stream, sst_p = stream_rows ~sw ~feed:(SZXX.Feed.of_flow src) yojson_cell_parser in
    let seq = to_seq stream in
    let unparsed = Queue.create () in
    let has_more =
      Fiber.first
        (fun () ->
          Seq.iter
            (fun status ->
              Queue.enqueue unparsed status;
              Fiber.yield ())
            seq;
          false)
        (fun () ->
          let _sst = Promise.await_exn sst_p in
          true)
    in
    let sst = Promise.await_exn sst_p in
    let parse status =
      let row = unwrap_status yojson_cell_parser sst status in
      `List row.data
    in
    let q = Queue.map unparsed ~f:parse in
    if has_more then Seq.iter (fun status -> parse status |> Queue.enqueue q) seq;
    `Assoc [ "data", `List (Queue.to_list q) ]
  in
  (* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
     Eio.Path.(env#fs / json_path)
     (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)); *)
  Json_diff.check parsed against

let sst_from_zip env filename () =
  let open SZXX in
  let xlsx_path = sprintf "../../../test/files/%s.xlsx" filename in
  Eio.Path.with_open_in Eio.Path.(env#fs / xlsx_path) @@ fun src ->
  let sst = Xlsx.SST.from_zip ~feed:(SZXX.Feed.of_flow src) in
  match Xlsx.resolve_sst_index sst ~sst_index:"30" with
  | Some "October" -> ()
  | Some x -> failwithf "Invalid SST index 30: '%s'" x ()
  | None -> failwith "Failed to resolve SST index 30"

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "SZXX XLSX"
    [
      ( "XLSX",
        [
          "SST from ZIP", `Quick, sst_from_zip env "financial";
          "simple.xlsx", `Quick, xlsx env "simple";
          "financial.xlsx", `Quick, xlsx_unparsed env "financial";
          "zip64.xlsx", `Quick, xlsx env "zip64";
          "formatting.xlsx", `Quick, xlsx env "formatting";
          "inline.xlsx", `Quick, xlsx env "inline";
          "Readme example 1", `Quick, readme_example1 env "financial";
          "Readme example 2", `Quick, readme_example2 env "financial";
          "Unbuffed stream", `Quick, stream_rows env "financial";
        ] );
    ]
