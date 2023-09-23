open! Base
open Eio.Std
open SZXX

let string_readers : string Xlsx.cell_parser =
  {
    string = (fun _location s -> s);
    formula = (fun _location ~formula:_ s -> s);
    date = (fun _location s -> s);
    error = (fun _location ~formula:_ s -> s);
    boolean = (fun _location s -> s);
    number = (fun _location s -> s);
    null = "";
  }

let get_time env =
  let mono = Eio.Stdenv.mono_clock env in
  Eio.Time.Mono.now mono |> Mtime.to_uint64_ns

let read env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = get_time env in
  let feed = Feed.of_flow_seekable src in
  let slices = ref 0 in

  let rec loop = function
    | `Eof -> ()
    | `String _ as x ->
      Int.incr slices;
      traceln !"%{Yojson.Basic}" x;
      Int.incr slices;
      loop (feed ())
    | `Bigstring bs ->
      Int.incr slices;
      traceln !"%{Yojson.Basic}" (`String (Bigstringaf.to_string bs));
      loop (feed ())
  in
  loop (feed ());

  let t1 = get_time env in

  traceln "Slice count: %d (%Ldms)\n" !slices Int64.((t1 - t0) / 1_000_000L)

let count env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = get_time env in
  let n = ref 0 in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) Xlsx.string_cell_parser
      ~filter:(fun _ ->
      Int.incr n;
      false )
  in

  Sequence.iter seq ~f:(fun _ -> ());
  let t1 = get_time env in

  traceln "Row count: %d (%Ldms)\n" !n Int64.((t1 - t0) / 1_000_000L)

let count2 env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = get_time env in

  let seq = Xlsx.stream_rows_double_pass ~sw src Xlsx.string_cell_parser in
  let n = Sequence.fold seq ~init:0 ~f:(fun acc _ -> acc + 1) in

  let t1 = get_time env in

  traceln "Row count: %d (%Ldms)\n" n Int64.((t1 - t0) / 1_000_000L)

let length env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let seq = Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (Fn.const Zip.Action.String) in
  Sequence.iter seq ~f:(function
    | ({ filename; _ } : Zip.entry), Zip.Data.String raw ->
      traceln "%s: %d\n" filename (String.length raw)
    | _ -> failwith "Expected Zip.Data.String" )

let extract_sst env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let sink =
    Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
      Eio.Path.(Eio.Stdenv.fs env / Printf.sprintf "%s.sst.xml" xlsx_path)
  in
  let seq =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
      | _ -> Zip.Action.Fast_skip )
  in
  Sequence.iter seq ~f:(function
    | _, Zip.Data.String s -> Eio.Flow.copy_string s sink
    | _ -> () )

let show_json env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) @@ fun w ->
  SZXX.Xlsx.stream_rows_single_pass ~max_buffering:10000 ~sw ~feed:(SZXX.Feed.of_flow src)
    SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iteri ~f:(fun i (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       let s = `List row.data |> Yojson.Basic.pretty_to_string in
       Eio.Buf_write.string w (Printf.sprintf "%d: " (i + 1));
       Eio.Buf_write.string w s;
       Eio.Buf_write.char w '\n' )

let show_json_double_pass env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) @@ fun w ->
  SZXX.Xlsx.stream_rows_double_pass ~sw src SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iteri ~f:(fun i (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       let s = `List row.data |> Yojson.Basic.pretty_to_string in
       Eio.Buf_write.string w (Printf.sprintf "%d: " (i + 1));
       Eio.Buf_write.string w s;
       Eio.Buf_write.char w '\n' )

let count_types env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let string = ref 0 in
  let ss = ref 0 in
  let formula = ref 0 in
  let error = ref 0 in
  let boolean = ref 0 in
  let number = ref 0 in
  let date = ref 0 in
  let cell_parser =
    Xlsx.
      {
        string = (fun _location _s -> Int.incr string);
        formula = (fun _location ~formula:_ _s -> Int.incr formula);
        error = (fun _location ~formula:_ _s -> Int.incr error);
        boolean = (fun _location _s -> Int.incr boolean);
        number = (fun _location _s -> Int.incr number);
        date = (fun _location _s -> Int.incr date);
        null = ();
      }
  in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) cell_parser ~filter:(fun raw ->
      let row = Xlsx.Expert.parse_row_without_sst cell_parser raw in
      List.iter row.data ~f:(function
        | Available _ -> ()
        | Delayed _ -> Int.incr ss );
      false )
  in
  Sequence.iter seq ~f:(fun _ -> assert false);
  traceln "%s\nstring: %d\nshared_string: %d\nformula: %d\nerror: %d\nboolean: %d\nnumber: %d\ndate: %d\n"
    xlsx_path !string !ss !formula !error !boolean !number !date

let count_total_string_length env xlsx_path =
  let t0 = get_time env in
  let num_rows = ref 0 in
  let num_strings = ref 0 in
  let total_length = ref 0 in
  let cell_parser =
    Xlsx.
      {
        string =
          (fun _location s ->
            Int.incr num_strings;
            total_length := !total_length + String.length s);
        formula = (fun _location ~formula:_ _s -> ());
        error = (fun _location ~formula:_ _s -> ());
        boolean = (fun _location _s -> ());
        number = (fun _location _s -> ());
        date = (fun _location _s -> ());
        null = ();
      }
  in
  Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let seq = Xlsx.stream_rows_double_pass ~sw file cell_parser in
  Sequence.iter seq ~f:(fun _row -> Int.incr num_rows);
  let t1 = get_time env in
  traceln
    !"Rows: %{Int#hum}\nStrings: %{Int#hum}\nTotal string length: %{Int#hum}\n%{Int64#hum}ms\n"
    !num_rows !num_strings !total_length
    Int64.((t1 - t0) / 1_000_000L)

let count_tokens env xlsx_path =
  let num_prologue = ref 0 in
  let num_open = ref 0 in
  let num_close = ref 0 in
  let num_text = ref 0 in
  let num_cdata = ref 0 in
  let num_nothing = ref 0 in
  let rec on_parse : Xml.SAX.node -> unit = function
    | Prologue _ -> Int.incr num_prologue
    | Element_open _ -> Int.incr num_open
    | Element_close _ -> Int.incr num_close
    | Text _ -> Int.incr num_text
    | Cdata _ -> Int.incr num_cdata
    | Nothing -> Int.incr num_nothing
    | Many ll -> List.iter ll ~f:on_parse
  in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let files =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } ->
        Zip.Action.Parse Angstrom.(SZXX__Parsing.skip_many (Xml.SAX.parser >>| on_parse))
      | _ -> Zip.Action.Fast_skip )
  in
  Sequence.iter files ~f:(function
    | _, Zip.Data.Parse state -> Zip.Data.parser_state_to_result state |> Result.ok_or_failwith
    | _ -> () );
  traceln
    !"Prologue: %{Int#hum}\n\
      Element open: %{Int#hum}\n\
      Element close: %{Int#hum}\n\
      Text: %{Int#hum}\n\
      Cdata: %{Int#hum}\n\
      Nothing: %{Int#hum}\n"
    !num_prologue !num_open !num_close !num_text !num_cdata !num_nothing

let index env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  traceln !"%{sexp#hum: Zip.entry list}" (Zip.index_entries src)

let () =
  Eio_main.run @@ fun env ->
  Sys.get_argv () |> function
  | [| _; "cat"; file |] -> read env file
  | [| _; "extract_sst"; file |] -> extract_sst env file
  | [| _; "count"; file |] -> count env file
  | [| _; "count2"; file |] -> count2 env file
  | [| _; "length"; file |] -> length env file
  | [| _; "show_json"; file |] -> show_json env file
  | [| _; "show_json2"; file |] -> show_json_double_pass env file
  | [| _; "count_types"; file |] -> count_types env file
  | [| _; "count_length"; file |] -> count_total_string_length env file
  | [| _; "count_tokens"; file |] -> count_tokens env file
  | [| _; "index"; file |] -> index env file
  | _ -> failwith "Invalid arguments passed to cli.exe"
