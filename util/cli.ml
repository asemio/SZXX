open! Core
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

let read env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let feed = Feed.of_flow_seekable src in
  let slices = ref 0 in

  let rec loop = function
    | `Eof -> ()
    | `String _ as x ->
      incr slices;
      Eio.Flow.copy_string (sprintf !"%{Yojson.Basic}" x) (Eio.Stdenv.stdout env);
      incr slices;
      loop (feed ())
    | `Bigstring bs ->
      incr slices;
      Eio.Flow.copy_string
        (sprintf !"%{Yojson.Basic}" (`String (Bigstring.to_string bs)))
        (Eio.Stdenv.stdout env);
      loop (feed ())
  in
  loop (feed ());

  let t1 = Time_now.nanoseconds_since_unix_epoch () in

  Eio.Flow.copy_string
    (sprintf "Slice count: %d (%Ldms)\n" !slices Int63.((t1 - t0) / of_int 1_000_000 |> to_int64))
    (Eio.Stdenv.stdout env)

let count env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let n = ref 0 in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) Xlsx.string_cell_parser
      ~filter:(fun _ ->
      incr n;
      false )
  in
  Sequence.iter seq ~f:(fun _ -> assert false);

  let t1 = Time_now.nanoseconds_since_unix_epoch () in

  Eio.Flow.copy_string
    (sprintf "Row count: %d (%Ldms)\n" !n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64))
    (Eio.Stdenv.stdout env)

let count2 env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let t0 = Time_now.nanoseconds_since_unix_epoch () in

  let seq = Xlsx.stream_rows_double_pass ~sw src Xlsx.string_cell_parser in
  let n = Sequence.fold seq ~init:0 ~f:(fun acc _ -> acc + 1) in

  let t1 = Time_now.nanoseconds_since_unix_epoch () in

  Eio.Flow.copy_string
    (sprintf "Row count: %d (%Ldms)\n" n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64))
    (Eio.Stdenv.stdout env)

let length env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let seq = Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (const Zip.Action.String) in
  Sequence.iter seq ~f:(function
    | ({ filename; _ } : Zip.entry), Zip.Data.String raw ->
      Eio.Flow.copy_string (sprintf "%s: %d\n" filename (String.length raw)) (Eio.Stdenv.stdout env)
    | _ -> failwith "Expected Zip.Data.String" )

let extract_sst env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let sink =
    Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
      Eio.Path.(Eio.Stdenv.fs env / sprintf "%s.sst.xml" xlsx_path)
  in
  let seq =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
      | _ -> Zip.Action.Skip )
  in
  Sequence.iter seq ~f:(function
    | _, Zip.Data.String s -> Eio.Flow.copy_string s sink
    | _ -> () )

let show_json env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) @@ fun w ->
  SZXX.Xlsx.stream_rows_single_pass ~max_buffering:1000 ~sw ~feed:(SZXX.Feed.of_flow src)
    SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iteri ~f:(fun i (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       let s = `List row.data |> Yojson.Basic.pretty_to_string in
       Eio.Buf_write.string w (sprintf "%d: " (i + 1));
       Eio.Buf_write.string w s;
       Eio.Buf_write.char w '\n' )

let show_json_double_pass env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) @@ fun w ->
  SZXX.Xlsx.stream_rows_double_pass ~sw src SZXX.Xlsx.yojson_cell_parser
  |> Sequence.iteri ~f:(fun i (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       let s = `List row.data |> Yojson.Basic.pretty_to_string in
       Eio.Buf_write.string w (sprintf "%d: " (i + 1));
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
        string = (fun _location _s -> incr string);
        formula = (fun _location ~formula:_ _s -> incr formula);
        error = (fun _location ~formula:_ _s -> incr error);
        boolean = (fun _location _s -> incr boolean);
        number = (fun _location _s -> incr number);
        date = (fun _location _s -> incr date);
        null = ();
      }
  in
  let seq =
    Xlsx.stream_rows_single_pass ~sw ~feed:(SZXX.Feed.of_flow src) cell_parser ~filter:(fun raw ->
      let row = Xlsx.Expert.parse_row_without_sst cell_parser raw in
      List.iter row.data ~f:(function
        | Available _ -> ()
        | Delayed _ -> incr ss );
      false )
  in
  Sequence.iter seq ~f:(fun _ -> ());
  Eio.Flow.copy_string
    (sprintf
       "%s\nstring: %d\nshared_string: %d\nformula: %d\nerror: %d\nboolean: %d\nnumber: %d\ndate: %d\n"
       xlsx_path !string !ss !formula !error !boolean !number !date )
    (Eio.Stdenv.stdout env)

let count_total_string_length env xlsx_path =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let num_rows = ref 0 in
  let num_strings = ref 0 in
  let total_length = ref 0 in
  let cell_parser =
    Xlsx.
      {
        string =
          (fun _location s ->
            incr num_strings;
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
  Sequence.iter seq ~f:(fun _row -> incr num_rows);
  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  Eio.Flow.copy_string
    (sprintf
       !"Rows: %{Int#hum}\nStrings: %{Int#hum}\nTotal string length: %{Int#hum}\n%{Time_float.Span#hum}\n"
       !num_rows !num_strings !total_length
       (Int63.(t1 - t0 |> to_float) |> Time_float.Span.of_ns) )
    (Eio.Stdenv.stdout env)

let count_tokens env xlsx_path =
  let num_prologue = ref 0 in
  let num_open = ref 0 in
  let num_close = ref 0 in
  let num_text = ref 0 in
  let num_cdata = ref 0 in
  let num_nothing = ref 0 in
  let rec on_parse : Xml.SAX.node -> unit = function
    | Prologue _ -> incr num_prologue
    | Element_open _ -> incr num_open
    | Element_close _ -> incr num_close
    | Text _ -> incr num_text
    | Cdata _ -> incr num_cdata
    | Nothing -> incr num_nothing
    | Many ll -> List.iter ll ~f:on_parse
  in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(Eio.Stdenv.fs env / xlsx_path) in
  let files =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } ->
        Zip.Action.Parse Angstrom.(SZXX__Parsing.skip_many (Xml.SAX.parser >>| on_parse))
      | _ -> Zip.Action.Skip )
  in
  Sequence.iter files ~f:(function
    | _, Zip.Data.Parse state -> Zip.Data.parser_state_to_result state |> Result.ok_or_failwith
    | _ -> () );
  Eio.Flow.copy_string
    (sprintf
       !"Prologue: %{Int#hum}\n\
         Element open: %{Int#hum}\n\
         Element close: %{Int#hum}\n\
         Text: %{Int#hum}\n\
         Cdata: %{Int#hum}\n\
         Nothing: %{Int#hum}\n"
       !num_prologue !num_open !num_close !num_text !num_cdata !num_nothing )
    (Eio.Stdenv.stdout env)

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
  | _ -> failwith "Invalid arguments"
