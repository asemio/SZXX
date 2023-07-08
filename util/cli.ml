open! Core
open Eio.Std
open SZXX

let string_readers : string Xlsx.cell_parser =
  {
    string = (fun _location s -> s);
    formula = (fun _location ~formula:_ s -> s);
    date = (fun _location s -> s);
    error = (fun _location s -> s);
    boolean = (fun _location s -> s);
    number = (fun _location s -> s);
    null = "";
  }

let count env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let stream, _sst_p = Xlsx.stream_rows_unparsed ~sw ~feed:(SZXX.Feed.of_flow src) ~skip_sst:true () in

  let n = Xlsx.to_seq stream |> Seq.fold_left (fun acc _x -> acc + 1) 0 in
  let t1 = Time_now.nanoseconds_since_unix_epoch () in

  Eio.Flow.copy_string
    (sprintf "Row count: %d (%Ldms)\n" n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64))
    env#stdout

let length env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let stream = Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (const Zip.Action.String) in
  Xlsx.to_seq stream
  |> Seq.iter (function
       | ({ filename; _ } : Zip.entry), Zip.Data.String raw ->
         Eio.Flow.copy_string (sprintf "%s: %d\n" filename (String.length raw)) env#stdout
       | _ -> failwith "Expected Zip.Data.String" )

let extract_sst env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let sink =
    Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / sprintf "%s.sst.xml" xlsx_path)
  in
  let stream =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
      | _ -> Zip.Action.Skip )
  in
  Xlsx.to_seq stream
  |> Seq.iter (function
       | _, Zip.Data.String s -> Eio.Flow.copy_string s sink
       | _ -> () )

let show_json env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  Eio.Buf_write.with_flow env#stdout @@ fun w ->
  SZXX.Xlsx.stream_rows_buffer ~sw ~feed:(SZXX.Feed.of_flow src) SZXX.Xlsx.yojson_cell_parser
  |> Seq.iter (fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
       let s = `List (Array.to_list row.data) |> Yojson.Basic.pretty_to_string in
       Eio.Buf_write.string w s;
       Eio.Buf_write.char w '\n' )

let count_types env xlsx_path =
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
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
        error = (fun _location _s -> incr error);
        boolean = (fun _location _s -> incr boolean);
        number = (fun _location _s -> incr number);
        date = (fun _location _s -> incr date);
        null = ();
      }
  in
  let stream, _sst_p = Xlsx.stream_rows ~sw ~feed:(SZXX.Feed.of_flow src) cell_parser in
  Xlsx.to_seq stream
  |> Seq.iter (fun Xlsx.{ data; _ } ->
       Array.iter data ~f:(function
         | Xlsx.Available _ -> ()
         | Delayed _ -> incr ss ) );
  Eio.Flow.copy_string
    (sprintf
       "%s\nstring: %d\nshared_string: %d\nformula: %d\nerror: %d\nboolean: %d\nnumber: %d\ndate: %d\n"
       xlsx_path !string !ss !formula !error !boolean !number !date )
    env#stdout

let count_total_string_length env xlsx_path =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let sst =
    Eio.Path.with_open_in Eio.Path.(env#fs / xlsx_path) @@ fun src ->
    Xlsx.SST.from_zip ~feed:(SZXX.Feed.of_flow src)
  in
  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  Eio.Flow.copy_string
    (sprintf
       !"Parse SST: %{Time_float.Span#hum}\n"
       (Int63.(t1 - t0 |> to_float) |> Time_float.Span.of_ns) )
    env#stdout;
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
        error = (fun _location _s -> ());
        boolean = (fun _location _s -> ());
        number = (fun _location _s -> ());
        date = (fun _location _s -> ());
        null = ();
      }
  in
  Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let stream, _sst = Xlsx.stream_rows_unparsed ~sw ~skip_sst:true ~feed:(SZXX.Feed.of_flow src) () in
  Xlsx.to_seq stream
  |> Seq.iter (fun el ->
       let _row = Xlsx.parse_row_with_sst sst cell_parser el in
       incr num_rows );
  let t2 = Time_now.nanoseconds_since_unix_epoch () in
  Eio.Flow.copy_string
    (sprintf
       !"Rows: %{Int#hum}\nStrings: %{Int#hum}\nTotal string length: %{Int#hum}\n%{Time_float.Span#hum}\n"
       !num_rows !num_strings !total_length
       (Int63.(t2 - t0 |> to_float) |> Time_float.Span.of_ns) )
    env#stdout

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
  let src = Eio.Path.open_in ~sw Eio.Path.(env#fs / xlsx_path) in
  let files =
    Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (function
      | { filename = "xl/sharedStrings.xml"; _ } ->
        Zip.Action.Parse Angstrom.(SZXX__Parsing.skip_many (Xml.parser >>| on_parse))
      | _ -> Zip.Action.Skip )
  in
  Xlsx.to_seq files
  |> Seq.iter (function
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
    env#stdout

let () =
  Eio_main.run @@ fun env ->
  Sys.get_argv () |> function
  | [| _; "extract_sst"; file |] -> extract_sst env file
  | [| _; "count"; file |] -> count env file
  | [| _; "length"; file |] -> length env file
  | [| _; "show_json"; file |] -> show_json env file
  | [| _; "count_types"; file |] -> count_types env file
  | [| _; "count_length"; file |] -> count_total_string_length env file
  | [| _; "count_tokens"; file |] -> count_tokens env file
  | _ -> failwith "Invalid arguments"
