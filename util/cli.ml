let flags_read = Unix.[ O_RDONLY; O_NONBLOCK ]

let flags_overwrite = Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]

open! Core
open Lwt.Syntax
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

let feed_bigstring ic =
  let open Lwt.Infix in
  let open SZXX.Zip in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })

let count xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
    let t0 = Time_now.nanoseconds_since_unix_epoch () in
    let stream, _sst_p, processed =
      Xlsx.stream_rows_unparsed ~feed:(feed_bigstring ic) ~skip_sst:false ()
    in

    let* n = Lwt_stream.fold (fun _x acc -> acc + 1) stream 0 in
    let t1 = Time_now.nanoseconds_since_unix_epoch () in

    let* () = Lwt_io.printlf "Row count: %d (%Ldms)" n Int63.((t1 - t0) / of_int 1_000_000 |> to_int64) in

    processed )

let length xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
    let stream, p = Zip.stream_files ~feed:(feed_bigstring ic) (const Zip.Action.String) in
    let* () =
      Lwt_stream.iter
        (function
          | ({ filename; _ } : Zip.entry), Zip.Data.String raw ->
            print_endline (sprintf "%s: %d" filename (String.length raw))
          | _ -> failwith "Expected Zip.Data.String")
        stream
    in
    p )

let extract_sst xlsx_path =
  Lwt_io.with_file ~flags:flags_overwrite ~mode:Output (sprintf "%s.sst.xml" xlsx_path) (fun oc ->
    Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let files, files_p =
        Zip.stream_files ~feed:(feed_bigstring ic) (function
          | { filename = "xl/sharedStrings.xml"; _ } -> Zip.Action.String
          | _ -> Zip.Action.Skip )
      in
      let* () =
        Lwt_stream.iter_s
          (function
            | _, Zip.Data.String s -> Lwt_io.write oc s
            | _ -> Lwt.return_unit)
          files
      in
      files_p ) )

let show_json xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
    let stream, success =
      SZXX.Xlsx.stream_rows_buffer ~feed:(feed_bigstring ic) SZXX.Xlsx.yojson_cell_parser
    in
    let processed =
      Lwt_stream.iter_s
        (fun (row : Yojson.Basic.t SZXX.Xlsx.row) ->
          `List (Array.to_list row.data) |> Yojson.Basic.pretty_to_string |> Lwt_io.printl)
        stream
    in
    let* () = success in
    let* () = processed in
    Lwt.return_unit )

let count_types xlsx_path =
  Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
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
    let stream, _sst_p, success = Xlsx.stream_rows ~feed:(feed_bigstring ic) cell_parser in
    let processed =
      Lwt_stream.iter
        (fun Xlsx.{ data; _ } ->
          Array.iter data ~f:(function
            | Xlsx.Available _ -> ()
            | Delayed _ -> incr ss ))
        stream
    in
    let* () = success in
    let* () = processed in
    print_endline
      (sprintf
         "%s\nstring: %d\nshared_string: %d\nformula: %d\nerror: %d\nboolean: %d\nnumber: %d\ndate: %d"
         xlsx_path !string !ss !formula !error !boolean !number !date );
    Lwt.return_unit )

let count_total_string_length xlsx_path =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let* sst =
    Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      Xlsx.SST.from_zip ~feed:(feed_bigstring ic) )
  in
  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  print_endline
    (sprintf !"%s" (Int63.(t1 - t0 |> to_float) |> Time.Span.of_ns |> Time.Span.to_string_hum));
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
  let* () =
    Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let stream, _sst, success = Xlsx.stream_rows_unparsed ~skip_sst:true ~feed:(feed_bigstring ic) () in
      let processed =
        Lwt_stream.iter
          (fun el ->
            let _row = Xlsx.parse_row_with_sst sst cell_parser el in
            incr num_rows)
          stream
      in
      let* () = success in
      processed )
  in
  print_endline
    (sprintf
       !"Rows: %d\nStrings: %d\nTotal string length: %{Int.to_string_hum}"
       !num_rows !num_strings !total_length );
  Lwt.return_unit

let count_tokens xlsx_path =
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
  let* () =
    Lwt_io.with_file ~flags:flags_read ~mode:Input xlsx_path (fun ic ->
      let files, success =
        Zip.stream_files ~feed:(feed_bigstring ic) (function
          | { filename = "xl/sharedStrings.xml"; _ } ->
            Zip.Action.Parse Angstrom.(skip_many (Xml.parser >>| on_parse))
          | _ -> Zip.Action.Skip )
      in
      let p =
        Lwt_stream.iter
          (function
            | _, Zip.Data.Parse result -> Result.ok_or_failwith result
            | _ -> ())
          files
      in
      let* () = success in
      p )
  in
  print_endline
    (sprintf
       !"Prologue: %{Int#hum}\n\
         Element open: %{Int#hum}\n\
         Element close: %{Int#hum}\n\
         Text: %{Int#hum}\n\
         Cdata: %{Int#hum}\n\
         Nothing: %{Int#hum}"
       !num_prologue !num_open !num_close !num_text !num_cdata !num_nothing );
  Lwt.return_unit

let () =
  Sys.get_argv () |> function
  | [| _; "extract_sst"; file |] -> Lwt_main.run (extract_sst file)
  | [| _; "count"; file |] -> Lwt_main.run (count file)
  | [| _; "length"; file |] -> Lwt_main.run (length file)
  | [| _; "show_json"; file |] -> Lwt_main.run (show_json file)
  | [| _; "count_types"; file |] -> Lwt_main.run (count_types file)
  | [| _; "count_length"; file |] -> Lwt_main.run (count_total_string_length file)
  | [| _; "count_tokens"; file |] -> Lwt_main.run (count_tokens file)
  | _ -> failwith "Invalid arguments"
