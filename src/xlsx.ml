open! Core
open Eio.Std

type location = {
  sheet_number: int;
  row_number: int;
  col_index: int;
}
[@@deriving sexp_of]

type 'a cell_parser = {
  string: location -> string -> 'a;
  formula: location -> formula:string -> string -> 'a;
  error: location -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
  date: location -> string -> 'a;
  null: 'a;
}

type delayed_string = {
  location: location;
  sst_index: string;
}
[@@deriving sexp_of]

type 'a status =
  | Available of 'a
  | Delayed of delayed_string
[@@deriving sexp_of]

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a list;
}
[@@deriving sexp_of]

let origin = Date.add_days (Date.create_exn ~y:1900 ~m:(Month.of_int_exn 1) ~d:1) (-2)

let parse_date f = Float.to_int f |> Date.add_days origin

let parse_datetime ~zone f =
  let parts = Float.modf f in
  let date = Float.Parts.integral parts |> Float.to_int |> Date.add_days origin in
  let frac = Float.(Parts.fractional parts * 86400000. |> round) |> Time_float.Span.of_ms in
  let ofday = Time_float.Ofday.of_span_since_start_of_day_exn frac in
  Time_float.of_date_ofday ~zone date ofday

let xml_parser_options =
  Xml.SAX.
    {
      accept_html_boolean_attributes = false;
      accept_unquoted_attributes = false;
      accept_single_quoted_attributes = false;
    }

let xml_parser = Xml.SAX.make_parser xml_parser_options

let fold_angstrom ~filter_path ~on_match () =
  let sax = ref (Ok Xml.SAX.Expert.Stream.init) in
  let on_parse node = sax := Xml.SAX.Expert.Stream.folder ~filter_path ~on_match !sax node in
  Zip.Action.Parse_many { parser = xml_parser; on_parse }

let parse_sheet ~sheet_number push =
  let num = ref 0 in
  let on_match (el : Xml.DOM.element) =
    (match Xml.DOM.get_attr el.attrs "r" with
    | None -> incr num
    | Some s -> (
      try
        let i = Int.of_string s in
        (* Insert blank rows *)
        for row_number = !num to i - 2 do
          push { sheet_number; row_number; data = [] }
        done;
        num := i
      with
      | _ -> incr num ));
    push { sheet_number; row_number = !num; data = el.children }
  in
  fold_angstrom ~filter_path:[ "worksheet"; "sheetData"; "row" ] ~on_match ()

let parse_string_cell el =
  let open Xml.DOM in
  match el |> dot "t" with
  | Some { text; _ } -> text
  | None -> filter_map "r" el ~f:(dot_text "t") |> String.concat

let to_seq stream = Seq.of_dispenser (fun () -> Eio.Stream.take stream)

let flush_zip_stream zip_stream =
  to_seq zip_stream
  |> Seq.iter (function
       | ( (_ : Zip.entry),
           Zip.Data.(Skip | String _ | Bigstring _ | Fold_string _ | Fold_bigstring _ | Parse _) ) ->
         ()
       | { filename; _ }, Parse_many state -> (
         match Zip.Data.parser_state_to_result state with
         | Ok x -> x
         | Error msg -> failwithf "SZXX: File '%s': %s" filename msg () ) )

module SST = struct
  type t = string Lazy.t array

  let filter_path = [ "sst"; "si" ]

  let zip_entry_filename = "xl/sharedStrings.xml"

  let from_zip ~feed =
    let q =
      Switch.run @@ fun sw ->
      let q = Queue.create () in
      let zip_stream =
        Zip.stream_files ~sw ~feed (function
          | { filename = "xl/sharedStrings.xml"; _ } ->
            let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
            fold_angstrom ~filter_path ~on_match ()
          | _ -> Zip.Action.Skip )
      in
      flush_zip_stream zip_stream;
      q
    in

    Queue.to_array q
end

let process_file ?only_sheet ~skip_sst ~sw ~feed push finalize =
  let q = Queue.create () in
  let zip_stream =
    Zip.stream_files ~sw ~feed (function
      | { filename = "xl/workbook.xml"; _ } -> Skip
      | { filename = "xl/sharedStrings.xml"; _ } when skip_sst -> Skip
      | { filename = "xl/sharedStrings.xml"; _ } ->
        let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
        fold_angstrom ~filter_path:SST.filter_path ~on_match ()
      | { filename; _ } ->
        let open Option.Monad_infix in
        String.chop_prefix ~prefix:"xl/worksheets/sheet" filename
        >>= String.chop_suffix ~suffix:".xml"
        >>= (fun s -> Option.try_with (fun () -> Int.of_string s))
        |> Option.filter ~f:(fun i -> Option.value_map only_sheet ~default:true ~f:(( = ) i))
        >>| (fun sheet_number -> parse_sheet ~sheet_number push)
        |> Option.value ~default:Zip.Action.Skip )
  in
  Fiber.fork_promise ~sw (fun () ->
    (* [flush_zip_stream] fails the Switch if it encounters an error
       so we don't need it to be in a try/catch to always call [finalize ()] *)
    flush_zip_stream zip_stream;
    finalize ();
    Queue.to_array q )

let resolve_sst_index (sst : SST.t) ~sst_index =
  let index = Int.of_string sst_index in
  match sst with
  | sst when index < Array.length sst && index >= 0 -> Some (force sst.(index))
  | _ -> None

let unwrap_status cell_parser (sst : SST.t) (row : 'a status row) =
  let data =
    List.map row.data ~f:(function
      | Available x -> x
      | Delayed { location; sst_index } -> (
        match resolve_sst_index sst ~sst_index with
        | Some index -> cell_parser.string location index
        | None -> cell_parser.null ) )
  in
  { row with data }

let extract_cell_sst, extract_cell_status =
  let open Xml.DOM in
  let extract ~null location extractor : element option -> 'a = function
    | None -> null
    | Some { text; _ } -> extractor location text
  in
  let extract_cell_base { string; formula; error; boolean; number; date; null } location el ty =
    match ty with
    | None
     |Some "n" ->
      el |> dot "v" |> extract ~null location number
    | Some "d" -> el |> dot "v" |> extract ~null location date
    | Some "str" -> (
      match el |> dot_text "v" with
      | None -> null
      | Some s -> formula location s ~formula:(el |> dot_text "f" |> Option.value ~default:"") )
    | Some "inlineStr" -> (
      match dot "is" el with
      | None -> null
      | Some el -> string location (parse_string_cell el) )
    | Some "e" -> el |> dot "v" |> extract ~null location error
    | Some "b" -> el |> dot "v" |> extract ~null location boolean
    | Some t -> failwithf "Unknown data type: %s ::: %s" t (sexp_of_element el |> Sexp.to_string) ()
  in
  let extract_cell_sst sst cell_parser location el =
    match Xml.DOM.get_attr el.attrs "t" with
    | Some "s" -> (
      match el |> dot "v" with
      | None -> cell_parser.null
      | Some { text = sst_index; _ } -> (
        match resolve_sst_index sst ~sst_index with
        | None -> cell_parser.null
        | Some resolved -> cell_parser.string location resolved ) )
    | ty -> extract_cell_base cell_parser location el ty
  in
  let extract_cell_status cell_parser location el =
    match Xml.DOM.get_attr el.attrs "t" with
    | Some "s" -> (
      match el |> dot "v" with
      | None -> Available cell_parser.null
      | Some { text = sst_index; _ } -> Delayed { location; sst_index } )
    | ty -> Available (extract_cell_base cell_parser location el ty)
  in
  extract_cell_sst, extract_cell_status

let index_of_column s =
  let key =
    String.take_while s ~f:(function
      | 'A' .. 'Z' -> true
      | _ -> false )
  in
  String.fold key ~init:0 ~f:(fun acc c -> (acc * 26) + Char.to_int c - 64) - 1

let parse_row_with_sst sst cell_parser ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  match data with
  | [] -> { row with data = [] }
  | _ ->
    let rec loop i acc = function
      | [] -> List.rev acc
      | el :: rest ->
        let col_index = Xml.DOM.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column in
        let v = extract_cell_sst sst cell_parser { col_index; sheet_number; row_number } el in
        let acc = Fn.apply_n_times ~n:(col_index - i) (List.cons cell_parser.null) acc in
        (loop [@tailcall]) (col_index + 1) (v :: acc) rest
    in
    { row with data = loop 0 [] data }

let parse_row_without_sst cell_parser ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  match data with
  | [] -> { row with data = [] }
  | _ ->
    let rec loop i acc = function
      | [] -> List.rev acc
      | el :: rest ->
        let col_index = Xml.DOM.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column in
        let v = extract_cell_status cell_parser { col_index; sheet_number; row_number } el in
        let acc = Fn.apply_n_times ~n:(col_index - i) (List.cons (Available cell_parser.null)) acc in
        (loop [@tailcall]) (col_index + 1) (v :: acc) rest
    in
    { row with data = loop 0 [] data }

let stream_rows ?only_sheet ?(skip_sst = false) ~sw ~feed cell_parser =
  let stream = Eio.Stream.create 0 in
  let push x = Eio.Stream.add stream (Some (parse_row_without_sst cell_parser x)) in
  let finalize () = Eio.Stream.add stream None in

  let sst_p = process_file ?only_sheet ~skip_sst ~sw ~feed push finalize in
  stream, sst_p

let stream_rows_unparsed ?only_sheet ?(skip_sst = false) ~sw ~feed () =
  let stream = Eio.Stream.create 0 in
  let push x = Eio.Stream.add stream (Some x) in
  let finalize () = Eio.Stream.add stream None in

  let sst_p = process_file ?only_sheet ~skip_sst ~sw ~feed push finalize in
  stream, sst_p

let with_minimal_buffering stream sst_p ~parse =
  let seq = to_seq stream in
  let unparsed = Queue.create () in
  let has_more =
    Fiber.first
      (fun () ->
        Seq.iter
          (fun x ->
            Queue.enqueue unparsed x;
            Fiber.yield ())
          seq;
        false)
      (fun () ->
        let _sst = Promise.await_exn sst_p in
        true)
  in
  let sst = Promise.await_exn sst_p in
  let seq1 = Seq.of_dispenser (fun () -> Queue.dequeue unparsed |> Option.map ~f:(parse ~sst)) in
  let seq2 = if has_more then Seq.map (parse ~sst) seq else Seq.empty in
  Seq.append seq1 seq2

let stream_rows_buffer ?only_sheet ~sw ~feed cell_parser =
  let stream = Eio.Stream.create 0 in
  let push x = Eio.Stream.add stream (Some x) in
  let finalize () = Eio.Stream.add stream None in

  let sst_p = process_file ?only_sheet ~skip_sst:false ~sw ~feed push finalize in

  let parse ~sst row = parse_row_with_sst sst cell_parser row in
  with_minimal_buffering stream sst_p ~parse

let yojson_cell_parser : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_parser =
  {
    string = (fun _location s -> `String (Xml.DOM.unescape s));
    formula = (fun _location ~formula:_ s -> `String (Xml.DOM.unescape s));
    error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    date = (fun _location s -> `String s);
    null = `Null;
  }
