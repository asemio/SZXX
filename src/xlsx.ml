open! Core
open Lwt.Syntax
open Lwt.Infix

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
  data: 'a array;
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

let xml_parser =
  Xml.make_parser Xml.{ accept_html_boolean_attributes = false; accept_unquoted_attributes = false }

let fold_angstrom ~filter_path ~on_match =
  let open Angstrom.Buffered in
  let rec loop state acc =
    match state, acc with
    | Done ({ buf; off = pos; len }, node), acc -> (
      let acc = Xml.SAX.Stream.folder ~filter_path ~on_match acc node in
      match parse xml_parser with
      | Partial feed -> (loop [@tailcall]) (feed (`Bigstring (Bigstring.sub_shared buf ~pos ~len))) acc
      | state -> (loop [@tailcall]) state acc )
    | state -> state
  in
  let f _entry bs ~len = function
    | (_, Error _) as x -> x
    | (Fail (_, [], err) as parse), _ -> parse, Error err
    | (Fail (_, marks, err) as parse), _ ->
      parse, Error (sprintf "%s: %s" (String.concat ~sep:" > " marks) err)
    | (Done _ as parse), Ok _ -> parse, Error "Impossible case Done. Please report this bug."
    | Partial feed, (Ok _ as acc) -> loop (feed (`Bigstring (Bigstring.sub_shared bs ~pos:0 ~len))) acc
  in
  Zip.Action.Fold_bigstring { init = parse xml_parser, Ok Xml.SAX.Stream.init; f }

let parse_sheet ~sheet_number push =
  let num = ref 0 in
  let on_match (el : Xml.DOM.element) =
    (match Xml.get_attr el.attrs "r" with
    | None -> incr num
    | Some s -> (
      try
        let i = Int.of_string s in
        (* Insert blank rows *)
        for row_number = !num to i - 2 do
          push { sheet_number; row_number; data = [||] }
        done;
        num := i
      with
      | _ -> incr num ));
    push { sheet_number; row_number = !num; data = el.children }
  in
  fold_angstrom ~filter_path:[ "worksheet"; "sheetData"; "row" ] ~on_match

let parse_string_cell el =
  let open Xml.DOM in
  match el |> dot "t" with
  | Some { text; _ } -> text
  | None -> filter_map "r" el ~f:(dot_text "t") |> String.concat_array

let flush_zip_stream zip_stream =
  Lwt_stream.filter_map
    (function
      | Zip.{ filename; _ }, Zip.Data.Fold_bigstring (_, Error msg) ->
        Some (sprintf "XLSX Parsing error in %s: %s" filename msg)
      | _ -> None)
    zip_stream
  |> Lwt_stream.to_list
  >>= function
  | [] -> Lwt.return_unit
  | errors -> failwith (String.concat ~sep:". " errors)

module SST = struct
  type t = string Lazy.t array

  let filter_path = [ "sst"; "si" ]

  let zip_entry_filename = "xl/sharedStrings.xml"

  let from_zip ~feed =
    let q = Queue.create () in
    let zip_stream, zip_success =
      Zip.stream_files ~feed (function
        | { filename = "xl/sharedStrings.xml"; _ } ->
          let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
          fold_angstrom ~filter_path ~on_match
        | _ -> Zip.Action.Skip )
    in
    let sst_p =
      let+ () = flush_zip_stream zip_stream in
      Queue.to_array q
    in
    let* () = zip_success in
    sst_p
end

let process_file ?only_sheet ~skip_sst ~feed push finalize =
  let q = Queue.create () in
  let zip_stream, zip_success =
    Zip.stream_files ~feed (function
      | { filename = "xl/workbook.xml"; _ } -> Skip
      | { filename = "xl/sharedStrings.xml"; _ } when skip_sst -> Skip
      | { filename = "xl/sharedStrings.xml"; _ } ->
        let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
        fold_angstrom ~filter_path:SST.filter_path ~on_match
      | { filename; _ } ->
        Option.try_with (fun () -> Scanf.sscanf filename "xl/worksheets/%[sS]heet%d.xml" (fun _ d -> d))
        |> Option.bind ~f:(fun i ->
             if Option.value_map only_sheet ~default:true ~f:(( = ) i)
             then Some (parse_sheet ~sheet_number:i push)
             else None )
        |> Option.value ~default:Zip.Action.Skip )
  in
  let result_p =
    Lwt.finalize
      (fun () -> flush_zip_stream zip_stream)
      (fun () ->
        finalize ();
        Lwt.return_unit)
  in
  let success =
    let* () = zip_success in
    result_p
  in
  let sst_p =
    let+ () = result_p in
    Queue.to_array q
  in
  sst_p, success

let resolve_sst_index (sst : SST.t) ~sst_index =
  let index = Int.of_string sst_index in
  match sst with
  | sst when index < Array.length sst && index >= 0 -> Some (force sst.(index))
  | _ -> None

let unwrap_status cell_parser (sst : SST.t) (row : 'a status row) =
  let data =
    Array.map row.data ~f:(function
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
    match Xml.get_attr el.attrs "t" with
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
    match Xml.get_attr el.attrs "t" with
    | Some "s" -> (
      match el |> dot "v" with
      | None -> Available cell_parser.null
      | Some { text = sst_index; _ } -> Delayed { location; sst_index } )
    | ty -> Available (extract_cell_base cell_parser location el ty)
  in
  extract_cell_sst, extract_cell_status

let col_cache = String.Table.create ()

let index_of_column s =
  let key =
    String.take_while s ~f:(function
      | 'A' .. 'Z' -> true
      | _ -> false )
  in
  Hashtbl.find_or_add col_cache key ~default:(fun () ->
    String.fold key ~init:0 ~f:(fun acc c -> (acc * 26) + Char.to_int c - 64) - 1 )

let row_width num_cells data =
  let open Xml.DOM in
  Xml.get_attr (Array.last data).attrs "r" |> function
  | None -> num_cells
  | Some r -> max num_cells (index_of_column r + 1)

let parse_row_with_sst sst cell_parser ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  match Array.length data with
  | 0 -> { row with data = [||] }
  | num_cells ->
    let num_cols = row_width num_cells data in
    let new_data = Array.create ~len:num_cols cell_parser.null in
    Array.iteri data ~f:(fun i el ->
      let col_index = Xml.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column in
      let v = extract_cell_sst sst cell_parser { col_index; sheet_number; row_number } el in
      new_data.(col_index) <- v );
    { row with data = new_data }

let parse_row_without_sst cell_parser ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  match Array.length data with
  | 0 -> { row with data = [||] }
  | num_cells ->
    let num_cols = row_width num_cells data in
    let new_data = Array.create ~len:num_cols (Available cell_parser.null) in
    Array.iteri data ~f:(fun i el ->
      let col_index = Xml.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column in
      let v = extract_cell_status cell_parser { col_index; sheet_number; row_number } el in
      new_data.(col_index) <- v );
    { row with data = new_data }

let stream_rows ?only_sheet ?(skip_sst = false) ~feed cell_parser =
  let stream, push = Lwt_stream.create () in
  let finalize () = push None in

  let sst_p, processed_p =
    process_file ?only_sheet ~skip_sst ~feed
      (fun x -> push (Some (parse_row_without_sst cell_parser x)))
      finalize
  in
  stream, sst_p, processed_p

let stream_rows_unparsed ?only_sheet ?(skip_sst = false) ~feed () =
  let stream, push = Lwt_stream.create () in
  let finalize () = push None in

  let sst_p, processed_p = process_file ?only_sheet ~skip_sst ~feed (fun x -> push (Some x)) finalize in
  stream, sst_p, processed_p

let stream_rows_buffer ?only_sheet ~feed cell_parser =
  let stream, push = Lwt_stream.create () in
  let finalize () = push None in
  let sst_p, processed_p =
    process_file ?only_sheet ~skip_sst:false ~feed (fun x -> push (Some x)) finalize
  in
  let parsed_stream =
    Lwt_stream.map_s
      (fun row -> sst_p |> Lwt.map (fun sst -> parse_row_with_sst sst cell_parser row))
      stream
  in
  parsed_stream, processed_p

let yojson_cell_parser : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_parser =
  {
    string = (fun _location s -> `String (Xml.unescape s));
    formula = (fun _location ~formula:_ s -> `String (Xml.unescape s));
    error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    date = (fun _location s -> `String s);
    null = `Null;
  }
