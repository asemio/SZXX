open! Core_kernel
open Lwt.Syntax

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
  | Delayed   of delayed_string
[@@deriving sexp_of]

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a array;
}
[@@deriving sexp_of]

type sstq =
  | SSTQ          of string Queue.t
  | SSTQ_unparsed of Xml.DOM.element Queue.t

type sst =
  | SST          of string array
  | SST_unparsed of Xml.DOM.element array
  | SST_skipped

let origin = Date.add_days (Date.create_exn ~y:1900 ~m:(Month.of_int_exn 1) ~d:1) (-2)

let parse_date f = Float.to_int f |> Date.add_days origin

let parse_datetime ~zone f =
  let parts = Float.modf f in
  let date = Float.Parts.integral parts |> Float.to_int |> Date.add_days origin in
  let frac = Float.(Parts.fractional parts * 86400000. |> round) |> Time.Span.of_ms in
  let ofday = Time.Ofday.of_span_since_start_of_day_exn frac in
  Time.of_date_ofday ~zone date ofday

let fold_angstrom ~filter_path ~on_match =
  let open Angstrom.Buffered in
  let rec loop state acc =
    match state, acc with
    | Done ({ buf; off = pos; len }, node), acc -> (
      let acc = Xml.SAX.Stream.folder ~filter_path ~on_match acc node in
      match parse Xml.parser with
      | Partial feed -> (loop [@tailcall]) (feed (`Bigstring (Bigstring.sub_shared buf ~pos ~len))) acc
      | state -> (loop [@tailcall]) state acc
    )
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
  Zip.Action.Fold_bigstring { init = parse Xml.parser, Ok Xml.SAX.Stream.init; f }

let parse_sheet ~sheet_number push =
  let num = ref 0 in
  let filter_path = [ "worksheet"; "sheetData"; "row" ] in
  let on_match (el : Xml.DOM.element) =
    incr num;
    let next = !num in
    let row_number =
      match Xml.get_attr el.attrs "r" with
      | None -> next
      | Some s -> (
        try
          let i = Int.of_string s in
          if next < i
          then begin
            (* Insert blank rows *)
            for row_number = next to pred i do
              push { sheet_number; row_number; data = [||] }
            done;
            num := i;
            i
          end
          else next
        with
        | _ -> next
      )
    in
    push { sheet_number; row_number; data = el.children }
  in
  fold_angstrom ~filter_path ~on_match

let parse_string_cell el =
  let open Xml.DOM in
  match el |> dot "t" with
  | Some { text; _ } -> text
  | None -> filter_map "r" el ~f:(fun r -> dot_text "t" r) |> String.concat_array

let process_file ?only_sheet ~skip_sst ~feed push finalize mode =
  let sst_p, sst_w = Lwt.wait () in
  let sst_ref = ref None in
  let processed_p =
    let zip_stream, zip_processed =
      Zip.stream_files ~feed (fun entry ->
          match entry.filename with
          | "xl/workbook.xml" -> Skip
          | "xl/sharedStrings.xml" when skip_sst -> Skip
          | "xl/sharedStrings.xml" -> (
            let filter_path = [ "sst"; "si" ] in
            match mode with
            | `Parsed ->
              let q = Queue.create () in
              sst_ref := Some (SSTQ q);
              let on_match el = Queue.enqueue q (parse_string_cell el) in
              fold_angstrom ~filter_path ~on_match
            | `Unparsed ->
              let q = Queue.create () in
              sst_ref := Some (SSTQ_unparsed q);
              let on_match el = Queue.enqueue q el in
              fold_angstrom ~filter_path ~on_match
          )
          | filename ->
            let open Option.Monad_infix in
            String.chop_prefix ~prefix:"xl/worksheets/sheet" filename
            >>= String.chop_suffix ~suffix:".xml"
            >>= (fun s -> Option.try_with (fun () -> Int.of_string s))
            |> Option.filter ~f:(fun i -> Option.value_map only_sheet ~default:true ~f:(( = ) i))
            >>| (fun sheet_number -> parse_sheet ~sheet_number push)
            |> Option.value ~default:Zip.Action.Skip)
    in
    let* () =
      Lwt.finalize
        (fun () ->
          Lwt_stream.iter
            (fun (entry, data) ->
              let open Zip in
              match data with
              | Data.Skip
               |Data.String _
               |Data.Parse _
               |Data.Fold_string _ ->
                ()
              | Data.Fold_bigstring (_, Error msg) ->
                failwithf "XLSX Parsing error for %s: %s" entry.filename msg ()
              | Data.Fold_bigstring (_, Ok _) -> (
                match entry.filename with
                | "xl/sharedStrings.xml" ->
                  let sst =
                    match !sst_ref with
                    | None -> SST_skipped
                    | Some (SSTQ q) -> SST (Queue.to_array q)
                    | Some (SSTQ_unparsed q) -> SST_unparsed (Queue.to_array q)
                  in
                  Lwt.wakeup_later sst_w sst
                | _ -> ()
              ))
            zip_stream)
        (fun () ->
          if Lwt.is_sleeping sst_p then Lwt.wakeup_later sst_w (SST [||]);
          Lwt.return_unit)
    in
    Lwt.( <&> ) (finalize ()) zip_processed
  in
  sst_p, processed_p

let resolve_sst_index sst ~sst_index =
  let index = Int.of_string sst_index in
  match sst with
  | SST sst when index < Array.length sst && index >= 0 -> Some sst.(index)
  | SST_unparsed sst when index < Array.length sst && index >= 0 -> Some (sst.(index) |> parse_string_cell)
  | SST _
   |SST_unparsed _ ->
    None
  | SST_skipped -> failwith "Cannot resolve SST indexed when the SST was intentionally skipped"

let unwrap_status cell_parser sst (row : 'a status row) =
  let data =
    match sst with
    | SST sst ->
      Array.map row.data ~f:(function
        | Available x -> x
        | Delayed { location; sst_index } ->
          let index = Int.of_string sst_index in
          if index < Array.length sst && index >= 0
          then cell_parser.string location sst.(index)
          else cell_parser.null)
    | SST_unparsed sst ->
      Array.map row.data ~f:(function
        | Available x -> x
        | Delayed { location; sst_index } ->
          let index = Int.of_string sst_index in
          if index < Array.length sst && index >= 0
          then cell_parser.string location (parse_string_cell sst.(index))
          else cell_parser.null)
    | SST_skipped -> failwith "Cannot unwrap status when the SST was intentionally skipped"
  in
  { row with data }

let extract ~null location extractor : Xml.DOM.element option -> 'a status = function
| None -> Available null
| Some { text; _ } -> Available (extractor location text)

let extract_cell ~sst { string; formula; error; boolean; number; date; null } location el =
  let open Xml.DOM in
  match Xml.get_attr el.attrs "t" with
  | None
   |Some "n" ->
    el |> dot "v" |> extract ~null location number
  | Some "d" -> el |> dot "v" |> extract ~null location date
  | Some "str" -> (
    match el |> dot_text "v", el |> dot_text "f" with
    | None, _ -> Available null
    | Some s, Some f -> Available (formula location ~formula:f s)
    | Some s, None -> Available (formula location ~formula:"" s)
  )
  | Some "inlineStr" -> (
    match dot "is" el with
    | None -> Available null
    | Some el -> Available (string location (parse_string_cell el))
  )
  | Some "s" -> (
    match el |> dot "v", sst with
    | None, _ -> Available null
    | Some { text = sst_index; _ }, None -> Delayed { location; sst_index }
    | Some { text = sst_index; _ }, Some sst -> (
      match resolve_sst_index sst ~sst_index with
      | None -> Available null
      | Some resolved -> Available (string location resolved)
    )
  )
  | Some "e" -> el |> dot "v" |> extract ~null location error
  | Some "b" -> el |> dot "v" |> extract ~null location boolean
  | Some t -> failwithf "Unknown data type: %s ::: %s" t (sexp_of_element el |> Sexp.to_string) ()

let col_cache = String.Table.create ()

let index_of_column s =
  let key =
    String.take_while s ~f:(function
      | 'A' .. 'Z' -> true
      | _ -> false)
  in
  String.Table.find_or_add col_cache key ~default:(fun () ->
      String.fold key ~init:0 ~f:(fun acc c -> (acc * 26) + Char.to_int c - 64) - 1)

let parse_row ?sst cell_parser ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  let num_cells = Array.length data in
  match num_cells with
  | 0 -> { row with data = [||] }
  | num_cells ->
    let num_cols =
      Array.last data
      |> (fun el -> Xml.get_attr el.attrs "r")
      |> Option.value_map ~default:num_cells ~f:(fun r -> max num_cells (index_of_column r + 1))
    in
    let null = Available cell_parser.null in
    let new_data = Array.create ~len:num_cols null in
    Array.iteri data ~f:(fun i el ->
        let col_index = Xml.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column in
        let v = extract_cell ~sst cell_parser { col_index; sheet_number; row_number } el in
        new_data.(col_index) <- v);
    { row with data = new_data }

let stream_rows ?only_sheet ?(skip_sst = false) ~feed cell_parser =
  let stream, push = Lwt_stream.create () in
  let finalize () =
    push None;
    Lwt.return_unit
  in
  let sst_ref = ref None in

  let sst_p, processed_p =
    process_file ?only_sheet ~skip_sst ~feed
      (fun x -> push (Some (parse_row ?sst:!sst_ref cell_parser x)))
      finalize `Parsed
  in
  Lwt.on_success sst_p (fun x -> sst_ref := Some x);
  stream, sst_p, processed_p

let stream_rows_unparsed ?only_sheet ?(skip_sst = false) ~feed () =
  let stream, push = Lwt_stream.create () in
  let finalize () =
    push None;
    Lwt.return_unit
  in

  let sst_p, processed_p =
    process_file ?only_sheet ~skip_sst ~feed (fun x -> push (Some x)) finalize `Unparsed
  in
  stream, sst_p, processed_p

let stream_rows_buffer ?only_sheet ~feed cell_parser =
  let stream, push = Lwt_stream.create () in
  let finalize () =
    push None;
    Lwt.return_unit
  in
  let sst_p, processed_p =
    process_file ?only_sheet ~skip_sst:false ~feed (fun x -> push (Some x)) finalize `Parsed
  in
  let parsed_stream =
    Lwt_stream.map_s
      (fun row ->
        sst_p |> Lwt.map (fun sst -> parse_row cell_parser row |> unwrap_status cell_parser sst))
      stream
  in
  parsed_stream, processed_p

let yojson_cell_parser : [> `Bool   of bool | `Float  of float | `String of string | `Null ] cell_parser =
  {
    string = (fun _location s -> `String (Xml.unescape s));
    formula = (fun _location ~formula:_ s -> `String (Xml.unescape s));
    error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    date = (fun _location s -> `String s);
    null = `Null;
  }
