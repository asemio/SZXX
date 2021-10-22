open! Core_kernel

type location = {
  sheet_number: int;
  row_number: int;
  col_index: int;
}
[@@deriving sexp_of]

type 'a cell_of_string = {
  string: location -> string -> 'a;
  error: location -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
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

type sst = SST of string array

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
  let filter_path = " worksheet sheetData row" in
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

let process_file_sst sst_queue =
  let open Xml.DOM in
  let on_match el =
    let text =
      begin
        match el |> dot "t" with
        | Some _ as x -> x
        | None -> get el [ dot "r"; dot "t" ]
      end
      |> Option.value_map ~default:"" ~f:(fun { text; _ } -> text)
    in
    Queue.enqueue sst_queue text
  in
  let filter_path = " sst si" in
  fold_angstrom ~filter_path ~on_match

let process_file ?only_sheet ~feed push finalize =
  let sst_p, sst_w = Lwt.wait () in
  let sst_queue = Queue.create () in
  let processed_p =
    let zip_stream, zip_processed =
      Zip.stream_files ~feed (fun entry ->
          match entry.filename with
          | "xl/workbook.xml" -> Skip
          | "xl/sharedStrings.xml" -> process_file_sst sst_queue
          | filename ->
            let open Option.Monad_infix in
            String.chop_prefix ~prefix:"xl/worksheets/sheet" filename
            >>= String.chop_suffix ~suffix:".xml"
            >>= (fun s -> Option.try_with (fun () -> Int.of_string s))
            |> Option.filter ~f:(fun i -> Option.value_map only_sheet ~default:true ~f:(( = ) i))
            >>| (fun sheet_number -> parse_sheet ~sheet_number push)
            |> Option.value ~default:Zip.Action.Skip)
    in
    let%lwt () =
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
                | "xl/sharedStrings.xml" -> Lwt.wakeup_later sst_w (SST (Queue.to_array sst_queue))
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

let extract ~null location extractor : Xml.DOM.element option -> 'a status = function
| None -> Available null
| Some { text; _ } -> Available (extractor location text)

let extract_cell { string; error; boolean; number; null } location el =
  let reader = extract ~null location in
  let open Xml.DOM in
  match Xml.get_attr el.attrs "t" with
  | None
   |Some "n" -> (
    try el |> dot "v" |> reader number with
    | _ -> Available null
  )
  | Some "str" -> el |> dot "v" |> reader string
  | Some "inlineStr" -> get el [ dot "is"; dot "t" ] |> reader string
  | Some "s" -> (
    match el |> dot "v" with
    | None -> Available null
    | Some { text = sst_index; _ } -> Delayed { location; sst_index }
  )
  | Some "e" -> el |> dot "v" |> reader error
  | Some "b" -> el |> dot "v" |> reader boolean
  | Some t -> failwithf "Unknown data type: %s ::: %s" t (sexp_of_element el |> Sexp.to_string) ()

let column_to_index col =
  String.fold_until col ~init:0 ~finish:Fn.id ~f:(fun acc -> function
    | 'A' .. 'Z' as c -> Continue ((acc * 26) + Char.to_int c - 64)
    | '0' .. '9' when acc > 0 -> Stop acc
    | _ -> failwithf "Invalid XLSX column name %s" col ())
  |> pred

let extract_row cell_of_string ({ data; sheet_number; row_number } as row) =
  let open Xml.DOM in
  let num_cells = Array.length data in
  if num_cells = 0
  then { row with data = [||] }
  else (
    let num_cols =
      Array.last data
      |> (fun el -> Xml.get_attr el.attrs "r")
      |> Option.value_map ~default:0 ~f:(fun r -> column_to_index r + 1)
      |> max num_cells
    in
    let new_data = Array.create ~len:num_cols (Available cell_of_string.null) in
    Array.iteri data ~f:(fun i el ->
        let col_index = Xml.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:column_to_index in
        let v = extract_cell cell_of_string { col_index; sheet_number; row_number } el in
        new_data.(col_index) <- v);
    { row with data = new_data }
  )

let stream_rows ?only_sheet ~feed cell_of_string =
  let stream, push = Lwt_stream.create () in
  let finalize () =
    push None;
    Lwt.return_unit
  in

  let sst_p, processed_p =
    process_file ?only_sheet ~feed (fun x -> push (Some (extract_row cell_of_string x))) finalize
  in
  stream, sst_p, processed_p

let resolve_sst_index (SST sst) ~sst_index =
  Option.try_with (fun () -> Int.of_string sst_index |> Array.get sst)

let await_delayed cell_of_string (SST sst) (row : 'a status row) =
  let data =
    Array.map row.data ~f:(function
      | Available x -> x
      | Delayed { location; sst_index } -> (
        Option.try_with (fun () -> Int.of_string sst_index |> Array.get sst) |> function
        | None -> cell_of_string.null
        | Some text -> cell_of_string.string location text
      ))
  in
  { row with data }

let stream_rows_buffer ?only_sheet ~feed cell_of_string =
  let stream, push = Lwt_stream.create () in
  let finalize () =
    push None;
    Lwt.return_unit
  in
  let sst_p, processed_p = process_file ?only_sheet ~feed (fun x -> push (Some x)) finalize in
  let parsed_stream =
    Lwt_stream.map_s
      (fun row ->
        sst_p |> Lwt.map (fun sst -> extract_row cell_of_string row |> await_delayed cell_of_string sst))
      stream
  in
  parsed_stream, processed_p

let yojson_readers : [> `Bool   of bool | `Float  of float | `String of string | `Null ] cell_of_string =
  {
    string = (fun _location s -> `String s);
    error = (fun _location s -> `String (sprintf "#ERROR# %s" s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    null = `Null;
  }
