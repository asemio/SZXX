open! Core_kernel

open Zip
open Xml

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a array;
} [@@deriving sexp_of]

let origin = Date.add_days (Date.create_exn ~y:1900 ~m:(Month.of_int_exn 1) ~d:1) (-2)

let parse_date f = Float.to_int f |> Date.add_days origin

let parse_datetime ~zone f =
  let parts = Float.modf f in
  let date = Float.Parts.integral parts |> Float.to_int |> Date.add_days origin in
  let frac = Float.(Parts.fractional parts * 86400000. |> round) |> Time.Span.of_ms in
  let ofday = Time.Ofday.of_span_since_start_of_day_exn frac in
  Time.of_date_ofday ~zone date ofday

let parse_sheet ~sheet_number push =
  let num = ref 0 in
  let filter_map el =
    incr num;
    let next = !num in
    let row_number = begin match get_attr el "r" with
    | None -> next
    | Some s ->
      try begin
        let i = Int.of_string s in
        if next < i then begin
          (* Insert blank rows *)
          for row_number = next to (pred i) do
            push (Some { sheet_number; row_number; data = [||] })
          done;
          num := i;
          i
        end
        else next
      end
      with _ -> next
    end
    in
    push (Some { sheet_number; row_number; data = el.children });
    None
  in
  Parse (parser ~filter_map ["worksheet"; "sheetData"; "row"])

let get_stream ?only_sheet input_channel =
  let stream, push = Lwt_stream.create () in
  let sst_p, sst_w = Lwt.wait () in
  let processed_p =
    let zip_stream = stream_files input_channel (fun entry ->
        begin match entry.filename with
        | "xl/workbook.xml" -> Parse (parser [])

        | "xl/sharedStrings.xml" ->
          let path = ["sst"; "si";] in
          let filter_map el =
            let text = begin match el |> dot "t" with
            | (Some _ as x) -> x
            | None -> get el [dot "r"; dot "t"]
            end |> Option.value_map ~default:"" ~f:(fun { text; _ } -> text)
            in
            Some { el with text; children = [||] }
          in
          Parse (parser ~filter_map path)

        | filename ->
          let open Option.Monad_infix in
          String.chop_prefix ~prefix:"xl/worksheets/sheet" filename
          >>= (String.chop_suffix ~suffix:".xml")
          >>= (fun s -> Option.try_with (fun () -> Int.of_string s))
          |> (Option.filter ~f:(fun i -> Option.value_map only_sheet ~default:true ~f:(Int.(=) i)))
          >>| (fun sheet_number -> parse_sheet ~sheet_number push)
          |> Option.value ~default:Skip
        end
      )
    in
    let%lwt () =
      Lwt.finalize (fun () ->
        Lwt_stream.iter (fun (entry, data) ->
          let open Zip in
          begin match data with
          | Skipped | As_string _ -> ()
          | Parsed (Ok xml) ->
            begin match entry.filename with
            | "xl/sharedStrings.xml" -> Lwt.wakeup_later sst_w xml
            | _ -> ()
            end
          | Parsed (Error msg) -> failwithf "XLSX Parsing error for %s: %s" entry.filename msg ()
          end
        ) zip_stream
      ) (fun () ->
        begin match Lwt.state sst_p with
        | Return _ | Fail _ -> ()
        | Sleep -> Lwt.wakeup_later sst_w {
            decl_attrs = None;
            top = { tag = "sst"; attrs = []; text = ""; children = [||] }
          }
        end;
        Lwt.return_unit
      )
    in
    push None;
    Lwt.return_unit
  in
  stream, sst_p, processed_p

type 'a cell_reader = col_index:int -> sheet_number:int -> row_number:int -> string -> 'a

type 'a cell_of_string = {
  string: 'a cell_reader;
  error: 'a cell_reader;
  boolean: 'a cell_reader;
  number: 'a cell_reader;
  null: 'a;
}

let extract ~null ~col_index ~sheet_number ~row_number extractor = function
| None -> null
| Some { text; _ } -> extractor ~col_index ~sheet_number ~row_number text

let extract_cell { string; error; boolean; number; null } sst ~col_index ~sheet_number ~row_number el =
  let reader = extract ~null ~col_index ~sheet_number ~row_number in
  begin match get_attr el "t" with
  | None
  | Some "n" ->
    begin
      try el |> dot "v" |> reader number
      with _ -> null
    end
  | Some "str" -> el |> dot "v" |> reader string
  | Some "inlineStr" -> get el [dot "is"; dot "t"] |> reader string
  | Some "s" ->
    begin match el |> dot "v" with
    | None -> null
    | Some { text; _ } ->
      let table = !sst () in
      try table |> at text |> reader string
      with _ -> null
    end
  | Some "e" -> el |> dot "v" |> reader error
  | Some "b" -> el |> dot "v" |> reader boolean
  | Some t ->
    failwithf "Unknown data type: %s ::: %s" t (sexp_of_element el |> Sexp.to_string) ()
  end

let column_to_index col =
  String.fold_until col ~init:0 ~finish:Fn.id ~f:(fun acc -> function
  | ('A'..'Z' as c) -> Continue ((acc * 26) + (Char.to_int c) - 64)
  | '0'..'9' when acc > 0 -> Stop acc
  | _ -> failwithf "Invalid XLSX column name %s" col ()
  ) |> pred

let extract_row cell_of_string sst ({ data; sheet_number; row_number } as row) =
  let num_cells = Array.length data in
  if num_cells = 0 then { row with data = [||] } else
  let num_cols =
    Array.last data
    |> (fun el -> get_attr el "r")
    |> Option.value_map ~default:0 ~f:(fun r -> (column_to_index r) + 1)
    |> max num_cells
  in
  let new_data = Array.create ~len:num_cols cell_of_string.null in
  Array.iteri data ~f:(fun i el ->
    let col_index = get_attr el "r" |> Option.value_map ~default:i ~f:column_to_index in
    let v = extract_cell cell_of_string sst ~col_index ~sheet_number ~row_number el in
    Array.set new_data col_index v
  );
  { row with data = new_data }

exception Not_ready

let stream_rows ?only_sheet cell_of_string input_channel =
  let stream, sst_p, processed_p = get_stream ?only_sheet input_channel in
  let sst = ref (fun () -> raise Not_ready) in
  let parsed_stream = Lwt_stream.map_s (fun row ->
      try%lwt
        Lwt.return (extract_row cell_of_string sst row)
      with Not_ready ->
        begin
          let%lwt sst_doc = sst_p in
          sst := (fun () -> sst_doc.top);
          Lwt.return (extract_row cell_of_string sst row)
        end
    ) stream
  in
  parsed_stream, processed_p

let yojson_readers : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_of_string = {
  string = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `String s);
  error = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `String (sprintf "#ERROR# %s" s));
  boolean = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `Bool String.(s = "1"));
  number = (fun ~col_index:_ ~sheet_number:_ ~row_number:_ s -> `Float (Float.of_string s));
  null = `Null;
}
