open! Base
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
  error: location -> formula:string -> string -> 'a;
  boolean: location -> string -> 'a;
  number: location -> string -> 'a;
  date: location -> string -> 'a;
  null: 'a;
}

type 'a row = {
  sheet_number: int;
  row_number: int;
  data: 'a list;
}
[@@deriving sexp_of]

let epoch = -25569

let parse_date f = Ptime.unsafe_of_d_ps (Float.to_int f + epoch, 0L) |> Ptime.to_date

let parse_datetime f =
  let parts = Float.modf f in
  let ps = Float.(Parts.fractional parts * 86_400_000. |> to_int64) |> Int64.( * ) 1_000_000_000L in
  let d = Float.(Parts.integral parts |> to_int) in
  Ptime.unsafe_of_d_ps (d + epoch, ps)

let xml_parser_options =
  Xml.SAX.
    {
      accept_html_boolean_attributes = false;
      accept_unquoted_attributes = false;
      accept_single_quoted_attributes = false;
      batch_size = 20;
    }

let xml_parser = Xml.SAX.make_parser xml_parser_options

let fold_angstrom ~filter_path ~on_match () =
  let sax = ref Xml.SAX.Expert.Stream.init in
  let on_parse node = sax := Xml.SAX.Expert.Stream.folder ~filter_path ~on_match !sax node in
  Zip.Action.Parse_many { parser = xml_parser; on_parse }

let parse_string_cell el =
  let open Xml.DOM in
  match dot "t" el with
  | Some { text; _ } -> text
  | None -> filter_map "r" el ~f:(dot_text "t") |> String.concat

module SST = struct
  type t = string Lazy.t array

  let filter_path = [ "sst"; "si" ]

  let zip_entry_filename = "xl/sharedStrings.xml"

  let from_feed feed =
    Switch.run @@ fun sw ->
    let q = Queue.create () in
    let seen = ref false in
    Zip.stream_files ~sw ~feed (function
      | { filename = "xl/sharedStrings.xml"; _ } ->
        seen := true;
        let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
        fold_angstrom ~filter_path ~on_match ()
      | _ when !seen -> Terminate
      | _ -> Fast_skip )
    |> Sequence.iter ~f:(function
         | Zip.{ filename; _ }, Zip.Data.Parse_many state -> (
           match Zip.Data.parser_state_to_result state with
           | Ok x -> x
           | Error msg -> Printf.failwithf "SZXX: File '%s' error: %s" filename msg () )
         | _ -> () );

    Queue.to_array q

  let from_entries file (entries : Zip.entry list) =
    match List.find entries ~f:(fun entry -> String.( = ) entry.filename zip_entry_filename) with
    | None -> [||]
    | Some entry -> (
      let q = Queue.create () in
      let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
      let action = fold_angstrom ~filter_path ~on_match () in
      match Zip.extract_from_index file entry action with
      | Zip.Data.Parse_many state ->
        (match Zip.Data.parser_state_to_result state with
        | Ok () -> ()
        | Error msg -> Printf.failwithf "SZXX: File '%s' error: %s" entry.filename msg ());
        Queue.to_array q
      | _ -> assert false )

  let from_file file = Zip.index_entries file |> from_entries file

  let resolve_sst_index (sst : t) ~sst_index =
    try Some (force (Array.get sst (Int.of_string sst_index))) with
    | _ -> None
end

let index_of_column s =
  String.fold_until s ~init:0 ~finish:Fn.id ~f:(fun acc -> function
    | 'A' .. 'Z' as c -> Continue ((acc * 26) + Char.to_int c - 64)
    | _ -> Stop acc )
  - 1

module Expert = struct
  module SST = SST

  type delayed_string = {
    location: location;
    sst_index: string;
  }
  [@@deriving sexp_of]

  type 'a status =
    | Available of 'a
    | Delayed of delayed_string
  [@@deriving sexp_of]

  let unwrap_status cell_parser (sst : SST.t) (row : 'a status row) =
    let data =
      List.map row.data ~f:(function
        | Available x -> x
        | Delayed { location; sst_index } -> (
          match SST.resolve_sst_index sst ~sst_index with
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
        dot "v" el |> extract ~null location number
      | Some "d" -> dot "v" el |> extract ~null location date
      | Some "str" -> (
        match dot_text "v" el with
        | None -> null
        | Some s -> formula location s ~formula:(dot_text "f" el |> Option.value ~default:"") )
      | Some "e" -> (
        match dot_text "v" el with
        | None -> null
        | Some s -> error location s ~formula:(dot_text "f" el |> Option.value ~default:"") )
      | Some "inlineStr" -> (
        match dot "is" el with
        | None -> null
        | Some el -> string location (parse_string_cell el) )
      | Some "b" -> dot "v" el |> extract ~null location boolean
      | Some t ->
        Printf.failwithf !"Unknown data type: %s. Please report this bug. %{sexp: element}" t el ()
    in
    let extract_cell_sst sst cell_parser location el =
      match Xml.DOM.get_attr el.attrs "t" with
      | Some "s" -> (
        match dot "v" el with
        | None -> cell_parser.null
        | Some { text = sst_index; _ } -> (
          match SST.resolve_sst_index sst ~sst_index with
          | None -> cell_parser.null
          | Some resolved -> cell_parser.string location resolved ) )
      | ty -> extract_cell_base cell_parser location el ty
    in
    let extract_cell_status cell_parser location el =
      match Xml.DOM.get_attr el.attrs "t" with
      | Some "s" -> (
        match dot "v" el with
        | None -> Available cell_parser.null
        | Some { text = sst_index; _ } -> Delayed { location; sst_index } )
      | ty -> Available (extract_cell_base cell_parser location el ty)
    in
    extract_cell_sst, extract_cell_status

  let parse_row_with_sst sst cell_parser ({ data; sheet_number; row_number } as row) =
    let open Xml.DOM in
    match data with
    | [] -> { row with data = [] }
    | _ ->
      let rec loop i acc = function
        | [] -> List.rev acc
        | el :: rest ->
          let col_index =
            Xml.DOM.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column
          in
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
          let col_index =
            Xml.DOM.get_attr el.attrs "r" |> Option.value_map ~default:i ~f:index_of_column
          in
          let v = extract_cell_status cell_parser { col_index; sheet_number; row_number } el in
          let acc = Fn.apply_n_times ~n:(col_index - i) (List.cons (Available cell_parser.null)) acc in
          (loop [@tailcall]) (col_index + 1) (v :: acc) rest
      in
      { row with data = loop 0 [] data }
end

let parse_sheet ~sheet_number push =
  let num = ref 0 in
  let on_match (el : Xml.DOM.element) =
    (match Xml.DOM.get_attr el.attrs "r" with
    | None -> Int.incr num
    | Some s -> (
      try
        let i = Int.of_string s in
        (* Insert blank rows *)
        for row_number = !num to i - 2 do
          push { sheet_number; row_number; data = [] }
        done;
        num := i
      with
      | _ -> Int.incr num ));
    push { sheet_number; row_number = !num; data = el.children }
  in
  fold_angstrom ~filter_path:[ "worksheet"; "sheetData"; "row" ] ~on_match ()

let get_sheet_action ~filter_sheets (entry : Zip.entry) push =
  Option.try_with (fun () ->
    Stdlib.Scanf.sscanf entry.filename "xl/worksheets/%[sS]heet%d.xml" (fun _ d -> d) )
  |> Option.filter ~f:(fun sheet_id ->
       Option.value_map filter_sheets ~default:true ~f:(fun f ->
         f ~sheet_id ~raw_size:entry.descriptor.uncompressed_size ) )
  |> Option.map ~f:(fun sheet_number -> parse_sheet ~sheet_number push)

let stream_rows_double_pass ?filter_sheets ~sw file cell_parser =
  let entries = Zip.index_entries file in
  let sst = SST.from_entries file entries in
  Sequence.of_seq
  @@ Fiber.fork_seq ~sw
  @@ fun yield ->
  let push x = yield (Expert.parse_row_with_sst sst cell_parser x) in

  List.iter entries ~f:(fun ({ filename; _ } as entry) ->
    get_sheet_action ~filter_sheets entry push
    |> Option.iter ~f:(fun action ->
         match Zip.extract_from_index file entry action with
         | Zip.Data.Parse_many state -> (
           match Zip.Data.parser_state_to_result state with
           | Ok () -> ()
           | Error msg -> Printf.failwithf "SZXX: File '%s': %s" filename msg () )
         | _ -> assert false ) )

let process_file ?filter_sheets ~sw ~feed (sst_p, sst_w) yield =
  let q = Queue.create () in

  Zip.stream_files ~sw ~feed (function
    | { filename = "xl/sharedStrings.xml"; _ } ->
      let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
      fold_angstrom ~filter_path:SST.filter_path ~on_match ()
    | entry -> get_sheet_action ~filter_sheets entry yield |> Option.value ~default:Zip.Action.Fast_skip )
  |> Sequence.iter ~f:(function
       | Zip.{ filename = "xl/sharedStrings.xml"; _ }, Zip.Data.Parse_many state -> (
         match Zip.Data.parser_state_to_result state with
         | Ok () -> Promise.resolve sst_w (Queue.to_array q)
         | Error msg -> Printf.failwithf "SZXX: File '%s': %s" SST.zip_entry_filename msg () )
       | Zip.{ filename; _ }, Zip.Data.Parse_many state -> (
         match Zip.Data.parser_state_to_result state with
         | Ok () -> ()
         | Error msg -> Printf.failwithf "SZXX: File '%s': %s" filename msg () )
       | _ -> () );

  if not (Promise.is_resolved sst_p) then Promise.resolve sst_w (Queue.to_array q)

type status =
  | All_buffered
  | Overflowed
  | Got_SST of SST.t

let with_minimal_buffering ?max_buffering ?filter cell_parser sst_p yield raw_rows =
  let module Seq = Stdlib.Seq in
  let q = Queue.create ?capacity:max_buffering () in
  let highwater =
    match max_buffering with
    | None -> Int.max_value
    | Some x when Int.is_non_negative x -> x
    | Some x -> Printf.failwithf "SZXX: stream_rows_single_pass max_buffering: %d < 0" x ()
  in

  let status, raw_rows =
    let filter = Option.value filter ~default:(fun _ -> true) in
    let rec loop acc =
      match Promise.peek sst_p with
      | Some sst -> Got_SST sst, acc
      | None -> (
        match acc () with
        | Seq.Nil -> All_buffered, Seq.empty
        | Cons (_, acc) when Queue.length q = highwater -> Overflowed, acc
        | Cons (row, next) ->
          if filter row then Queue.enqueue q row;
          Fiber.yield ();
          (loop [@tailcall]) next )
    in
    loop raw_rows
  in

  match status with
  | All_buffered ->
    let sst = Promise.await sst_p in
    Queue.iter q ~f:(fun raw -> yield (Expert.parse_row_with_sst sst cell_parser raw))
  | Overflowed ->
    Printf.failwithf "SZXX: stream_rows_single_pass max_buffering exceeded %d."
      (Option.value max_buffering ~default:Int.max_value)
      ()
  | Got_SST sst -> (
    Queue.iter q ~f:(fun row -> yield (Expert.parse_row_with_sst sst cell_parser row));
    match filter with
    | None -> Seq.iter (fun raw -> yield (Expert.parse_row_with_sst sst cell_parser raw)) raw_rows
    | Some filter ->
      Seq.iter
        (fun raw -> if filter raw then yield (Expert.parse_row_with_sst sst cell_parser raw))
        raw_rows )

let stream_rows_single_pass ?max_buffering ?filter ?filter_sheets ~sw ~feed cell_parser =
  Sequence.of_seq
  @@ Fiber.fork_seq ~sw
  @@ fun yield ->
  let ((sst_p, _) as p) = Promise.create () in
  Fiber.fork_seq ~sw (process_file ?filter_sheets ~sw ~feed p)
  |> with_minimal_buffering ?max_buffering ?filter cell_parser sst_p yield

let unescape = Xml.DOM.unescape

let string_cell_parser : string cell_parser =
  {
    string = (fun _location s -> unescape s);
    formula = (fun _location ~formula:_ s -> unescape s);
    error = (fun _location ~formula s -> Printf.sprintf !"#ERROR# %{unescape} -> %{unescape}" formula s);
    boolean = (fun _location s -> if String.(s = "0") then "false" else "true");
    number = (fun _location s -> s);
    date = (fun _location s -> s);
    null = "";
  }

let yojson_cell_parser : [> `Bool of bool | `Float of float | `String of string | `Null ] cell_parser =
  {
    string = (fun _location s -> `String (unescape s));
    formula = (fun _location ~formula:_ s -> `String (unescape s));
    error =
      (fun _location ~formula s ->
        `String (Printf.sprintf !"#ERROR# %{unescape} -> %{unescape}" formula s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    date = (fun _location s -> `String s);
    null = `Null;
  }
