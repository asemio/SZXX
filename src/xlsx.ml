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
           | Error msg -> failwithf "SZXX: File '%s' error: %s" filename msg () )
         | _ -> () );

    Queue.to_array q

  let from_file file =
    match
      Zip.index_entries file |> List.find ~f:(fun entry -> String.( = ) entry.filename zip_entry_filename)
    with
    | None -> [||]
    | Some entry -> (
      let q = Queue.create () in
      let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
      let action = fold_angstrom ~filter_path ~on_match () in
      match Zip.extract_from_index file entry action with
      | Zip.Data.Parse_many state ->
        (match Zip.Data.parser_state_to_result state with
        | Ok () -> ()
        | Error msg -> failwithf "SZXX: File '%s' error: %s" entry.filename msg ());
        Queue.to_array q
      | _ -> assert false )

  let resolve_sst_index (sst : t) ~sst_index =
    let index = Int.of_string sst_index in
    match sst with
    | sst when index < Array.length sst && index >= 0 -> Some (force sst.(index))
    | _ -> None
end

let flush_zip_seq ~sw zip_seq finalize =
  let sst_p, sst_w = Promise.create () in
  let flushed_p =
    Fiber.fork_promise ~sw (fun () ->
      Sequence.iter zip_seq ~f:(function
        | Zip.{ filename = "xl/sharedStrings.xml"; _ }, Zip.Data.Fast_skip -> Promise.resolve sst_w ()
        | Zip.{ filename = "xl/sharedStrings.xml"; _ }, Zip.Data.Parse_many state -> (
          match Zip.Data.parser_state_to_result state with
          | Ok () -> Promise.resolve sst_w ()
          | Error msg -> failwithf "SZXX: File '%s': %s" SST.zip_entry_filename msg () )
        | Zip.{ filename; _ }, Zip.Data.Parse_many state -> (
          match Zip.Data.parser_state_to_result state with
          | Ok () -> ()
          | Error msg -> failwithf "SZXX: File '%s': %s" filename msg () )
        | _ -> () );

      if not (Promise.is_resolved sst_p) then Promise.resolve sst_w ();

      finalize () )
  in
  sst_p, flushed_p

let process_file ?filter_sheets ~skip_sst ~sw ~feed push finalize =
  let q = Queue.create () in
  let zip_seq =
    Zip.stream_files ~sw ~feed (function
      | { filename = "xl/sharedStrings.xml"; _ } when skip_sst -> Fast_skip
      | { filename = "xl/sharedStrings.xml"; _ } ->
        let on_match el = Queue.enqueue q (lazy (parse_string_cell el)) in
        fold_angstrom ~filter_path:SST.filter_path ~on_match ()
      | { filename; descriptor = { uncompressed_size; _ }; _ } ->
        let open Option.Monad_infix in
        String.chop_prefix ~prefix:"xl/worksheets/sheet" filename
        >>= String.chop_suffix ~suffix:".xml"
        >>= (fun s -> Option.try_with (fun () -> Int.of_string s))
        |> Option.filter ~f:(fun sheet_id ->
             Option.value_map filter_sheets ~default:true ~f:(fun f ->
               f ~sheet_id ~raw_size:(Byte_units.of_bytes_int64_exn uncompressed_size) ) )
        >>| (fun sheet_number -> parse_sheet ~sheet_number push)
        |> Option.value ~default:Fast_skip )
  in
  let sst_p, flushed_p = flush_zip_seq ~sw zip_seq finalize in
  Fiber.fork_daemon ~sw (fun () ->
    Promise.await_exn flushed_p;
    `Stop_daemon );
  Fiber.fork_promise ~sw (fun () ->
    Promise.await sst_p;
    Queue.to_array q )

let index_of_column s =
  let key =
    String.take_while s ~f:(function
      | 'A' .. 'Z' -> true
      | _ -> false )
  in
  String.fold key ~init:0 ~f:(fun acc c -> (acc * 26) + Char.to_int c - 64) - 1

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
        failwithf "Unknown data type: %s. Please report this bug. %s" t
          (sexp_of_element el |> Sexp.to_string)
          ()
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

let make_finalizer ~sw stream () =
  Eio.Stream.add stream None;
  (* Keep adding [None] in the background to allow the user to use
     the Sequence again even after it's been iterated over. It doesn't
     memoize the elements, but it does allow [Sequence.is_empty] to return [true]
     without deadlocking, and [Sequence.fold] to return its initial value
     without deadlocking, etc. *)
  Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Stream.add stream None
    done;
    `Stop_daemon )

let to_sequence stream = Seq.of_dispenser (fun () -> Eio.Stream.take stream) |> Sequence.of_seq

let stream_rows_double_pass ?filter_sheets ~sw file cell_parser =
  let sst = SST.from_file file in
  let stream = Eio.Stream.create 0 in
  let push x = Eio.Stream.add stream (Some (Expert.parse_row_with_sst sst cell_parser x)) in
  let finalize = make_finalizer ~sw stream in

  let _sst_p =
    let feed = Feed.of_flow file in
    process_file ?filter_sheets ~skip_sst:true ~sw ~feed push finalize
  in
  to_sequence stream

type status =
  | All_buffered
  | Overflowed
  | Got_SST of SST.t

let with_minimal_buffering ?max_buffering ?filter cell_parser stream sst_p =
  let q = Queue.create ?capacity:max_buffering () in
  let highwater =
    match max_buffering with
    | None -> Int.max_value
    | Some 0 ->
      (* The internal reader is waiting on [Eio.Stream.take] so the first row
         always ends up in the buffer unless it gets dropped by filter. *)
      1
    | Some x when Int.is_positive x -> x
    | Some x -> failwithf "SZXX: stream_rows_single_pass max_buffering: %d < 0" x ()
  in
  let seq = to_sequence stream in

  let status =
    let filter = Option.value filter ~default:(fun _ -> true) in
    let rec loop acc =
      match Promise.peek sst_p with
      | Some sst -> Got_SST (Result.ok_exn sst)
      | None -> (
        match Sequence.next acc with
        | None -> All_buffered
        | Some _ when Queue.length q = highwater -> Overflowed
        | Some (row, next) ->
          if filter row then Queue.enqueue q row;
          Fiber.yield ();
          (loop [@tailcall]) next )
    in
    loop seq
  in

  match status with
  | All_buffered ->
    let sst = Promise.await_exn sst_p in
    Seq.of_dispenser (fun () ->
      Queue.dequeue q |> Option.map ~f:(Expert.parse_row_with_sst sst cell_parser) )
    |> Sequence.of_seq
  | Overflowed ->
    failwithf "SZXX: stream_rows_single_pass max_buffering exceeded %d."
      (Option.value max_buffering ~default:Int.max_value)
      ()
  | Got_SST sst ->
    let seq2 =
      match filter with
      | None -> Sequence.map seq ~f:(Expert.parse_row_with_sst sst cell_parser)
      | Some filter ->
        Sequence.filter_map seq ~f:(fun raw ->
          if filter raw then Some (Expert.parse_row_with_sst sst cell_parser raw) else None )
    in
    if Queue.is_empty q
    then seq2
    else (
      let seq1 =
        Seq.of_dispenser (fun () ->
          Queue.dequeue q |> Option.map ~f:(Expert.parse_row_with_sst sst cell_parser) )
        |> Sequence.of_seq
      in
      Sequence.append seq1 seq2 )

let stream_rows_single_pass ?max_buffering ?filter ?filter_sheets ~sw ~feed cell_parser =
  let stream = Eio.Stream.create 0 in
  let push x = Eio.Stream.add stream (Some x) in
  let finalize = make_finalizer ~sw stream in

  let sst_p = process_file ?filter_sheets ~skip_sst:false ~sw ~feed push finalize in

  with_minimal_buffering ?max_buffering ?filter cell_parser stream sst_p

let unescape = Xml.DOM.unescape

let string_cell_parser : string cell_parser =
  {
    string = (fun _location s -> unescape s);
    formula = (fun _location ~formula:_ s -> unescape s);
    error = (fun _location ~formula s -> sprintf !"#ERROR# %{unescape} -> %{unescape}" formula s);
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
      (fun _location ~formula s -> `String (sprintf !"#ERROR# %{unescape} -> %{unescape}" formula s));
    boolean = (fun _location s -> `Bool String.(s = "1"));
    number = (fun _location s -> `Float (Float.of_string s));
    date = (fun _location s -> `String s);
    null = `Null;
  }
