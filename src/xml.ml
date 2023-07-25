open! Core

module Unescape = struct
  type utf8_consts = {
    shifts: int array;
    masks: int array;
    blends: int array;
  }

  let consts =
    (* https://en.wikipedia.org/wiki/UTF-8#Encoding *)
    [|
      { shifts = [| 6; 0 |]; masks = [| 31; 63 |]; blends = [| 192; 128 |] };
      { shifts = [| 12; 6; 0 |]; masks = [| 15; 63; 63 |]; blends = [| 224; 128; 128 |] };
      { shifts = [| 18; 12; 6; 0 |]; masks = [| 7; 63; 63; 63 |]; blends = [| 240; 128; 128; 128 |] };
    |]

  let encode_utf_8_codepoint = function
  | code when code <= 0x7f -> Char.unsafe_of_int code |> String.of_char
  | code ->
    let num_bytes =
      match code with
      | x when x <= 0x7ff -> 2
      | x when x <= 0xffff -> 3
      | _ -> 4
    in
    let consts = consts.(num_bytes - 2) in
    String.init num_bytes ~f:(fun i ->
      (* lsr: Shift the relevant bits to the least significant position *)
      (* land: Blank out the reserved bits *)
      (* lor: Set the reserved bits to the right value *)
      (code lsr consts.shifts.(i)) land consts.masks.(i) lor consts.blends.(i) |> Char.unsafe_of_int )

  let decode_exn = function
  (* https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references#List_of_predefined_entities_in_XML *)
  | "amp" -> "&"
  | "lt" -> "<"
  | "gt" -> ">"
  | "apos" -> "'"
  | "quot" -> "\""
  | str -> (
    match str.[0], str.[1] with
    | '#', 'x' ->
      let b = Bytes.of_string str in
      Bytes.set b 0 '0';
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
      |> Int.of_string
      |> encode_utf_8_codepoint
    | '#', '0' .. '9' -> String.slice str 1 0 |> Int.of_string |> encode_utf_8_codepoint
    | _ -> raise (Invalid_argument (sprintf "Not a valid XML-encoded entity: '%s'" str)) )

  let run original =
    let rec loop buf from =
      match String.index_from original from '&' with
      | None -> Buffer.add_substring buf original ~pos:from ~len:(String.length original - from)
      | Some start -> (
        match String.index_from original start ';' with
        | None -> Buffer.add_substring buf original ~pos:from ~len:(String.length original - from)
        | Some stop ->
          Buffer.add_substring buf original ~pos:from ~len:(start - from);
          (try
             let s = decode_exn (String.slice original (start + 1) stop) in
             Buffer.add_string buf s
           with
          | _ -> Buffer.add_substring buf original ~pos:start ~len:(stop - start + 1));
          loop buf (stop + 1) )
    in
    (* Unroll first index call for performance *)
    match String.index original '&' with
    | None -> original
    | Some start ->
      let buf = Buffer.create (String.length original) in
      Buffer.add_substring buf original ~pos:0 ~len:start;
      loop buf start;
      Buffer.contents buf
end

module DOM = struct
  type attr_list = (string * string) list [@@deriving sexp_of, compare, equal]

  let get_attr attrs name = List.find_map attrs ~f:(fun (x, y) -> Option.some_if String.(x = name) y)

  let rec preserve_space = function
  | [] -> false
  | ("xml:space", "preserve") :: _ -> true
  | _ :: rest -> (preserve_space [@tailcall]) rest

  let unescape = Unescape.run

  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element list;
  }
  [@@deriving sexp_of, compare, equal]

  let dot tag node = List.find node.children ~f:(fun x -> String.(x.tag = tag))

  let dot_text tag node =
    List.find_map node.children ~f:(function
      | x when String.(x.tag = tag) -> Some x.text
      | _ -> None )

  let filter_map tag ~f node =
    List.filter_map node.children ~f:(function
      | x when String.(x.tag = tag) -> f x
      | _ -> None )

  let at i node = Option.try_with (fun () -> List.nth_exn node.children i)

  let at_s s node = at (Int.of_string s) node

  let get (steps : (element -> element option) list) node =
    let rec loop acc = function
      | [] -> Some acc
      | step :: rest -> (
        match step acc with
        | None -> None
        | Some x -> (loop [@tailcall]) x rest )
    in
    loop node steps
end

module Parser = struct
  open Angstrom
  open Parsing

  module Types = struct
    type node =
      | Prologue of DOM.attr_list
      | Element_open of {
          tag: string;
          attrs: DOM.attr_list;
        }
      | Element_close of string
      | Text of string
      | Cdata of string
      | Nothing
      | Many of node list
    [@@deriving sexp_of, compare, equal]

    type parser_options = {
      accept_html_boolean_attributes: bool;
      accept_unquoted_attributes: bool;
      accept_single_quoted_attributes: bool;
    }
    [@@deriving sexp_of, compare, equal]
  end

  open Types

  let is_token = function
  | '"'
   |'='
   |'<'
   |'?'
   |'!'
   |'/'
   |'>'
   |'['
   |']'
   |'\x20'
   |'\x0a' ->
    false
  | _ -> true

  let is_text = function
  | '<' -> false
  | _ -> true

  let is_ws = function
  | '\x20'
   |'\x0d'
   |'\x09'
   |'\x0a' ->
    true
  | _ -> false

  let make_string_parser ~separator = char separator *> take_till (Char.( = ) separator) <* char separator

  let ws = skip_while is_ws

  let comment =
    string "<!--" *> bounded_file_reader ~slice_size:Int.(2 ** 8) ~pattern:"-->" Storage.noop_backtrack

  let blank = skip_many (ws *> comment) *> ws

  let token_parser = take_while1 is_token

  let xml_string_parser options =
    let dq_string = make_string_parser ~separator:'"' in
    let sq_string () = make_string_parser ~separator:'\'' in
    let uq_string () =
      take_while1 (function
        | ' '
         |'"'
         |'\''
         |'='
         |'<'
         |'>'
         |'`' ->
          false
        | _ -> true )
    in
    match options.accept_unquoted_attributes, options.accept_single_quoted_attributes with
    | true, true -> choice [ dq_string; sq_string (); uq_string () ]
    | true, false -> choice [ dq_string; uq_string () ]
    | false, true -> choice [ dq_string; sq_string () ]
    | false, false -> dq_string

  let attr_parser ~xml_string_parser options =
    let value_parser = ws *> char '=' *> ws *> xml_string_parser in
    let* attr = token_parser in
    let+ v = if options.accept_html_boolean_attributes then option attr value_parser else value_parser in
    attr, v

  let doctype_parser ~xml_string_parser =
    let declaration =
      string "<!" *> token_parser *> ws *> skip_many (ws *> (token_parser <|> xml_string_parser))
      <* ws
      <* char '>'
    in
    let declarations = char '[' *> skip_many (blank *> declaration) <* blank <* char ']' in
    string_ci "<!DOCTYPE"
    *> ( skip_many (blank *> choice [ declarations; drop token_parser; drop xml_string_parser ])
       <* blank
       <* char '>' )
    *> return Nothing

  let comment_parser = comment *> return Nothing

  let prologue_parser ~attr_parser =
    (* UTF-8 BOM *)
    option () (string "\xEF\xBB\xBF")
    *> blank
    *> string "<?xml "
    *> (many (ws *> attr_parser) >>| fun attrs -> Prologue attrs)
    <* ws
    <* string "?>"

  let cdata_parser =
    string_ci "<![CDATA[" *> take_until_pattern ~slice_size:Int.(2 ** 10) ~pattern:"]]>" >>| fun s ->
    Cdata s

  let element_open_parser ~attr_parser =
    let+ tag = char '<' *> ws *> token_parser
    and+ attrs = many (ws *> attr_parser) <* ws
    and+ self_closing = option false (char '/' *> ws *> return_true) <* char '>' in
    let eopen = Element_open { tag; attrs } in
    if self_closing then Many [ eopen; Element_close tag ] else eopen

  let element_close_parser =
    string "</" *> ws *> (token_parser >>| fun s -> Element_close s) <* (ws <* char '>')

  let text_parser = take_while1 is_text >>| fun s -> Text s

  let create options =
    let ( .*{} ) = Bigstringaf.unsafe_get in
    let xml_string_parser = xml_string_parser options in
    let attr_parser = attr_parser ~xml_string_parser options in
    let element_open_parser = element_open_parser ~attr_parser in
    let prologue_parser = prologue_parser ~attr_parser in
    let doctype_parser = doctype_parser ~xml_string_parser in
    let slow_path =
      choice
        [
          element_close_parser;
          element_open_parser;
          text_parser;
          comment_parser;
          cdata_parser;
          prologue_parser;
          doctype_parser;
        ]
    in
    let fast_path =
      Unsafe.peek 2 (fun buf ~off ~len:_ ->
        if Char.(buf.*{off} = '<')
        then (
          match buf.*{off + 1} with
          | '/' -> element_close_parser
          | '!' -> choice [ comment_parser; cdata_parser; doctype_parser ]
          | '?' -> prologue_parser
          | _ -> element_open_parser )
        else if Char.(buf.*{off} = '\xEF' && buf.*{off + 1} = '\xBB')
        then prologue_parser
        else text_parser )
      >>= Fn.id
    in
    fast_path <|> slow_path
end

module SAX = struct
  include Parser.Types

  let default_parser_options =
    {
      accept_html_boolean_attributes = true;
      accept_unquoted_attributes = false;
      accept_single_quoted_attributes = false;
    }

  let parser = Parser.create default_parser_options

  let make_parser = Parser.create

  module Expert = struct
    type partial_text = {
      literal: bool;
      raw: string;
    }
    [@@deriving sexp_of, compare, equal]

    type partial = {
      tag: string;
      tag_hash: int;
      attrs: DOM.attr_list;
      text: partial_text list;
      children: DOM.element list;
      staged: DOM.element list;
    }
    [@@deriving sexp_of, compare, equal]

    let make_partial tag attrs =
      { tag; tag_hash = [%hash: string] tag; attrs; text = []; children = []; staged = [] }

    let squish_into raw final acc =
      String.fold raw ~init:acc ~f:(fun acc c ->
        match acc with
        | after_first, _ when Parser.is_ws c -> after_first, true
        | true, true ->
          bprintf final " %c" c;
          true, false
        | _ ->
          Buffer.add_char final c;
          true, false )

    let render attrs = function
    | [] -> ""
    | ll ->
      let final = Buffer.create 32 in
      let _prev_ws =
        List.fold_right ll ~init:(false, false) ~f:(fun partial ((after_first, prev_ws) as acc) ->
          match partial with
          | { literal = true; raw } ->
            Buffer.add_string final raw;
            true, true
          | { raw; _ } when DOM.preserve_space attrs ->
            if after_first && not prev_ws then Buffer.add_char final ' ';
            Buffer.add_string final raw;
            true, Parser.is_ws raw.[String.length raw - 1]
          | { raw; _ } -> squish_into raw final (if after_first then true, true else acc) )
      in
      Buffer.contents final

    let partial_to_element { tag; attrs; text; children; _ } =
      DOM.{ tag; attrs; text = render attrs text; children = List.rev children }

    module To_DOM = struct
      type state = {
        decl_attrs: DOM.attr_list option;
        stack: partial list;
        top: DOM.element option;
      }
      [@@deriving sexp_of, compare, equal]

      let init = { decl_attrs = None; stack = []; top = None }

      let rec folder ?(strict = true) acc (node : node) =
        match node, acc with
        | Nothing, acc -> acc
        | Many ll, acc -> List.fold ll ~init:acc ~f:(fun acc x -> (folder [@tailcall]) ~strict acc x)
        | Prologue attrs, ({ decl_attrs = None; stack = []; top = None; _ } as acc) ->
          { acc with decl_attrs = Some attrs }
        | Prologue _, _ ->
          failwith "SZXX: Invalid XML document. The prologue must located at the beginning of the file"
        | Element_open { tag; attrs }, acc ->
          let partial = make_partial tag attrs in
          { acc with stack = partial :: acc.stack }
        | Text _, { stack = []; top = None; _ }
         |Cdata _, { stack = []; top = None; _ } ->
          acc
        | Text _, { stack = []; top = Some _; _ }
         |Cdata _, { stack = []; top = Some _; _ } ->
          acc
        | Text s, ({ stack = current :: rest; _ } as acc) ->
          {
            acc with
            stack = { current with text = { raw = s; literal = false } :: current.text } :: rest;
          }
        | Cdata s, ({ stack = current :: rest; _ } as acc) ->
          { acc with stack = { current with text = { raw = s; literal = true } :: current.text } :: rest }
        | Element_close tag, ({ stack = current :: parent :: rest; _ } as acc)
          when String.( = ) tag current.tag ->
          {
            acc with
            stack =
              {
                parent with
                children = parent.staged @ (partial_to_element current :: parent.children);
                staged = [];
              }
              :: rest;
          }
        | Element_close tag, ({ stack = [ current ]; top = None; _ } as acc)
          when String.( = ) tag current.tag ->
          { acc with stack = []; top = Some (partial_to_element current) }
        | Element_close tag, { stack = current :: _ :: _; _ } when strict ->
          failwithf "SZXX: Invalid XML document. Closing element '%s' before element '%s'" tag current.tag
            ()
        | (Element_close _ as tried), ({ stack = current :: parent :: rest; _ } as acc) ->
          let acc =
            {
              acc with
              stack =
                { current with text = []; children = [] }
                :: {
                     parent with
                     text = current.text @ parent.text;
                     staged = current.children @ parent.staged;
                   }
                :: rest;
            }
          in
          (folder [@tailcall]) ~strict acc (Many [ Element_close current.tag; tried ])
        | Element_close tag, _ ->
          failwithf "SZXX: Invalid XML document. Closing tag without a matching opening tag: '%s'" tag ()
    end

    module Stream = struct
      module Hash_state = struct
        type t = Hash.state

        let sexp_of_t x = [%sexp_of: int] (Hash.get_hash_value x)

        let compare a b = [%compare: int] (Hash.get_hash_value a) (Hash.get_hash_value b)

        let equal a b = [%equal: int] (Hash.get_hash_value a) (Hash.get_hash_value b)
      end

      type state = {
        decl_attrs: DOM.attr_list option;
        stack: partial list;
        path_stack: (int * Hash_state.t * int) list;
        top: DOM.element option;
      }
      [@@deriving sexp_of, compare, equal]

      let init =
        let hash = Hash.create () in
        { decl_attrs = None; stack = []; path_stack = [ Hash.get_hash_value hash, hash, 0 ]; top = None }

      let rec folder ~filter_path ~filter_level ~on_match ~strict acc (node : node) =
        match node, acc with
        | Nothing, acc -> acc
        | Many ll, acc ->
          List.fold ll ~init:acc ~f:(fun acc x ->
            (folder [@tailcall]) ~filter_path ~filter_level ~on_match ~strict acc x )
        | Prologue attrs, ({ decl_attrs = None; stack = []; top = None; _ } as acc) ->
          { acc with decl_attrs = Some attrs }
        | Prologue _, _ ->
          failwith "SZXX: Invalid XML document. The prologue must located at the beginning of the file"
        | Element_open { tag; attrs }, ({ stack; path_stack = (path, hash, level) :: _; _ } as acc) ->
          let partial = make_partial tag attrs in
          let frame =
            if path = filter_path
            then path, hash, level + 1
            else (
              let hash = [%hash_fold: string] hash tag in
              Hash.get_hash_value hash, hash, level + 1 )
          in
          { acc with stack = partial :: stack; path_stack = frame :: acc.path_stack }
        | Element_open _, { path_stack = []; _ } ->
          failwith "Impossible case. Path stack cannot be empty. Please report this bug."
        | Text _, { stack = []; top = None; _ }
         |Cdata _, { stack = []; top = None; _ } ->
          acc
        | Text s, ({ stack = current :: rest; path_stack = (path, _, _) :: _; _ } as acc)
          when path = filter_path ->
          {
            acc with
            stack = { current with text = { raw = s; literal = false } :: current.text } :: rest;
          }
        | Cdata s, ({ stack = current :: rest; path_stack = (path, _, _) :: _; _ } as acc)
          when path = filter_path ->
          { acc with stack = { current with text = { raw = s; literal = true } :: current.text } :: rest }
        | Text _, _
         |Cdata _, _ ->
          acc
        | ( Element_close tag,
            ({ stack = current :: parent :: rest; path_stack = (path, _, level) :: path_rest; _ } as acc)
          )
          when String.( = ) tag current.tag ->
          let parent_children =
            match path = filter_path with
            | true when level = filter_level ->
              (* Exactly filter_path *)
              on_match (partial_to_element current);
              parent.children
            | true ->
              (* Children of filter_path *)
              partial_to_element current :: parent.children
            | false -> parent.children
          in

          {
            acc with
            stack = { parent with staged = []; children = parent.staged @ parent_children } :: rest;
            path_stack = path_rest;
          }
        | Element_close tag, ({ stack = [ current ]; top = None; _ } as acc)
          when String.( = ) tag current.tag ->
          { acc with stack = []; top = Some (partial_to_element current) }
        | Element_close tag, { stack = current :: _ :: _; _ } when strict ->
          failwithf "SZXX: Invalid XML document. Closing element '%s' before element '%s'" tag current.tag
            ()
        | (Element_close _ as tried), ({ stack = current :: parent :: rest; _ } as acc) ->
          let acc =
            {
              acc with
              stack =
                { current with text = []; children = [] }
                :: {
                     parent with
                     text = current.text @ parent.text;
                     staged = current.children @ parent.staged;
                   }
                :: rest;
            }
          in
          (folder [@tailcall]) ~filter_path ~filter_level ~on_match ~strict acc
            (Many [ Element_close current.tag; tried ])
        | Element_close tag, _ ->
          failwithf "SZXX: Invalid XML document. Closing tag without a matching opening tag: '%s'" tag ()

      let folder ~filter_path:path_list ~on_match ?(strict = true) acc node =
        let filter_path, filter_level =
          List.fold path_list
            ~init:(Hash.create (), 0)
            ~f:(fun (h, len) x -> [%hash_fold: string] h x, len + 1)
          |> Tuple2.map_fst ~f:Hash.get_hash_value
        in
        folder ~filter_path ~filter_level ~on_match ~strict acc (node : node)
    end
  end
end

let html_parser =
  SAX.make_parser
    {
      accept_html_boolean_attributes = true;
      accept_unquoted_attributes = true;
      accept_single_quoted_attributes = true;
    }

type document = {
  decl_attrs: DOM.attr_list;
  top: DOM.element;
}
[@@deriving sexp_of, compare, equal]

let unescape_text = function
| SAX.Text s -> SAX.Text (Unescape.run s)
| x -> x

let invalid = function
| Ok x -> x
| Error msg -> failwithf "SZXX: Invalid XML structure. Error: %s" msg ()

let parse_feed ?parser:(node_parser = SAX.parser) ~feed:read on_parse =
  let open Angstrom in
  let open Parsing in
  let parser = skip_many (node_parser >>| on_parse) *> end_of_input in
  let open Buffered in
  let rec loop = function
    | Fail _ as state -> state_to_result state |> invalid
    | Done (_, ()) -> Error "SZXX: Processing completed before reaching end of input"
    | Partial feed -> (
      match read () with
      | `Eof as eof -> (
        match feed eof with
        | Fail _ as state -> state_to_result state |> invalid
        | _ -> Ok () )
      | chunk -> (loop [@tailcall]) (feed chunk) )
  in
  loop (parse parser)

let parse_document ?parser ?strict feed =
  let sax = ref SAX.Expert.To_DOM.init in
  let on_parse node = sax := SAX.Expert.To_DOM.folder ?strict !sax (unescape_text node) in
  let open Result.Monad_infix in
  parse_feed ?parser ~feed on_parse >>= fun () ->
  match !sax with
  | { stack = []; top = Some top; decl_attrs; _ } ->
    Ok { top; decl_attrs = Option.value ~default:[] decl_attrs }
  | { stack = []; top = None; _ } -> Error "SZXX: Empty XML document"
  | { stack = [ x ]; _ } -> Error (sprintf "SZXX: Unclosed XML element: %s" x.tag)
  | { stack; _ } ->
    Error
      (sprintf "SZXX: Unclosed XML elements: %s"
         (List.map stack ~f:(fun { tag; _ } -> tag) |> String.concat ~sep:", ") )
  | exception Failure msg -> Error msg

let parse_document_from_string ?parser ?strict raw =
  let feed = Feed.of_string raw in
  parse_document ?parser ?strict feed

let stream_matching_elements ?parser ?strict ~filter_path ~on_match feed =
  let sax = ref SAX.Expert.Stream.init in
  let on_parse node =
    sax := SAX.Expert.Stream.folder ~filter_path ~on_match ?strict !sax (unescape_text node)
  in
  let open Result.Monad_infix in
  parse_feed ?parser ~feed on_parse >>= fun () ->
  match !sax with
  | { stack = []; top = Some top; decl_attrs; _ } ->
    Ok { top; decl_attrs = Option.value ~default:[] decl_attrs }
  | { stack = []; top = None; _ } -> Error "SZXX: Empty XML document"
  | { stack = [ x ]; _ } -> Error (sprintf "SZXX: Unclosed XML element: %s" x.tag)
  | { stack; _ } ->
    Error
      (sprintf "SZXX: Unclosed XML elements: %s"
         (List.map stack ~f:(fun { tag; _ } -> tag) |> String.concat ~sep:", ") )
  | exception Failure msg -> Error msg
