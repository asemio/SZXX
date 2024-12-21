module STR = String
open! Core
open Angstrom

type attr_list = (string * string) list [@@deriving sexp_of]

let get_attr attrs name = List.find_map attrs ~f:(fun (x, y) -> Option.some_if String.(x = name) y)

let rec preserve_space = function
| [] -> false
| ("xml:space", "preserve") :: _ -> true
| _ :: rest -> (preserve_space [@tailcall]) rest

module DOM = struct
  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element array;
  }
  [@@deriving sexp_of]

  let dot tag node = Array.find node.children ~f:(fun x -> String.(x.tag = tag))

  let dot_text tag node =
    Array.find_map node.children ~f:(function
      | x when String.(x.tag = tag) -> Some x.text
      | _ -> None )

  let filter_map tag ~f node =
    Array.filter_map node.children ~f:(function
      | x when String.(x.tag = tag) -> f x
      | _ -> None )

  let at i node = Option.try_with (fun () -> Int.of_string i |> Array.get node.children)

  let get node (steps : (element -> element option) list) =
    let rec loop node = function
      | [] -> node
      | step :: rest -> (loop [@tailcall]) (Option.bind node ~f:step) rest
    in
    loop (Some node) steps
end

module SAX = struct
  type node =
    | Prologue of attr_list
    | Element_open of {
        tag: string;
        attrs: attr_list;
      }
    | Element_close of string
    | Text of string
    | Cdata of string
    | Nothing
    | Many of node list
  [@@deriving sexp_of]

  type partial_text = {
    literal: bool;
    raw: string;
  }
  [@@deriving sexp_of]

  type partial = {
    tag: string;
    attrs: attr_list;
    text: partial_text Queue.t;
    children: DOM.element Queue.t;
    staged: DOM.element Queue.t;
  }
  [@@deriving sexp_of]

  let make_partial tag attrs =
    {
      tag;
      attrs;
      text = Queue.create ~capacity:4 ();
      children = Queue.create ~capacity:4 ();
      staged = Queue.create ~capacity:2 ();
    }

  let is_ws = function
  | '\x20'
   |'\x0d'
   |'\x09'
   |'\x0a' ->
    true
  | _ -> false

  let squish_into raw final acc =
    String.fold raw ~init:acc ~f:(fun acc c ->
      match acc with
      | after_first, _ when is_ws c -> after_first, true
      | true, true ->
        Buffer.add_char final ' ';
        Buffer.add_char final c;
        true, false
      | _ ->
        Buffer.add_char final c;
        true, false )

  let render attrs text =
    match Queue.length text with
    | 0 -> ""
    | 1 when (Queue.get text 0).literal -> (Queue.get text 0).raw
    | _ ->
      let final = Buffer.create 16 in
      let preserve_space = lazy (preserve_space attrs) in
      let _prev_ws =
        Queue.fold text ~init:(false, false) ~f:(fun ((after_first, prev_ws) as acc) -> function
          | { raw = ""; _ } -> acc
          | { literal; raw } when literal || force preserve_space ->
            if after_first && not prev_ws then Buffer.add_char final ' ';
            Buffer.add_string final raw;
            true, is_ws raw.[String.length raw - 1]
          | { raw; _ } -> squish_into raw final (if after_first then true, true else acc) )
      in
      Buffer.contents final

  let partial_to_element { tag; attrs; text; children; _ } =
    DOM.{ tag; attrs; text = render attrs text; children = Queue.to_array children }

  module To_DOM = struct
    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      top: DOM.element option;
    }
    [@@deriving sexp_of]

    let init = { decl_attrs = None; stack = []; top = None }

    let rec folder ?(strict = true) acc (node : node) =
      match node, acc with
      | _, Error _ -> acc
      | Nothing, Ok _ -> acc
      | Many ll, Ok acc -> List.fold_result ll ~init:acc ~f:(fun acc x -> folder ~strict (Ok acc) x)
      | Prologue attrs, Ok ({ decl_attrs = None; stack = []; top = None; _ } as acc) ->
        Ok { acc with decl_attrs = Some attrs }
      | Prologue _, Ok _ -> Error "The prologue must located at the beginning of the file"
      | Element_open { tag; attrs }, Ok acc ->
        let partial = make_partial tag attrs in
        Ok { acc with stack = partial :: acc.stack }
      | Text _, Ok { stack = []; top = None; _ }
       |Cdata _, Ok { stack = []; top = None; _ } ->
        acc
      | Text _, Ok { stack = []; top = Some _; _ }
       |Cdata _, Ok { stack = []; top = Some _; _ } ->
        acc
      | Text s, Ok { stack = current :: _; _ } ->
        Queue.enqueue current.text { raw = s; literal = false };
        acc
      | Cdata s, Ok { stack = current :: _; _ } ->
        Queue.enqueue current.text { raw = s; literal = true };
        acc
      | Element_close tag, Ok ({ stack = current :: (parent :: _ as rest); _ } as acc)
        when String.( = ) tag current.tag ->
        Queue.enqueue parent.children (partial_to_element current);
        Queue.blit_transfer ~src:parent.staged ~dst:parent.children ();
        Ok { acc with stack = rest }
      | Element_close tag, Ok ({ stack = [ current ]; top = None; _ } as acc)
        when String.( = ) tag current.tag ->
        Ok { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, Ok { stack = current :: _ :: _; _ } when strict ->
        Error (sprintf "Invalid document. Closing element '%s' before element '%s'" tag current.tag)
      | (Element_close _ as tried), Ok { stack = current :: parent :: _; _ } ->
        Queue.blit_transfer ~src:current.children ~dst:parent.staged ();
        Queue.blit_transfer ~src:current.text ~dst:parent.text ();
        folder ~strict acc (Many [ Element_close current.tag; tried ])
      | Element_close tag, Ok _ ->
        Error (sprintf "Invalid document. Closing tag without a matching opening tag: '%s'" tag)
  end

  module Stream = struct
    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      path_stack: string list;
      top: DOM.element option;
    }
    [@@deriving sexp_of]

    let init = { decl_attrs = None; stack = []; path_stack = [ "" ]; top = None }

    let rec folder ~filter_path ~on_match ~strict acc (node : node) =
      match node, acc with
      | _, Error _ -> acc
      | Nothing, Ok _ -> acc
      | Many ll, Ok acc ->
        List.fold_result ll ~init:acc ~f:(fun acc x -> folder ~filter_path ~on_match ~strict (Ok acc) x)
      | Prologue attrs, Ok ({ decl_attrs = None; stack = []; top = None; _ } as acc) ->
        Ok { acc with decl_attrs = Some attrs }
      | Prologue _, Ok _ -> Error "The prologue must located at the beginning of the file"
      | Element_open { tag; attrs }, Ok ({ path_stack = path :: _; _ } as acc) ->
        let partial = make_partial tag attrs in
        Ok
          {
            acc with
            stack = partial :: acc.stack;
            path_stack = sprintf "%s %s" path tag :: acc.path_stack;
          }
      | Element_open _, Ok { path_stack = []; _ } ->
        Error "Impossible case. Path stack cannot be empty. Please report this bug."
      | Text _, Ok { stack = []; top = None; _ }
       |Cdata _, Ok { stack = []; top = None; _ } ->
        acc
      | Text s, Ok { stack = current :: _; path_stack = path :: _; _ }
        when String.is_prefix path ~prefix:filter_path ->
        Queue.enqueue current.text { raw = s; literal = false };
        acc
      | Cdata s, Ok { stack = current :: _; path_stack = path :: _; _ }
        when String.is_prefix path ~prefix:filter_path ->
        Queue.enqueue current.text { raw = s; literal = true };
        acc
      | Text _, Ok _
       |Cdata _, Ok _ ->
        acc
      | ( Element_close tag,
          Ok ({ stack = current :: (parent :: _ as rest); path_stack = path :: path_rest; _ } as acc) )
        when String.( = ) tag current.tag ->
        (match String.is_prefix path ~prefix:filter_path with
        | true when String.length path = String.length filter_path ->
          (* Exactly filter_path *)
          on_match (partial_to_element current)
        | true ->
          (* Children of filter_path *)
          Queue.enqueue parent.children (partial_to_element current)
        | false -> ());
        Queue.blit_transfer ~src:parent.staged ~dst:parent.children ();
        Ok { acc with stack = rest; path_stack = path_rest }
      | Element_close tag, Ok ({ stack = [ current ]; top = None; _ } as acc)
        when String.( = ) tag current.tag ->
        Ok { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, Ok { stack = current :: _ :: _; _ } when strict ->
        Error (sprintf "Invalid document. Closing element '%s' before element '%s'" tag current.tag)
      | (Element_close _ as tried), Ok { stack = current :: parent :: _; _ } ->
        Queue.blit_transfer ~src:current.children ~dst:parent.staged ();
        Queue.blit_transfer ~src:current.text ~dst:parent.text ();
        folder ~filter_path ~on_match ~strict acc (Many [ Element_close current.tag; tried ])
      | Element_close tag, Ok _ ->
        Error (sprintf "Invalid document. Closing tag without a matching opening tag: '%s'" tag)

    let folder ~filter_path:path_list ~on_match ?(strict = true) acc node =
      let filter_path =
        let buf = Buffer.create 30 in
        List.iter path_list ~f:(fun s ->
          Buffer.add_char buf ' ';
          Buffer.add_string buf s );
        Buffer.contents buf
      in
      folder ~filter_path ~on_match ~strict acc (node : node)
  end
end

type utf8_consts = {
  shifts: int array;
  masks: int array;
  blends: int array;
}

let encode_utf_8_codepoint code =
  let consts =
    (* https://en.wikipedia.org/wiki/UTF-8#Encoding *)
    [|
      { shifts = [| 0 |]; masks = [| 127 |]; blends = [| 0 |] };
      { shifts = [| 6; 0 |]; masks = [| 31; 63 |]; blends = [| 192; 128 |] };
      { shifts = [| 12; 6; 0 |]; masks = [| 15; 63; 63 |]; blends = [| 224; 128; 128 |] };
      { shifts = [| 18; 12; 6; 0 |]; masks = [| 7; 63; 63; 63 |]; blends = [| 240; 128; 128; 128 |] };
    |]
  in
  match code with
  | code when code <= 0x7f -> Char.unsafe_of_int code |> String.of_char
  | code ->
    let num_bytes =
      match code with
      | x when x <= 0x7ff -> 2
      | x when x <= 0xffff -> 3
      | _ -> 4
    in
    let consts = consts.(num_bytes - 1) in
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
  | _ -> raise (Invalid_argument str) )

let unescape original =
  let rec loop buf from =
    match String.index_from original from '&' with
    | None -> Buffer.add_substring buf original ~pos:from ~len:(String.length original - from)
    | Some start -> (
      match String.index_from original start ';' with
      | None -> Buffer.add_substring buf original ~pos:from ~len:(String.length original - from)
      | Some stop ->
        Buffer.add_substring buf original ~pos:from ~len:(start - from);
        (match decode_exn (String.slice original (start + 1) stop) with
        | s -> Buffer.add_string buf s
        | exception _ -> Buffer.add_substring buf original ~pos:start ~len:(stop - start + 1));
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
 |'\x0d'
 |'\x09'
 |'\x0a' ->
  false
| _ -> true

let is_text = function
| '<' -> false
| _ -> true

let drop p = p >>| const ()

let make_string_parser ~separator = char separator *> take_till (Char.( = ) separator) <* char separator

let ws = skip_while SAX.is_ws

let comment = string "<!--" *> Parsing.skip_until_pattern ~pattern:"-->"

let blank = skip_many (ws *> comment) *> ws

let token_parser = take_while1 is_token

type parser_options = {
  accept_html_boolean_attributes: bool;
  accept_unquoted_attributes: bool;
}

let default_parser_options = { accept_html_boolean_attributes = true; accept_unquoted_attributes = false }

let make_parser { accept_html_boolean_attributes; accept_unquoted_attributes } =
  let xml_string_parser =
    let dq_string = make_string_parser ~separator:'"' in
    let sq_string = make_string_parser ~separator:'\'' in
    if accept_unquoted_attributes
    then (
      let uq_string =
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
      choice [ dq_string; sq_string; uq_string ] )
    else choice [ dq_string; sq_string ]
  in
  let attr_parser =
    let value_parser = ws *> char '=' *> ws *> xml_string_parser in
    lift2 Tuple2.create token_parser
      (if accept_html_boolean_attributes then option "" value_parser else value_parser)
  in
  let doctype_parser =
    let declaration =
      string "<!" *> token_parser *> ws *> skip_many (ws *> (token_parser <|> xml_string_parser))
      <* ws
      <* char '>'
    in
    let declarations = char '[' *> skip_many (blank *> declaration) <* blank <* char ']' in
    (string "<!DOCTYPE" <|> string "<!doctype")
    *> ( skip_many (blank *> choice [ drop token_parser; drop xml_string_parser; declarations ])
       <* blank
       <* char '>' )
    >>| const SAX.Nothing
  in
  let comment_parser = comment >>| const SAX.Nothing in
  let prologue_parser =
    (* UTF-8 BOM *)
    option "" (string "\xEF\xBB\xBF")
    *> blank
    *> string "<?xml "
    *> (many (ws *> attr_parser) >>| fun attrs -> SAX.Prologue attrs)
    <* ws
    <* string "?>"
  in
  let cdata_parser =
    string "<![CDATA[" *> Parsing.take_until_pattern ~pattern:"]]>" >>| fun s -> SAX.Cdata s
  in
  let element_open_parser =
    lift3
      (fun tag attrs self_closing ->
        let eopen = SAX.Element_open { tag; attrs } in
        if self_closing then SAX.Many [ eopen; Element_close tag ] else eopen)
      (char '<' *> ws *> token_parser)
      (many (ws *> attr_parser) <* ws)
      (option false (char '/' >>| const true) <* char '>')
  in
  let element_close_parser =
    string "</" *> ws *> (token_parser >>| fun s -> SAX.Element_close s) <* (ws <* char '>')
  in
  let text_parser = take_while1 is_text >>| fun s -> SAX.Text s in
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
    peek_string 2 >>= function
    | "</" -> element_close_parser
    | "<!" -> choice [ comment_parser; cdata_parser; doctype_parser ]
    | "<?"
     |"\xEF\xBB" ->
      prologue_parser
    | s when Char.(s.[0] = '<') -> element_open_parser
    | _ -> text_parser
  in
  fast_path <|> slow_path

let parser = make_parser default_parser_options
