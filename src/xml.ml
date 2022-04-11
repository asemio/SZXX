module STR = String
open! Core_kernel
open Angstrom

type attr_list = (string * string) list [@@deriving sexp_of]

let get_attr attrs name = List.find_map attrs ~f:(fun (x, y) -> Option.some_if String.(x = name) y)

module DOM = struct
  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element array;
  }
  [@@deriving sexp_of]

  type doc = {
    decl_attrs: attr_list option;
    top: element;
  }
  [@@deriving sexp_of]

  let dot tag node = Array.find node.children ~f:(fun x -> String.(x.tag = tag))

  let dot_text tag node =
    Array.find_map node.children ~f:(function
      | x when String.(x.tag = tag) -> Some x.text
      | _ -> None)

  let filter_map tag ~f node =
    Array.filter_map node.children ~f:(function
      | x when String.(x.tag = tag) -> f x
      | _ -> None)

  let at i node = Option.try_with (fun () -> Int.of_string i |> Array.get node.children)

  let get node (steps : (element -> element option) list) =
    let rec loop node = function
      | [] -> node
      | step :: rest -> (loop [@tailcall]) (Option.bind node ~f:step) rest
    in
    loop (Some node) steps
end

module SAX = struct
  type element_open = {
    tag: string;
    attrs: attr_list;
    preserve_space: bool Lazy.t;
  }
  [@@deriving sexp_of]

  type node =
    | Prologue      of attr_list
    | Element_open  of element_open
    | Element_close of string
    | Text          of string
    | Cdata         of string
    | Nothing
    | Many          of node list
  [@@deriving sexp_of]

  module To_DOM = struct
    type partial = {
      tag: string;
      attrs: attr_list;
      preserve_space: bool Lazy.t;
      buf: Buffer.t;
      children: DOM.element Queue.t;
    }
    [@@deriving sexp_of]

    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      top: DOM.element option;
    }
    [@@deriving sexp_of]

    let init = { decl_attrs = None; stack = []; top = None }

    let partial_to_element { tag; attrs; buf; children; _ } =
      DOM.{ tag; attrs; text = Buffer.contents buf; children = Queue.to_array children }

    let rec folder acc (node : node) =
      match node, acc with
      | _, Error _ -> acc
      | Nothing, Ok _ -> acc
      | Many ll, Ok acc -> List.fold_result ll ~init:acc ~f:(fun acc x -> folder (Ok acc) x)
      | Prologue _, Ok { decl_attrs = Some _; _ } -> Error "A prologue already exists"
      | Prologue attrs, Ok ({ decl_attrs = None; _ } as acc) -> Ok { acc with decl_attrs = Some attrs }
      | Element_open { tag; attrs; preserve_space }, Ok acc ->
        let partial =
          { tag; attrs; preserve_space; buf = Buffer.create 16; children = Queue.create ~capacity:4 () }
        in
        Ok { acc with stack = partial :: acc.stack }
      | Text s, Ok { stack = []; top = None; _ } ->
        Error (sprintf "Invalid document. Text not contained within an element: \"%s\"" s)
      | Cdata s, Ok { stack = []; top = None; _ } ->
        Error (sprintf "Invalid document. CDATA not contained within an element: \"%s\"" s)
      | Text _, Ok { stack = []; top = Some _; _ }
       |Cdata _, Ok { stack = []; top = Some _; _ } ->
        acc
      | Text s, Ok { stack = { buf; preserve_space; _ } :: _; _ }
       |Cdata s, Ok { stack = { buf; preserve_space; _ } :: _; _ } ->
        let preserve = force preserve_space in
        if Buffer.length buf > 0 && not (preserve && String.is_prefix s ~prefix:" ")
        then Buffer.add_char buf ' ';
        Buffer.add_string buf (if preserve then s else String.strip s);
        acc
      | Element_close tag, Ok ({ stack = current :: (parent :: _ as rest); _ } as acc)
        when String.( = ) tag current.tag ->
        Queue.enqueue parent.children (partial_to_element current);
        Ok { acc with stack = rest }
      | Element_close tag, Ok ({ stack = [ current ]; top = None; _ } as acc)
        when String.( = ) tag current.tag ->
        Ok { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, Ok { stack = current :: _ :: _; _ } ->
        Error (sprintf "Invalid document. Closing element \"%s\" before element \"%s\"" tag current.tag)
      | Element_close tag, Ok _ ->
        Error (sprintf "Invalid document. Closing tag without matching opening tag: %s" tag)
  end

  module Stream = struct
    type partial = {
      tag: string;
      attrs: attr_list;
      preserve_space: bool Lazy.t;
      buf: Buffer.t;
      children: DOM.element Queue.t;
    }
    [@@deriving sexp_of]

    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      path_stack: string list;
      top: DOM.element option;
    }
    [@@deriving sexp_of]

    let init = { decl_attrs = None; stack = []; path_stack = [ "" ]; top = None }

    let partial_to_element { tag; attrs; buf; children; _ } =
      DOM.{ tag; attrs; text = Buffer.contents buf; children = Queue.to_array children }

    let rec folder ~filter_path ~on_match acc (node : node) =
      match node, acc with
      | _, Error _ -> acc
      | Nothing, Ok _ -> acc
      | Many ll, Ok acc ->
        List.fold_result ll ~init:acc ~f:(fun acc x -> folder ~filter_path ~on_match (Ok acc) x)
      | Prologue _, Ok { decl_attrs = Some _; _ } -> Error "A prologue already exists"
      | Prologue attrs, Ok ({ decl_attrs = None; _ } as acc) -> Ok { acc with decl_attrs = Some attrs }
      | Element_open { tag; attrs; preserve_space }, Ok ({ path_stack = path :: _; _ } as acc) ->
        let partial =
          { tag; attrs; preserve_space; buf = Buffer.create 16; children = Queue.create ~capacity:4 () }
        in
        Ok
          {
            acc with
            stack = partial :: acc.stack;
            path_stack = sprintf "%s %s" path tag :: acc.path_stack;
          }
      | Element_open _, Ok { path_stack = []; _ } ->
        Error "Impossible case. Path stack cannot be empty. Please report this bug."
      | Text s, Ok { stack = []; top = None; _ } ->
        Error (sprintf "Invalid document. Text not contained within an element: %s" s)
      | Cdata s, Ok { stack = []; top = None; _ } ->
        Error (sprintf "Invalid document. CDATA not contained within an element: %s" s)
      | Text s, Ok { stack = { buf; preserve_space; _ } :: _; path_stack = path :: _; _ }
       |Cdata s, Ok { stack = { buf; preserve_space; _ } :: _; path_stack = path :: _; _ }
        when String.is_prefix path ~prefix:filter_path ->
        let preserve = force preserve_space in
        if Buffer.length buf > 0 && not (preserve && String.is_prefix s ~prefix:" ")
        then Buffer.add_char buf ' ';
        Buffer.add_string buf (if preserve then s else String.strip s);
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
        Ok { acc with stack = rest; path_stack = path_rest }
      | Element_close tag, Ok ({ stack = [ current ]; top = None; _ } as acc)
        when String.( = ) tag current.tag ->
        Ok { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, Ok { stack = current :: _ :: _; _ } ->
        Error (sprintf "Invalid document. Closing element '%s' before element '%s'" tag current.tag)
      | Element_close tag, Ok _ ->
        Error (sprintf "Invalid document. Closing tag without matching opening tag: %s" tag)

    let folder ~filter_path:path_list ~on_match acc node =
      let filter_path =
        let buf = Buffer.create 30 in
        List.iter path_list ~f:(fun s ->
            Buffer.add_char buf ' ';
            Buffer.add_string buf s);
        Buffer.contents buf
      in
      folder ~filter_path ~on_match acc (node : node)
  end
end

type utf8_consts = {
  shifts: int array;
  masks: int array;
  blends: int array;
}

let decode_exn =
  let consts =
    [|
      { shifts = [| 0 |]; masks = [| 127 |]; blends = [| 0 |] };
      { shifts = [| 6; 0 |]; masks = [| 31; 63 |]; blends = [| 192; 128 |] };
      { shifts = [| 12; 6; 0 |]; masks = [| 15; 63; 63 |]; blends = [| 224; 128; 128 |] };
      { shifts = [| 18; 12; 6; 0 |]; masks = [| 7; 63; 63; 63 |]; blends = [| 240; 128; 128; 128 |] };
    |]
  in
  let convert_exn str =
    match Int.of_string str with
    | code when code <= 0x7f -> Char.of_int_exn code |> String.of_char
    | code ->
      let num_bytes =
        match code with
        | x when x <= 0x7ff -> 2
        | x when x <= 0xffff -> 3
        | _ -> 4
      in
      let consts = consts.(num_bytes - 1) in
      String.init num_bytes ~f:(fun i ->
          (code lsr consts.shifts.(i)) land consts.masks.(i) lor consts.blends.(i) |> Char.of_int_exn)
  in
  function
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
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b |> convert_exn
    | '#', '0' .. '9' -> String.slice str 1 0 |> convert_exn
    | _ -> raise (Invalid_argument str)
  )

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
        loop buf (stop + 1)
    )
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
 |'\''
 |'='
 |'<'
 |'?'
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

let is_ws = function
| '\x20'
 |'\x0d'
 |'\x09'
 |'\x0a' ->
  true
| _ -> false

let drop p = p >>| const ()

let double x y = x, y

let make_string_parser ~separator = char separator *> take_till (Char.( = ) separator) <* char separator

let ws = skip_while is_ws

let comment = string "<!--" *> Parsing.skip_until_pattern ~pattern:"-->"

let blank = skip_many (ws *> comment) *> ws

let token_parser = take_while1 is_token

let parser =
  let xml_string_parser =
    let dq_string = make_string_parser ~separator:'"' in
    let sq_string = make_string_parser ~separator:'\'' in
    dq_string <|> sq_string
  in
  let attr_parser = lift2 double (token_parser <* ws <* char '=') (ws *> xml_string_parser) in
  let doctype_parser =
    let entity =
      string "[<!ENTITY" *> ws *> skip_many (ws *> (token_parser <|> xml_string_parser))
      <* ws
      <* string ">]"
    in
    string "<!DOCTYPE"
    *> (skip_many (blank *> choice [ drop token_parser; drop xml_string_parser; entity ])
       <* blank
       <* char '>'
       )
    >>| const SAX.Nothing
  in
  let prologue_parser =
    string "<?xml " *> (many (ws *> attr_parser) >>| fun attrs -> SAX.Prologue attrs) <* ws <* string "?>"
  in
  let cdata_parser =
    string "<![CDATA[" *> Parsing.take_until_pattern ~pattern:"]]>" >>| fun s -> SAX.Cdata s
  in
  let element_open_parser =
    lift3
      (fun tag attrs self_closing ->
        let preserve_space =
          lazy
            (List.mem attrs ("xml:space", "preserve")
               ~equal:String.((fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)))
        in
        let eopen = SAX.Element_open { tag; attrs; preserve_space } in
        if self_closing then SAX.Many [ eopen; Element_close tag ] else eopen)
      (char '<' *> ws *> token_parser)
      (many (ws *> attr_parser) <* ws)
      (option false (char '/' >>| const true) <* char '>')
  in
  let element_close_parser =
    string "</" *> ws *> (token_parser >>| fun s -> SAX.Element_close s) <* (ws <* char '>')
  in
  let text_parser = take_while1 is_text >>| fun s -> SAX.Text s in
  let fast_path =
    peek_string 2 >>= function
    | "</" -> element_close_parser
    | "<!" -> cdata_parser <|> doctype_parser
    | "<?" -> prologue_parser
    | s -> (
      match s.[0] with
      | '<' -> element_open_parser
      | c when is_token c -> text_parser
      | _ -> fail "Lookahead impossible"
    )
  in
  let slow_path =
    let node =
      choice [ element_close_parser; element_open_parser; cdata_parser; prologue_parser; doctype_parser ]
    in
    skip_many (ws *> comment) *> (ws *> node <|> text_parser)
  in
  fast_path <|> slow_path
