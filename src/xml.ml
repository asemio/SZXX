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

    let cb acc (node : node) =
      match node, acc with
      | Prologue _, { decl_attrs = Some _; _ } -> failwith "A prologue already exists"
      | Prologue attrs, { decl_attrs = None; _ } -> { acc with decl_attrs = Some attrs }
      | Element_open { tag; attrs; preserve_space }, _ ->
        let partial =
          { tag; attrs; preserve_space; buf = Buffer.create 16; children = Queue.create ~capacity:4 () }
        in
        { acc with stack = partial :: acc.stack }
      | Text s, { stack = []; top = None; _ } ->
        failwithf "Invalid document. Text not contained within an element: %s" s ()
      | Cdata s, { stack = []; top = None; _ } ->
        failwithf "Invalid document. CDATA not contained within an element: %s" s ()
      | Text _, { stack = []; top = Some _; _ }
       |Cdata _, { stack = []; top = Some _; _ } ->
        acc
      | Text s, { stack = { buf; preserve_space; _ } :: _; _ }
       |Cdata s, { stack = { buf; preserve_space; _ } :: _; _ } ->
        let preserve = force preserve_space in
        if Buffer.length buf > 0 && not (preserve && String.is_prefix s ~prefix:" ")
        then Buffer.add_char buf ' ';
        Buffer.add_string buf (if preserve then s else String.strip s);
        acc
      | Element_close tag, { stack = current :: (parent :: _ as rest); _ }
        when String.( = ) tag current.tag ->
        Queue.enqueue parent.children (partial_to_element current);
        { acc with stack = rest }
      | Element_close tag, { stack = [ current ]; top = None; _ } when String.( = ) tag current.tag ->
        { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, { stack = current :: _ :: _; _ } ->
        failwithf "Invalid document. Closing element '%s' before element '%s'" tag current.tag ()
      | Element_close tag, _ ->
        failwithf "Invalid document. Closing tag without matching opening tag: %s" tag ()
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
      path: string;
      top: DOM.element option;
    }
    [@@deriving sexp_of]

    let init = { decl_attrs = None; stack = []; path = ""; top = None }

    let partial_to_element { tag; attrs; buf; children; _ } =
      DOM.{ tag; attrs; text = Buffer.contents buf; children = Queue.to_array children }

    let cb ~filter_path ~on_match acc (node : node) =
      match node, acc with
      | Prologue _, { decl_attrs = Some _; _ } -> failwith "A prologue already exists"
      | Prologue attrs, { decl_attrs = None; _ } -> { acc with decl_attrs = Some attrs }
      | Element_open { tag; attrs; preserve_space }, _ ->
        let partial =
          { tag; attrs; preserve_space; buf = Buffer.create 16; children = Queue.create ~capacity:4 () }
        in
        { acc with stack = partial :: acc.stack; path = sprintf "%s %s" acc.path tag }
      | Text s, { stack = []; top = None; _ } ->
        failwithf "Invalid document. Text not contained within an element: %s" s ()
      | Cdata s, { stack = []; top = None; _ } ->
        failwithf "Invalid document. CDATA not contained within an element: %s" s ()
      | Text s, { stack = { buf; preserve_space; _ } :: _; path; _ }
       |Cdata s, { stack = { buf; preserve_space; _ } :: _; path; _ }
        when String.is_prefix path ~prefix:filter_path ->
        let preserve = force preserve_space in
        if Buffer.length buf > 0 && not (preserve && String.is_prefix s ~prefix:" ")
        then Buffer.add_char buf ' ';
        Buffer.add_string buf (if preserve then s else String.strip s);
        acc
      | Text _, _
       |Cdata _, _ ->
        acc
      | Element_close tag, { stack = current :: (parent :: _ as rest); path; _ }
        when String.( = ) tag current.tag ->
        let len_path = String.length path in
        let len_tag = String.length tag in
        let len_filter_path = String.length filter_path in
        (match String.is_prefix path ~prefix:filter_path, len_path = len_filter_path with
        | true, false -> Queue.enqueue parent.children (partial_to_element current)
        | true, true -> on_match (partial_to_element current)
        | false, _ -> ());
        let new_path =
          match len_path - (len_tag + 1) with
          | 0 -> ""
          | stop -> String.slice path 0 stop
        in
        { acc with stack = rest; path = new_path }
      | Element_close tag, { stack = [ current ]; top = None; _ } when String.( = ) tag current.tag ->
        { acc with stack = []; top = Some (partial_to_element current) }
      | Element_close tag, { stack = current :: _ :: _; _ } ->
        failwithf "Invalid document. Closing element '%s' before element '%s'" tag current.tag ()
      | Element_close tag, _ ->
        failwithf "Invalid document. Closing tag without matching opening tag: %s" tag ()
  end
end

let escape_table = function
| "amp" -> "&"
| "lt" -> "<"
| "gt" -> ">"
| "apos" -> "'"
| "quot" -> "\""
| s -> (
  match String.chop_prefix ~prefix:"#" s with
  | None -> sprintf "&%s;" s
  | Some num -> (
    try Int.of_string num |> Char.of_int_exn |> Char.to_string with
    | _ -> sprintf "&%s;" s
  )
)

let unescape s =
  let buf = Buffer.create (String.length s) in
  let _ =
    String.fold s ~init:(false, []) ~f:(fun (escaping, ll) c ->
        match c, escaping with
        | '&', false -> true, ll
        | ';', true ->
          let code = String.of_char_list ll |> String.rev |> escape_table in
          Buffer.add_string buf code;
          false, []
        | c, true -> true, c :: ll
        | c, false ->
          Buffer.add_char buf c;
          false, ll)
  in
  if Buffer.length buf = String.length s then s else Buffer.contents buf

let escapable_string_parser ~separator =
  char separator
  *>
  let is_separator = Char.( = ) separator in
  let buf = Buffer.create 20 in
  let rec loop escaping ll =
    any_char >>= fun x ->
    match x, escaping with
    | '&', false -> (loop [@tailcall]) true ll
    | ';', true ->
      let code = String.of_char_list ll |> String.rev |> escape_table in
      Buffer.add_string buf code;
      (loop [@tailcall]) false []
    | c, _ when is_separator c ->
      List.fold_right ll ~init:() ~f:(fun c () -> Buffer.add_char buf c);
      let result = Buffer.contents buf in
      Buffer.clear buf;
      return result
    | c, true -> (loop [@tailcall]) true (c :: ll)
    | c, false ->
      Buffer.add_char buf c;
      (loop [@tailcall]) false ll
  in
  loop false []

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

let drop p = p *> return ()

let double x y = x, y

let skip_until_string terminate =
  let first = terminate.[0] in
  let len = String.length terminate in
  let rec loop () =
    skip_while (Char.( <> ) first) >>= fun () ->
    peek_string len >>= function
    | x when String.(x = terminate) -> string terminate
    | _ -> (loop [@tailcall]) ()
  in
  loop ()

let ws = skip_while is_ws

let comment = string "<!--" *> skip_until_string "-->"

let blank = drop (sep_by comment ws)

let parser ~init ~(cb : 'a -> SAX.node -> 'a) =
  let box = ref init in
  let cb node = box := cb !box node in
  let xml_string =
    let dq_string = escapable_string_parser ~separator:'"' in
    let sq_string = escapable_string_parser ~separator:'\'' in
    dq_string <|> sq_string
  in
  let token = take_while1 is_token in
  let attr = lift2 double (token <* ws <* char '=') (ws *> xml_string) in
  let doctype_parser =
    let entity =
      string "[<!ENTITY" *> ws *> skip_many (ws *> choice [ token; xml_string ]) <* ws <* string ">]"
    in
    string "<!DOCTYPE" *> ws *> skip_many (ws *> choice [ drop token; drop xml_string; entity ])
    <* ws
    <* char '>'
  in
  let prologue_parser =
    lift4
      (fun _ attrs _ _ -> cb (Prologue attrs))
      (string "<?xml ")
      (many (blank *> attr))
      blank (string "?>")
  in
  let cdata_parser =
    let buf = Buffer.create 20 in
    blank
    *> string "<![CDATA["
    *>
    let rec loop n ll =
      any_char >>= fun c ->
      match c, n with
      | ']', 0 -> (loop [@tailcall]) 1 (']' :: ll)
      | ']', 1 -> (loop [@tailcall]) 2 (']' :: ll)
      | '>', 2 ->
        let s = Buffer.contents buf in
        Buffer.clear buf;
        cb (Text s);
        return ()
      | c, 0 ->
        Buffer.add_char buf c;
        (loop [@tailcall]) 0 ll
      | c, _ ->
        List.fold_right (c :: ll) ~init:() ~f:(fun x () -> Buffer.add_char buf x);
        (loop [@tailcall]) 0 []
    in
    loop 0 []
  in
  let element_open_parser =
    lift3
      (fun tag attrs self_closing ->
        let preserve_space =
          lazy
            (List.mem attrs ("xml:space", "preserve")
               ~equal:String.((fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)))
        in
        cb (Element_open { tag; attrs; preserve_space });
        if self_closing then cb (Element_close tag))
      (blank *> char '<' *> ws *> token)
      (many (ws *> attr) <* ws)
      (option false (char '/' *> return true) <* char '>')
  in
  let element_close_parser =
    lift4 (fun _ _ tag _ -> cb (Element_close tag)) (blank *> string "</") ws token (ws <* char '>')
  in
  let text_parser = skip_many (ws <* comment) *> take_while1 is_text >>| fun s -> cb (Text s) in
  let node_parser = choice [ cdata_parser; element_close_parser; element_open_parser; text_parser ] in
  lift4
    (fun () () () () -> !box)
    (blank *> option () prologue_parser)
    (blank *> option () doctype_parser)
    (skip_many node_parser) blank
