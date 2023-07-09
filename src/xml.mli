open! Core

type attr_list = (string * string) list [@@deriving sexp_of, compare, equal]

(** Convenience function to access attributes by name *)
val get_attr : attr_list -> string -> string option

(** Convenience function to check whether a node has attribute [xml:space="preserve"]  *)
val preserve_space : attr_list -> bool

module DOM : sig
  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element list;
  }
  [@@deriving sexp_of, compare, equal]

  (** [el |> dot "row"] returns the first immediate [<row>] child of element [el] *)
  val dot : string -> element -> element option

  (** [el |> dot "row"] returns the text of the first immediate [<row>] child of element [el] *)
  val dot_text : string -> element -> string option

  (** [el |> filter_map "row" ~f] returns all filtered [f <row>] children of element [el] *)
  val filter_map : string -> f:(element -> 'a option) -> element -> 'a list

  (** [el |> at "3"] returns the nth (0-based indexing) immediate child of element [el]. The first argument is a string. *)
  val at : string -> element -> element option

  (**
   [get el [dot "abc"; dot "def"]] is equivalent to [el |> dot "abc" |> Option.bind ~f:(dot "def")]
   Convenience function to chain multiple [dot] and [at] calls to access nested nodes.
*)
  val get : element -> (element -> element option) list -> element option
end

module SAX : sig
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
  [@@deriving sexp_of, compare, equal]

  type partial_text = {
    literal: bool;
    raw: string;
  }
  [@@deriving sexp_of, compare, equal]

  type partial = {
    tag: string;
    attrs: attr_list;
    text: partial_text list;
    children: DOM.element list;
    staged: DOM.element list;
  }
  [@@deriving sexp_of, compare, equal]

  module To_DOM : sig
    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      top: DOM.element option;
    }
    [@@deriving sexp_of, compare, equal]

    val init : state

    (** [strict] (default: [true]) When false, non-closed elements are treated as self-closing elements, HTML-style.
        For example a [<br>] without a matching [</br>] is treated as [<br />] *)
    val folder : ?strict:bool -> (state, string) result -> node -> (state, string) result
  end

  module Stream : sig
    type state = {
      decl_attrs: attr_list option;
      stack: partial list;
      path_stack: string list;
      top: DOM.element option;
    }
    [@@deriving sexp_of, compare, equal]

    val init : state

    (** [strict] (default: [true]) When false, non-closed elements are treated as self-closing elements, HTML-style.
        For example a [<br>] without a matching [</br>] is treated as [<br />].
        Note that non-closed elements within the path to [filter_path] will prevent
        [on_match] from being called on some (but not all) otherwise matching elements. *)
    val folder :
      filter_path:string list ->
      on_match:(DOM.element -> unit) ->
      ?strict:bool ->
      (state, string) result ->
      node ->
      (state, string) result
  end
end

module Easy : sig
  type document = {
    decl_attrs: attr_list;
    top: DOM.element;
  }
  [@@deriving sexp_of, compare, equal]

  (** Returns a fully formed, fully unescaped document, or a (sometimes) helpful error in case of failure. *)
  val of_string : ?parser:SAX.node Angstrom.t -> ?strict:bool -> string -> (document, string) result
end

type parser_options = {
  accept_html_boolean_attributes: bool;
    (** Invalid XML but valid HTML: [<div attr1="foo" attr2>]
      When [accept_html_boolean_attributes] is [true], [attr2] will be [""] *)
  accept_unquoted_attributes: bool;
    (** Invalid XML but valid HTML: [<div attr1="foo" attr2=bar>]
      When [accept_unquoted_attributes] is [true], [attr2] will be ["bar"] *)
  accept_single_quoted_attributes: bool;
    (** Invalid XML but valid HTML: [<div attr1="foo" attr2=bar>]
         When [accept_unquoted_attributes] is [true], [attr2] will be ["bar"] *)
}

(** HTML boolean attributes: [true]. Anything else: [false]. *)
val default_parser_options : parser_options

val make_parser : parser_options -> SAX.node Angstrom.t

(**
   IO-agnostic [Angstrom.t] parser.

   It is not fully spec-compliant, it does not attempt to validate character encoding or reject all incorrect documents.
   It does not process references.
   It does not automatically unescape XML escape sequences automatically but [unescape] is provided to do so.

   See README.md for examples on how to use it.
*)
val parser : SAX.node Angstrom.t

(**
   [unescape "Fast &amp; Furious"] returns ["Fast & Furious"]
*)
val unescape : string -> string
