open! Core

(** Basic XML types and accessor functions *)
module DOM : sig
  type attr_list = (string * string) list [@@deriving sexp_of, compare, equal]

  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element list;
  }
  [@@deriving sexp_of, compare, equal]

  (** Convenience function to access attributes by name *)
  val get_attr : attr_list -> string -> string option

  (** Convenience function to check whether an element has attribute [xml:space="preserve"]  *)
  val preserve_space : attr_list -> bool

  (** [SZXX.Xml.DOM.unescape "Fast &amp; Furious &#x1F3CE;&#xFE0F;"] returns ["Fast & Furious 🏎️"] *)
  val unescape : string -> string

  (** [el |> dot "row"] returns the first immediate [<row>] child of element [el] *)
  val dot : string -> element -> element option

  (** [el |> dot "row"] returns the text of the first immediate [<row>] child of element [el] *)
  val dot_text : string -> element -> string option

  (** [el |> filter_map "row" ~f] returns all filtered [f <row>] children of element [el] *)
  val filter_map : string -> f:(element -> 'a option) -> element -> 'a list

  (** [el |> at "3"] returns the nth (0-based indexing) immediate child of element [el]. The first argument is a string. *)
  val at : string -> element -> element option

  (** [get el [dot "abc"; dot "def"]] is equivalent to [el |> dot "abc" |> Option.bind ~f:(dot "def")]
      Convenience function to chain multiple [dot] and [at] calls to access nested elements. *)
  val get : (element -> element option) list -> element -> element option
end

(** Advanced parsing utilities: custom parser options and tools to stream huge documents *)
module SAX : sig
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
      (** Invalid XML but valid HTML: [<div attr1="foo" attr2>]
          But with [accept_html_boolean_attributes] set to [true], [attr2] will be ["attr2"] *)
    accept_unquoted_attributes: bool;
      (** Invalid XML but valid HTML: [<div attr1="foo" attr2=bar>]
          But with [accept_unquoted_attributes] set to [true], [attr2] will be ["bar"] *)
    accept_single_quoted_attributes: bool;
      (** Invalid XML but valid HTML: [<div attr1="foo" attr2='bar'>]
          But with [accept_unquoted_attributes] set to [true], [attr2] will be ["bar"] *)
  }
  [@@deriving sexp_of, compare, equal]

  (** HTML boolean attributes: [true]. Anything else: [false]. *)
  val default_parser_options : parser_options

  val make_parser : parser_options -> node Angstrom.t

  (** IO-agnostic [Angstrom.t] XML parser.

    It is not fully spec-compliant, it does not attempt to validate character encoding or reject all incorrect documents.
    It does not process references.
    It does not automatically unescape XML escape sequences but [SZXX.Xml.DOM.unescape] is provided to do so.

    See README.md for examples on how to use it. *)
  val parser : node Angstrom.t

  (** For those who want finer-grained control and want to parse (using Angstrom) and fold (using this module) by hand. *)
  module Expert : sig
    type partial_text = {
      literal: bool;
      raw: string;
    }
    [@@deriving sexp_of, compare, equal]

    type partial = {
      tag: string;
      attrs: DOM.attr_list;
      text: partial_text list;
      children: DOM.element list;
      staged: DOM.element list;
    }
    [@@deriving sexp_of, compare, equal]

    (** Assemble a sequence of [SAX.node] "events" into a classic XML document *)
    module To_DOM : sig
      type state = {
        decl_attrs: DOM.attr_list option;
        stack: partial list;
        top: DOM.element option;
      }
      [@@deriving sexp_of, compare, equal]

      val init : state

      (** [strict] (default: [true]) When false, non-closed elements are treated as self-closing elements, HTML-style.
        For example a [<br>] without a matching [</br>] will be treated as a self-closing [<br />] *)
      val folder : ?strict:bool -> (state, string) result -> node -> (state, string) result
    end

    (** Assemble a sequence of [SAX.node] "events" into a "shallow DOM" while streaming out the children matching a certain path.
      Those children aren't added to the DOM (hence "shallow"). *)
    module Stream : sig
      type state = {
        decl_attrs: DOM.attr_list option;
        stack: partial list;
        path_stack: string list;
        top: DOM.element option;
      }
      [@@deriving sexp_of, compare, equal]

      val init : state

      (** [strict] (default: [true]) When false, non-closed elements are treated as self-closing elements, HTML-style.
        For example a [<br>] without a matching [</br>] will be treated as a self-closing [<br />].
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
end

type document = {
  decl_attrs: DOM.attr_list;  (** The declaration attributes, e.g. version and encoding *)
  top: DOM.element;  (** The top element of the document *)
}
[@@deriving sexp_of, compare, equal]

(** Progressively parse a fully formed, fully escaped XML document.
    It begins parsing without having to read the whole input in its entirety.

    [parser]: Override the default parser.
      Make your own parser with [SZXX.Xml.SAX.make_parser] or pass [SZXX.Xml.html_parser].

    [strict]: Default: [true]. When false, non-closed elements are treated as self-closing elements, HTML-style.
      For example a [<br>] without a matching [</br>] will be treated as a self-closing [<br />].

    [feed]: A producer of raw input data. Create a [feed] by using the [SZXX.Feed] module. *)
val parse_document : ?parser:SAX.node Angstrom.t -> ?strict:bool -> Feed.t -> (document, string) result

(** Same as [parse_document], but from a string *)
val parse_document_from_string :
  ?parser:SAX.node Angstrom.t -> ?strict:bool -> string -> (document, string) result

val html_parser : SAX.node Angstrom.t

(** Progressively assemble an XML DOM, but every element that matches [filter_path] is passed
    to [on_match] instead of being added to the DOM. This "shallow DOM" is then returned.
    All text nodes are properly unescaped.
    It begins parsing without having to read the whole input in its entirety.

    [parser]: Override the default parser.
      Make your own parser with [SZXX.Xml.SAX.make_parser] or pass [SZXX.Xml.html_parser].

    [strict]: Default: [true]. When false, non-closed elements are treated as self-closing elements, HTML-style.
      For example a [<br>] without a matching [</br>] will be treated as a self-closing [<br />].

    [feed]: A producer of raw input data. Create a [feed] by using the [SZXX.Feed] module.

    [filter_path]: indicates which part of the DOM should be streamed out instead of being stored in the DOM.
      For example [ ["html"; "body"; "div"; "div"; "p"] ] will emit all the [<p>] tags nested inside exactly 2 levels of [<div>] tags in an HTML document.

    [on_match]: Called on every element that matched [filter_path] *)
val stream_matching_elements :
  ?parser:SAX.node Angstrom.t ->
  ?strict:bool ->
  filter_path:string list ->
  on_match:(DOM.element -> unit) ->
  Feed.t ->
  (document, string) result
