open! Core_kernel

type attr_list = (string * string) list [@@deriving sexp_of]

(** Convenience function to access attributes by name *)
val get_attr : attr_list -> string -> string option

module DOM : sig
  type element = {
    tag: string;
    attrs: attr_list;
    text: string;
    children: element array;
  }
  [@@deriving sexp_of]

  (** [el |> dot "row"] returns the first immediate [<row>] child of element [el] *)
  val dot : string -> element -> element option

  (** [el |> dot "row"] returns the text of the first immediate [<row>] child of element [el] *)
  val dot_text : string -> element -> string option

  (** [el |> filter_map "row" ~f] returns all filtered [f <row>] children of element [el] *)
  val filter_map : string -> f:(element -> 'a option) -> element -> 'a array

  (** [el |> at "3"] returns the nth (0-based indexing) immediate child of element [el]. The first argument is a string. *)
  val at : string -> element -> element option

  (**
   [get el [dot "abc"; dot "def"]] is equivalent to [el |> dot "abc" |> Option.bind ~f:(dot "def")]
   Convenience function to chain multiple [dot] and [at] calls to access nested nodes.
*)
  val get : element -> (element -> element option) list -> element option
end

module SAX : sig
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

  module To_DOM : sig
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

    val init : state

    val folder : (state, string) result -> node -> (state, string) result
  end

  module Stream : sig
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

    val init : state

    val folder :
      filter_path:string list ->
      on_match:(DOM.element -> unit) ->
      (state, string) result ->
      node ->
      (state, string) result
  end
end

(**
   IO-agnostic [Angstrom.t] parser.

   It is not fully spec-compliant, it does not attempt to validate character encoding or reject all incorrect documents.
   It does not process references.
   It does not automatically unescape XML escape sequences automatically but [unescape] is provided to do so.

   Call this parser repeatedly to generate a sequence of SAX.node values.
   Those SAX.node values can then be folded using [SAX.To_DOM] or [SAX.Stream].

   See README for more information.
*)
val parser : SAX.node Angstrom.t

(**
   [unescape "Fast &amp; Furious"] returns ["Fast & Furious"]
*)
val unescape : string -> string
