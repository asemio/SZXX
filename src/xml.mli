open! Core_kernel

type attr_list = (string * string) list [@@deriving sexp_of]

type element = {
  tag: string;
  attrs: attr_list;
  text: string;
  children: element array;
} [@@deriving sexp_of]

type doc = {
  decl_attrs: attr_list option;
  top: element;
} [@@deriving sexp_of]

(**
   Creates an IO-agnostic [Angstrom.t] parser.
   By combining it with Angstrom-lwt-unix or Angstrom-async, it can be used to stream data while the file is being read.

   It is not fully spec-compliant, it does not attempt to validate character encoding or reject all incorrect documents.
   It does not process references.
   It does not automatically unescape XML escape sequences, but a limited utility function is provided.

   [SZXX.Xml.parser ?filter_map path]

   [filter_map]: function called on every element matched by [path].
   It can be used to stream data (by side effect) while the document is being parsed.
   Elements for which [filter_map] returns [None] will be excluded from the parsed document output.
   Excluding elements allows the user to stream a file of virtually infinite size.

   [path]: Nodes located within the element described by this path will be processed by [filter_map].
   For example, here's the [path] of the rows in an XLSX worksheet: [["worksheet"; "sheetData"; "row"]].
   In that case, the top level element is [<worksheet>], which contains [<sheetData>], which in turn contains many [<row>] elements.
*)
val parser:
  ?filter_map:(element -> element option) ->
  string list ->
  doc Angstrom.t

(** [el |> dot "row"] returns the first immediate [<row>] child of element [el] *)
val dot: string -> element -> element option

(** [el |> at "3"] returns the nth (0-based indexing) immediate child of element [el]. The first argument is a string. *)
val at: string -> element -> element option

(**
   [get el [dot "abc"; dot "def"]] is equivalent to [el |> dot "abc" |> Option.bind ~f:(dot "def")]
   Convenience function to chain multiple [dot] and [at] calls to access nested nodes.
*)
val get: element -> (element -> element option) list -> element option

(** Convenience function to access node attributes by name *)
val get_attr: element -> string -> string option

(**
   Limited but efficient function to unescapable XML escape sequences.
   Good enough for most cases, but validate it meets your needs before using.
   [unescape "Fast &amp; Furious"] returns ["Fast & Furious"]
*)
val unescape: string -> string
