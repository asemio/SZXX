open! Core_kernel

let test1 =
  {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?> <!DOCTYPE stylesheet [<!ENTITY
nbsp "<xsl:text
disable-output-escaping='yes'>&amp;nbsp;</xsl:text>">]
>
<!-- some
comment -->
    <worksheet
      xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x14ac xr xr2 xr3"
      xmlns:xr3="http://schemas.microsoft.com/office/spreadsheetml/2016/revision3" xr:uid="{00000000-0001-0000-0400-000000000000}" >
    < c ><![CDATA[<something />]]> <!-- some other comment -->
					234<![CDATA[woo
          hoo]]>
				</ c >
    <c> world < v >  woot</ v >WORLD <v> hurray </ v > </c>
    <c><v>FOO</v></c>
    <c><c><v>BAR</v></c></c>
    < dimension ref = "A1:A38" />
    <t> hello  world <x/>  y </t>
    <t xml:space="preserve"> hello!  world <x/>  z </t>
    </worksheet>
    |s}

let data1 =
  Yojson.Safe.from_string
    {json|
{
  "decl_attrs": [
    [ "version", "1.0" ],
    [ "encoding", "UTF-8" ],
    [ "standalone", "yes" ]
  ],
  "top": {
    "tag": "worksheet",
    "attrs": [
      [
        "xmlns:mc",
        "http://schemas.openxmlformats.org/markup-compatibility/2006"
      ],
      [ "mc:Ignorable", "x14ac xr xr2 xr3" ],
      [
        "xmlns:xr3",
        "http://schemas.microsoft.com/office/spreadsheetml/2016/revision3"
      ],
      [ "xr:uid", "{00000000-0001-0000-0400-000000000000}" ]
    ],
    "text": "",
    "children": [
      {
        "tag": "c",
        "attrs": [],
        "text": "<something /> 234 woo\n          hoo",
        "children": []
      },
      {
        "tag": "c",
        "attrs": [],
        "text": "world WORLD",
        "children": [
          { "tag": "v", "attrs": [], "text": "woot", "children": [] },
          { "tag": "v", "attrs": [], "text": "hurray", "children": [] }
        ]
      },
      {
        "tag": "c",
        "attrs": [],
        "text": "",
        "children": [
          { "tag": "v", "attrs": [], "text": "FOO", "children": [] }
        ]
      },
      {
        "tag": "c",
        "attrs": [],
        "text": "",
        "children": [
          {
            "tag": "c",
            "attrs": [],
            "text": "",
            "children": [
              { "tag": "v", "attrs": [], "text": "BAR", "children": [] }
            ]
          }
        ]
      },
      {
        "tag": "dimension",
        "attrs": [ [ "ref", "A1:A38" ] ],
        "text": "",
        "children": []
      },
      {
        "tag": "t",
        "attrs": [],
        "text": "hello  world y",
        "children": [
          { "tag": "x", "attrs": [], "text": "", "children": [] }
        ]
      },
      {
        "tag": "t",
        "attrs": [ [ "xml:space", "preserve" ] ],
        "text": " hello!  world   z ",
        "children": [
          { "tag": "x", "attrs": [], "text": "", "children": [] }
        ]
      }
    ]
  }
}
|json}

let test2 =
  {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="3" uniqueCount="3"><si><t xml:space="preserve">hello</t></si><si><t xml:space="preserve">world</t></si><si><t xml:space="preserve">ok bye</t></si></sst>|s}

let data2 =
  Yojson.Safe.from_string
    {json|
{
  "decl_attrs": [
    [ "version", "1.0" ],
    [ "encoding", "UTF-8" ],
    [ "standalone", "yes" ]
  ],
  "top": {
    "tag": "sst",
    "attrs": [
      [
        "xmlns", "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
      ],
      [ "count", "3" ],
      [ "uniqueCount", "3" ]
    ],
    "text": "",
    "children": [
      {
        "tag": "si",
        "attrs": [],
        "text": "",
        "children": [
          {
            "tag": "t",
            "attrs": [ [ "xml:space", "preserve" ] ],
            "text": "hello",
            "children": []
          }
        ]
      },
      {
        "tag": "si",
        "attrs": [],
        "text": "",
        "children": [
          {
            "tag": "t",
            "attrs": [ [ "xml:space", "preserve" ] ],
            "text": "world",
            "children": []
          }
        ]
      },
      {
        "tag": "si",
        "attrs": [],
        "text": "",
        "children": [
          {
            "tag": "t",
            "attrs": [ [ "xml:space", "preserve" ] ],
            "text": "ok bye",
            "children": []
          }
        ]
      }
    ]
  }
}
|json}

let data3 =
  Yojson.Safe.from_string
    {json|
{
  "data": [
    {
      "tag": "t",
      "attrs": [ [ "xml:space", "preserve" ] ],
      "text": "hello",
      "children": []
    },
    {
      "tag": "t",
      "attrs": [ [ "xml:space", "preserve" ] ],
      "text": "world",
      "children": []
    },
    {
      "tag": "t",
      "attrs": [ [ "xml:space", "preserve" ] ],
      "text": "ok bye",
      "children": []
    }
  ]
}
|json}

type element = SZXX.Xml.DOM.element = {
  tag: string;
  attrs: (string * string) list;
  text: string;
  children: element array;
}
[@@deriving sexp_of, to_yojson]

type doc = SZXX.Xml.DOM.doc = {
  decl_attrs: (string * string) list option;
  top: element;
}
[@@deriving sexp_of, to_yojson]

type buffer = Buffer.t

let sexp_of_buffer buf = Sexp.List [ Atom "Buffer"; Atom (Buffer.contents buf) ]

let xml_to_dom test data () =
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.All
      (SZXX.Xml.parser ~init:SZXX.Xml.SAX.To_DOM.init ~cb:SZXX.Xml.SAX.To_DOM.cb)
      test
  with
  | Ok { decl_attrs; stack = []; top = Some top; _ } ->
    let doc = { decl_attrs; top } in
    Json_diff.check (doc_to_yojson doc) data;
    Lwt.return_unit
  | Ok state -> failwithf !"Invalid state: %{sexp: SZXX.Xml.SAX.To_DOM.state}" state ()
  | Error msg -> failwith msg

let xml_stream test data filter_path () =
  let queue = Queue.create () in
  let on_match x = Queue.enqueue queue x in
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.All
      (SZXX.Xml.parser ~init:SZXX.Xml.SAX.Stream.init ~cb:(SZXX.Xml.SAX.Stream.cb ~filter_path ~on_match))
      test
  with
  | Ok _ ->
    let streamed = `Assoc [ "data", [%to_yojson: element array] (Queue.to_array queue) ] in
    Json_diff.check streamed data;
    Lwt.return_unit
  | Error msg -> failwith msg

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "SZXX XML"
       [
         ( "XML",
           [
             "To DOM 1", `Quick, xml_to_dom test1 data1;
             "To DOM 2", `Quick, xml_to_dom test2 data2;
             "Stream 2", `Quick, xml_stream test2 data3 " sst si t";
           ] );
       ]
