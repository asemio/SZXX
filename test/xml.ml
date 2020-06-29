open! Core_kernel

let test1 = {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?> <!DOCTYPE stylesheet [<!ENTITY
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
    <t> hello  world <x/>  x </t>
    <t xml:space="preserve"> hello!  world <x/>  x </t>
    </worksheet>
    |s}

let data1 = Yojson.Safe.from_string {json|
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
        "text": "hello  world x",
        "children": [
          { "tag": "x", "attrs": [], "text": "", "children": [] }
        ]
      },
      {
        "tag": "t",
        "attrs": [ [ "xml:space", "preserve" ] ],
        "text": " hello!  world  x ",
        "children": [
          { "tag": "x", "attrs": [], "text": "", "children": [] }
        ]
      }
    ]
  }
}
|json}

let test2 = {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="3" uniqueCount="3"><si><t xml:space="preserve">hello</t></si><si><t xml:space="preserve">world</t></si><si><t xml:space="preserve">ok bye</t></si></sst>|s}

let data2 = Yojson.Safe.from_string {json|
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

let yojson_of_attr_list (ll : SZXX.Xml.attr_list) : Yojson.Safe.t = `List (
    List.map ll ~f:(fun (x, y) -> `List [`String x; `String y])
  )

let rec yojson_of_element (el : SZXX.Xml.element) = `Assoc [
    "tag", `String el.tag;
    "attrs", yojson_of_attr_list el.attrs;
    "text", `String el.text;
    "children", `List (Array.fold_right el.children ~init:[] ~f:(fun x acc -> (yojson_of_element x) :: acc));
  ]

let yojson_of_doc (doc : SZXX.Xml.doc) : Yojson.Safe.t = `Assoc [
    "decl_attrs", Option.value_map doc.decl_attrs ~default:`Null ~f:yojson_of_attr_list;
    "top", yojson_of_element doc.top;
  ]

let xml test data () =
  begin match Angstrom.parse_string ~consume:Angstrom.Consume.All (SZXX.Xml.parser ["worksheet"; "sheetData"; "row"; "c"]) test with
  | Ok parsed ->
    Json_diff.check (yojson_of_doc parsed) data;
    Lwt.return_unit
  | Error msg -> failwith msg
  end

let () =
  Lwt_main.run @@ Alcotest_lwt.run "SZXX XML" [
    "XML", [
      "test1", `Quick, xml test1 data1;
      "test2", `Quick, xml test2 data2;
    ];
  ]
