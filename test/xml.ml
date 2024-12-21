open! Base

exception Exit = Stdlib.Exit

let get_file_path = Printf.sprintf "../../../test/files/%s"

module Test1 = struct
  let raw =
    {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?> <!DOCTYPE stylesheet [<!ENTITY
nbsp "<xsl:text
disable-output-escaping='yes'>&amp;nbsp;</xsl:text>">
  <!ELEMENT html (head, body)>
  <!ATTLIST img
   src    CDATA          #REQUIRED
   id     ID             #IMPLIED
   sort   CDATA          #FIXED "true"
   print  (yes | no) "yes"
  >
]
<!--comment-with-dashes-->
>
<!-- some
comment -->
    <worksheet
      xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x14ac xr xr2 xr3"
      xmlns:xr3="http://schemas.microsoft.com/office/spreadsheetml/2016/revision3" xr:uid="{00000000-0001-0000-0400-000000000000}" >
    < c > <!-- comment --> <![CDATA[<something />]]> <!-- some other comment -->
					234<![CDATA[woo
          hoo]]>
				</ c >
    <c> world < v >  woot</ v >WORLD <v> hurray </ v > </c>
    <c><v>FOO</v></c>
    <c><c><v>BAR</v></c></c>
    < dimension ref = "A1:A38" test="double&quot;quote" />
    <t> hello  world <x/>  y </t>
    <t xml:space="preserve"> hello!  world <x/>  z </t>
    </worksheet>
    |s}

  let data =
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
        "text": "<something /> 234woo\n          hoo",
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
        "attrs": [ [ "ref", "A1:A38" ], [ "test", "double&quot;quote" ] ],
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
end

module Test2 = struct
  let raw =
    {s|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="3" uniqueCount="3"><si><t xml:space="preserve">hello</t></si><si><t xml:space="preserve">world</t></si><si><t xml:space="preserve">ok bye</t></si></sst>

|s}
    (* UTF-8 BOM *)
    |> Printf.sprintf "\xEF\xBB\xBF%s"

  let data =
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

  let data_streamed =
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
end

module Test3 = struct
  let raw =
    {s|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet>
  <c>hello &amp; goodbye</c>
  <c>world<v>woot</v>WORLD<v>hurray</v></c>
  <t xml:space="preserve" scoped> &quot;hello!&quot;  world <x/>  &#124; &#xa; &#x1F1E8;&#x1F1E6; </t>
  <c>bad1: &abc&amp; -</c>
  <c>bad2: &#1welp; -</c>
</worksheet>
|s}
    (* UTF-8 BOM *)
    |> Printf.sprintf "\xEF\xBB\xBF%s"

  let data =
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
    "attrs": [],
    "text": "",
    "children": [
      {
        "tag": "c",
        "attrs": [],
        "text": "hello & goodbye",
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
        "tag": "t",
        "attrs": [ [ "xml:space", "preserve" ], [ "scoped", "scoped" ] ],
        "text": " \"hello!\"  world   | \n 🇨🇦 ",
        "children": [
          { "tag": "x", "attrs": [], "text": "", "children": [] }
        ]
      },
      {
        "tag": "c",
        "attrs": [],
        "text": "bad1: &abc&amp; -",
        "children": []
      },
      {
        "tag": "c",
        "attrs": [],
        "text": "bad2: &#1welp; -",
        "children": []
      }
    ]
  }
}
|json}
end

module Test4 = struct
  let raw =
    {s|
<!doctype html>
<html lang="eng">
  <head>
    clear head   text
    <meta charset="utf-8">X
    <link rel="stylesheet" href="/assets/css/main.css">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    some lost text
    <meta name="description" content="">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0">
    <link rel="stylesheet" href="/assets/css/main.css">
    <title>Group Income</title>
    back to the top
    <!--<welp>welpo</welp>-->
  </head>
  <head2 xml:space="preserve">
    clear head   text
    <link rel="stylesheet" href="/assets/css/main.css">
    <meta charset="utf-8">
    <meta http-equiv=x-ua-compatible content="ie=edge">
    some lost text
    <meta name="description" content="">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0">
    <link rel="stylesheet" href="/assets/css/main.css">
    <title>Group Income</title>
    back to the top
  </head2>
  <body scoped>
    <div>BOO!</div>
  </body>
</html>
|s}

  let data =
    Yojson.Safe.from_string
      {json|
{
  "decl_attrs": [],
  "top": {
    "tag": "html",
    "attrs": [ [ "lang", "eng" ] ],
    "text": "",
    "children": [
      {
        "tag": "head",
        "attrs": [],
        "text": "clear head   text X some lost text back to the top",
        "children": [
          {
            "tag": "meta",
            "attrs": [ [ "charset", "utf-8" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "link",
            "attrs": [ [ "rel", "stylesheet" ], [ "href", "/assets/css/main.css" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "http-equiv", "x-ua-compatible" ], [ "content", "ie=edge" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "name", "description" ], [ "content", "" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "name", "viewport" ], [ "content", "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "link",
            "attrs": [ [ "rel", "stylesheet" ], [ "href", "/assets/css/main.css" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "title",
            "attrs": [],
            "text": "Group Income",
            "children": []
          }
        ]
      },
      {
        "tag": "head2",
        "attrs": [ [ "xml:space", "preserve" ] ],
        "text": "\n    clear head   text\n    \n    \n    \n    some lost text\n    \n    \n    \n    \n    back to the top\n  ",
        "children": [
          {
            "tag": "link",
            "attrs": [ [ "rel", "stylesheet" ], [ "href", "/assets/css/main.css" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "charset", "utf-8" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "http-equiv", "x-ua-compatible" ], [ "content", "ie=edge" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "name", "description" ], [ "content", "" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "meta",
            "attrs": [ [ "name", "viewport" ], [ "content", "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "link",
            "attrs": [ [ "rel", "stylesheet" ], [ "href", "/assets/css/main.css" ] ],
            "text": "",
            "children": []
          },
          {
            "tag": "title",
            "attrs": [],
            "text": "Group Income",
            "children": []
          }
        ]
      },
      {
        "tag": "body",
        "attrs": [ [ "scoped", "scoped" ] ],
        "text": "",
        "children": [
          {
            "tag": "div",
            "attrs": [],
            "text": "BOO!",
            "children": []
          }
        ]
      }
    ]
  }
}
|json}

  let data_streamed =
    Yojson.Safe.from_string
      {json|
{
"data": [
  {
    "tag": "meta",
    "attrs": [ [ "charset", "utf-8" ] ],
    "text": "",
    "children": []
  }
]
}
|json}
end

let test_large_cdata () =
  let buf = Buffer.create 1500 in
  let random =
    let s = String.init 256 ~f:Char.of_int_exn in
    String.concat [ s; s; s; s; s ]
  in
  Buffer.add_string buf (Printf.sprintf "<![CDATA[%s]]>" random);
  let node =
    Angstrom.parse_string ~consume:All SZXX.Xml.SAX.parser (Buffer.contents buf) |> Result.ok_or_failwith
  in
  if not ([%equal: SZXX.Xml.SAX.node] node (SZXX.Xml.SAX.Cdata random))
  then Printf.failwithf !"CDATA Strings did not match %{sexp#hum: SZXX.Xml.SAX.node}" node ()

type element = SZXX.Xml.DOM.element = {
  tag: string;
  attrs: (string * string) list;
  text: string;
  children: element list;
}
[@@deriving sexp_of, to_yojson]

type doc = SZXX.Xml.document = {
  decl_attrs: (string * string) list;
  top: element;
}
[@@deriving sexp_of, to_yojson]

type buffer = Buffer.t

let sexp_of_buffer buf = Sexp.List [ Atom "Buffer"; Atom (Buffer.contents buf) ]

let xml_to_dom ?(parser = SZXX.Xml.html_parser) ?strict test data () =
  let open SZXX in
  let doc = Xml.parse_document_from_string ~parser ?strict test |> Result.ok_or_failwith in
  Json_diff.check (doc_to_yojson doc) data

let xml_stream ?parser ?strict test data filter_path () =
  let open SZXX in
  let queue = Queue.create () in
  let on_match x = Queue.enqueue queue x in
  let _doc =
    Xml.stream_matching_elements ?parser ?strict ~filter_path ~on_match (Feed.of_string test)
    |> Result.ok_or_failwith
  in
  let streamed = `Assoc [ "data", [%to_yojson: element array] (Queue.to_array queue) ] in
  Json_diff.check streamed data

let readme_example1 env filename () =
  let open SZXX in
  let xml_path = get_file_path filename in
  let _doc =
    Eio.Path.with_open_in
      Eio.Path.(Eio.Stdenv.fs env / xml_path)
      (fun file -> Xml.parse_document (Feed.of_flow file))
    |> Result.ok_or_failwith
  in
  (* Do something with it *)
  ()

let readme_example2 env filename () =
  let open SZXX in
  let html_path = get_file_path filename in
  let doc =
    Eio.Path.with_open_in
      Eio.Path.(Eio.Stdenv.fs env / html_path)
      (fun file ->
        Xml.parse_document ~parser:Xml.html_parser (* New for HTML *)
          ~strict:false (* New for HTML *)
          (Feed.of_flow file))
    |> Result.ok_or_failwith
  in

  let text =
    doc.top |> Xml.DOM.(get [ dot "body"; dot "div" ]) |> Option.bind ~f:(Xml.DOM.dot_text "h1")
  in
  if not ([%equal: string option] text (Some "BOO!"))
  then Printf.failwithf !"h1 contained %{sexp: string option}" text ()

let corrupted env filename () =
  try
    let raw = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / get_file_path filename) in
    xml_to_dom raw Test1.data ();
    raise Exit
  with
  | Failure msg when String.is_prefix msg ~prefix:"SZXX: Invalid XML structure." -> ()

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run ~verbose:true "SZXX XML"
    [
      ( "XML",
        [
          "To DOM 1", `Quick, xml_to_dom Test1.raw Test1.data;
          "To DOM 2", `Quick, xml_to_dom Test2.raw Test2.data;
          "Stream 2", `Quick, xml_stream Test2.raw Test2.data_streamed [ "sst"; "si"; "t" ];
          "To DOM 3", `Quick, xml_to_dom Test3.raw Test3.data;
          ( "To DOM 4",
            `Quick,
            xml_to_dom ~strict:false
              ~parser:
                (SZXX.Xml.SAX.make_parser
                   {
                     accept_html_boolean_attributes = true;
                     accept_unquoted_attributes = true;
                     accept_single_quoted_attributes = false;
                     batch_size = 20;
                   } )
              Test4.raw Test4.data );
          ( "Stream 4",
            `Quick,
            xml_stream ~strict:false
              ~parser:
                (SZXX.Xml.SAX.make_parser
                   {
                     accept_html_boolean_attributes = true;
                     accept_unquoted_attributes = true;
                     accept_single_quoted_attributes = false;
                     batch_size = 20;
                   } )
              Test4.raw Test4.data_streamed [ "html"; "head"; "meta" ] );
          "Large CDATA", `Quick, test_large_cdata;
          "Readme example 1", `Quick, readme_example1 env "valid.xml";
          "Readme example 2", `Quick, readme_example2 env "index.html";
          "Corrupted", `Quick, corrupted env "chunks.json";
        ] );
    ]
