open! Core_kernel

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
        "attrs": [ [ "ref", "A1:A38" ], [ "test", "double&quot;quote" ] ],
        "text": "",
        "children": []
      },
      {
        "tag": "t",
        "attrs": [],
        "text": "hello world y",
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
    |> sprintf "\xEF\xBB\xBF%s"

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
    |> sprintf "\xEF\xBB\xBF%s"

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
        "attrs": [ [ "xml:space", "preserve" ], [ "scoped", "" ] ],
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
    <span>BOO!</span>
  </body>
</html>
|s}

  let data =
    Yojson.Safe.from_string
      {json|
{
  "decl_attrs": null,
  "top": {
    "tag": "html",
    "attrs": [ [ "lang", "eng" ] ],
    "text": "",
    "children": [
      {
        "tag": "head",
        "attrs": [],
        "text": "clear head text X some lost text back to the top",
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
        "attrs": [ [ "scoped", "" ] ],
        "text": "",
        "children": [
          {
            "tag": "span",
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

type element = SZXX.Xml.DOM.element = {
  tag: string;
  attrs: (string * string) list;
  text: string;
  children: element array;
}
[@@deriving sexp_of, to_yojson]

type doc = {
  decl_attrs: (string * string) list option;
  top: element;
}
[@@deriving sexp_of, to_yojson]

type buffer = Buffer.t

let sexp_of_buffer buf = Sexp.List [ Atom "Buffer"; Atom (Buffer.contents buf) ]

let xml_to_dom ?(options = SZXX.Xml.default_parser_options) ?strict test data () =
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.All
      (Angstrom.many SZXX.Xml.(make_parser options))
      test
  with
  | Ok nodes ->
    (* print_endline (sprintf !"%{sexp#hum: SZXX.Xml.SAX.node list}" nodes); *)
    let doc =
      match
        List.fold_result nodes ~init:SZXX.Xml.SAX.To_DOM.init ~f:(fun acc -> function
          | Text s -> SZXX.Xml.SAX.To_DOM.folder ?strict (Ok acc) (Text (SZXX.Xml.unescape s))
          | el -> SZXX.Xml.SAX.To_DOM.folder ?strict (Ok acc) el)
      with
      | Ok { decl_attrs; stack = []; top = Some top; _ } -> { decl_attrs; top }
      | Ok state -> failwithf !"Invalid state: %{sexp: SZXX.Xml.SAX.To_DOM.state}" state ()
      | Error msg -> failwith msg
    in
    Json_diff.check (doc_to_yojson doc) data;
    Lwt.return_unit
  | Error msg -> failwith msg

let xml_stream ?(options = SZXX.Xml.default_parser_options) ?strict test data filter_path () =
  let queue = Queue.create () in
  let on_match x = Queue.enqueue queue x in
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.All
      (Angstrom.many SZXX.Xml.(make_parser options))
      test
  with
  | Ok nodes ->
    let _state =
      List.fold_result nodes ~init:SZXX.Xml.SAX.Stream.init ~f:(fun acc x ->
          SZXX.Xml.SAX.Stream.folder ?strict ~filter_path ~on_match (Ok acc) x)
    in
    let streamed = `Assoc [ "data", [%to_yojson: element array] (Queue.to_array queue) ] in
    Json_diff.check streamed data;
    Lwt.return_unit
  | Error msg -> failwith msg

let readme_example1 () =
  let open SZXX in
  let raw_xml_string =
    {|
<container>123
  <value>foo</value>
  <value>bar</value>
  456
</container>
|}
  in

  let nodes =
    Angstrom.parse_string ~consume:All (Angstrom.many Xml.parser) raw_xml_string |> Result.ok_or_failwith
  in
  let xml =
    List.fold_result nodes ~init:Xml.SAX.To_DOM.init ~f:(fun acc x -> Xml.SAX.To_DOM.folder (Ok acc) x)
    |> Result.ok_or_failwith
  in
  (* Do something with `xml` *)
  Lwt_io.printl
    (Option.value_map xml.top ~default:"--" ~f:(fun el -> Xml.DOM.sexp_of_element el |> Sexp.to_string))

let readme_example2 () =
  let open Lwt.Syntax in
  let open SZXX in
  let input_channel, output_channel = Lwt_io.pipe () in
  let* () = Lwt_io.write output_channel Test1.raw in
  let* () = Lwt_io.close output_channel in

  let state = ref (Ok Xml.SAX.To_DOM.init) in
  let on_parse node =
    state := Xml.SAX.To_DOM.folder !state node;
    Lwt.return_unit
  in
  let* _rest, result =
    Lwt.finalize
      (fun () -> Angstrom_lwt_unix.parse_many Xml.parser on_parse input_channel)
      (fun () -> Lwt_io.close input_channel)
  in

  match result, !state with
  | Error msg, _
   |_, Error msg ->
    failwith msg
  | Ok (), Ok parsed_xml ->
    (* Do something with parsed_xml *)
    let* () =
      Option.value_map parsed_xml.top ~default:"--" ~f:(fun el ->
          Xml.DOM.sexp_of_element el |> Sexp.to_string)
      |> Lwt_io.print
    in
    Lwt.return_unit

let readme_example3 () =
  let open Lwt.Syntax in
  let open SZXX in
  let input_channel, output_channel = Lwt_io.pipe () in
  let* () =
    Lwt_io.write output_channel
      {|
<html>
  <head></head>
  <body>
    <div><p>foo</p></div>
    <div>bar</div>
    456
  </body>
</html>
|}
  in
  let* () = Lwt_io.close output_channel in

  let state = ref (Ok Xml.SAX.Stream.init) in
  let filter_path = [ "html"; "body"; "div" ] in
  let on_match div =
    (* Do something with `div` *)
    print_endline (Xml.DOM.sexp_of_element div |> Sexp.to_string)
  in
  let on_parse node =
    state := Xml.SAX.Stream.folder ~filter_path ~on_match !state node;
    Lwt.return_unit
  in
  let* _rest, result =
    Lwt.finalize
      (fun () -> Angstrom_lwt_unix.parse_many Xml.parser on_parse input_channel)
      (fun () -> Lwt_io.close input_channel)
  in

  match result, !state with
  | Error msg, _
   |_, Error msg ->
    failwith msg
  | Ok (), Ok shallow_tree ->
    (* Do something with `shallow_tree` or `acc_top_level_divs` *)
    let* () =
      Lwt_io.printl
        (Option.value_map shallow_tree.decl_attrs ~default:"--" ~f:(fun x ->
             Xml.sexp_of_attr_list x |> Sexp.to_string))
    in
    Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run ~verbose:true "SZXX XML"
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
                 ~options:{ accept_html_boolean_attributes = true; accept_unquoted_attributes = true }
                 Test4.raw Test4.data );
             ( "Stream 4",
               `Quick,
               xml_stream ~strict:false
                 ~options:{ accept_html_boolean_attributes = true; accept_unquoted_attributes = true }
                 Test4.raw Test4.data_streamed [ "html"; "head"; "meta" ] );
             "Sync To_DOM", `Quick, readme_example1;
             "Async To_DOM", `Quick, readme_example2;
             "Async Stream", `Quick, readme_example3;
           ] );
       ]
