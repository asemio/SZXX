# SZXX

_Streaming ZIP XML XLSX parser_

SZXX is a streaming, non-seeking and efficient XLSX parser built from ground up for low memory usage and high performance. SZXX is able to output XLSX rows while a file is being downloaded from the network without buffering any part of the file.

XLSX files are a ZIP archive containing a number of XML files. As such, SZXX is made up of three nested parsers: ZIP, XML, and XLSX. They can all be used independently, but none of them implement the entire spec, preferring to trade features for performance. Despite this, SZXX might be sufficient for your needs.

The ZIP archive is parsed while the file is still being read from the file descriptor. Files can be either skipped, returned as a string, or processed chunk by chunk by an `Angstrom` parser. The ZIP layer only supports method `0` (`store`) and `8` (`deflated`) as those are the only ones used by XLSX.

The XML layer is technically a DOM parser, but allows streaming and/or pruning a certain "path" within the document. The XML parser is far from spec-compliant and does not attempt to validate, correct errors, or follow references. Text nodes are seen as opaque bytes.

The XLSX layer will automatically load string references when the Shared Strings Table (SST) is used. However, some documents that make use of the SST will place it after the worksheets in the ZIP archive, forcing SZXX to hold onto those rows until the SST is encountered. Rows are always streamed in the order they were encountered.

### Example
```ocaml
let print_rows_as_json xlsx_path =
  (* The input channel (ic) could be a network socket that's already open *)
  Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
    let open SZXX.Xlsx in
    (* yojson_readers is an easy way to quickly inspect a file *)
    let stream, processed = stream_rows yojson_readers ic in
    let%lwt () = Lwt_stream.iter (fun row ->
        (`List (Array.to_list row.data))
        |> Yojson.Basic.pretty_to_string
        |> print_endline
      ) stream
    in
    (* bind to/await the `processed` promise to catch any error that may have terminated the stream early *)
    processed
  )
```

### FAQ

#### Why `Lwt_stream`?

It's a convenient way to expose this sort of functionality. Unfortunately it results in having to return a promise to pass back errors that killed the stream half way through. A future version might improve on this.

#### I noticed it uses [Camlzip](https://github.com/xavierleroy/camlzip), are you sure it's not buffering the whole zip file?

It only uses Camlzip for its zlib bindings. The next version of SZXX will allow the user to decide between zlib and [Decompress](https://github.com/mirage/decompress).

#### Does this work in the browser?

Not yet. The only C code is the zlib bindings. It will soon support the [Decompress](https://github.com/mirage/decompress) library in pure OCaml. At that point, yes it will compile to JavaScript under js_of_ocaml.

#### Is it fast?

Yes