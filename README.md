# SZXX

_Streaming ZIP XML XLSX parser_

SZXX is a streaming, non-seeking and efficient XLSX parser built from ground up for low memory usage. SZXX is able to output XLSX rows while a file is being read from the file descriptor without buffering any part of the file.

XLSX files are a ZIP archive containing a number of XML files. As such, SZXX is made up of three nested parsers: ZIP, XML, and XLSX. They can all be used independently, but none of them implement the entire spec due to the non-seeking requirement, only enough to stream XLSX rows. Despite this, SZXX might be sufficient for your needs.

The ZIP archive is parsed while the file is still being read from the file descriptor. Files can be either skipped, returned as a string, or processed chunk by chunk by an `Angstrom` parser. The ZIP layer only supports methods `0` (`store`) and `8` (`deflated`) as those are the only ones used by XLSX.

The XML layer is technically a DOM parser, but allows streaming and pruning a specific path within the document. The XML parser is far from spec-compliant and does not attempt to validate, correct errors, or follow references. Text nodes are seen as opaque bytes.

The XLSX layer always streams rows in the order they are encountered. It does not automatically load string references from the Shared Strings Table (SST) because it is usually located after the worksheets within the ZIP.

### Example
```ocaml
let print_rows_as_json xlsx_path =
  (* The input channel (ic) could be a network socket that's already open *)
  Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input xlsx_path (fun ic ->
    let open SZXX.Xlsx in
    (* yojson_readers is an easy way to quickly inspect a file *)
    let stream, processed = stream_rows_buffer yojson_readers ic in
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

#### I see it uses [Camlzip](https://github.com/xavierleroy/camlzip), are you sure it's not buffering the whole zip file?

It only uses Camlzip for its zlib bindings. The next version of SZXX will allow the user to choose between zlib and [Decompress](https://github.com/mirage/decompress).

#### Does this work in the browser?

Not yet, due to the zlib bindings. It will soon support the [Decompress](https://github.com/mirage/decompress) library in pure OCaml. At that point, yes it will compile to JavaScript under js_of_ocaml.

#### Is it fast?

Reasonably so. Using 1 core on a 2018 Macbook Pro, it processes a 107MB, 28-column x 1,048,576-row file in 120 seconds using 1 570MB of memory. The same file takes 65 seconds to open in LibreOffice using 2 cores and nearly 2GB of memory.
