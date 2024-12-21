let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core
open Lwt.Syntax
open Lwt.Infix

let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)

let feed_bigstring ic =
  let open SZXX.Zip in
  let open Lwt.Infix in
  let len = Lwt_io.buffer_size ic in
  let buf = Bigstring.create len in
  Bigstring
    (fun () ->
      Lwt_io.read_into_bigstring ic buf 0 len >|= function
      | 0 -> None
      | len -> Some { buf; pos = 0; len })

let fold xlsx_filename json_filename () =
  let xlsx_path = sprintf "../../../test/files/%s" xlsx_filename in
  let json_path = sprintf "../../../test/files/%s" json_filename in
  let* against =
    Lwt_io.with_file ~flags ~mode:Input json_path (fun ic ->
      let+ contents = Lwt_io.read ic in
      Yojson.Safe.from_string contents )
  in
  let* parsed =
    let queue = Queue.create () in
    Lwt_io.with_file ~flags ~mode:Input xlsx_path (fun ic ->
      let stream, success =
        SZXX.Zip.stream_files ~feed:(feed_bigstring ic) (fun _ ->
          Fold_string { init = (); f = (fun _entry s () -> Queue.enqueue queue (`String s)) } )
      in
      let processed = Lwt_stream.iter (const ()) stream in
      let* () = success in
      let+ () = processed in
      `Assoc [ "data", `List (List.rev (Queue.to_list queue)) ] )
  in

  Json_diff.check parsed against;
  (* let* () =
       Lwt_io.with_file ~flags:[ O_WRONLY; O_NONBLOCK; O_TRUNC ] ~mode:Output json_path (fun oc ->
           Lwt_io.write oc (Yojson.Safe.to_string parsed))
     in *)
  Lwt.return_unit

let readme_example extract_filename input_channel =
  let open Lwt.Syntax in
  let open SZXX in
  (* 1. Create a `feed` function. This README contains examples of this towards the end. *)
  let feed = feed_string input_channel in

  (* 2. Create a callback function. Here we skip all files except the one we're interested in.
      If you deem the file(s) too large for `Action.String`,
      then look into `Action.Fold_string`, `Action.Fold_bigstring` and `Action.Angstrom` *)
  let callback = function
    | ({ filename; _ } : Zip.entry) when String.(filename = extract_filename) -> Zip.Action.String
    | _ -> Zip.Action.Skip
  in

  (* 3. Invoke `Zip.stream_files` *)
  let stream, success = Zip.stream_files ~feed callback in

  (* 4. Work with the stream, but DO NOT AWAIT this promise!
      Again, do not bind unto this promise yet! *)
  let unzipped =
    (* Here we're just going to keep  *)
    let+ files = Lwt_stream.to_list stream in
    List.find_map files ~f:(function
      | _entry, Zip.Data.String raw -> Some raw
      | _ -> None )
  in

  (* 4. Bind/await the `success` promise to catch any error that may have terminated the stream early
      This could be due to a corrupted file or similar issues. *)
  let* () = success in

  (* 5. Finally we can bind/await the promise from step 3 and use it! *)
  let* unzipped in
  match unzipped with
  | None -> failwithf "File `%s` not found in ZIP archive" extract_filename ()
  | Some _raw ->
    (* Use `raw` file contents *)
    Lwt.return_unit

let test_readme_example zip_path () =
  Lwt_io.with_file ~flags:[ O_RDONLY; O_NONBLOCK ] ~mode:Input
    (sprintf "../../../test/files/%s" zip_path)
    (readme_example "xl/sharedStrings.xml")

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "SZXX ZIP"
       [
         ( "ZIP",
           [
             "financial.xlsx", `Quick, fold "financial.xlsx" "chunks.json";
             "Readme example", `Quick, test_readme_example "financial.xlsx";
           ] );
       ]
