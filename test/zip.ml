let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core
open Eio.Std

let feed_string src =
  let buf = Cstruct.create 4096 in
  SZXX.Zip.String
    (fun () ->
      match Eio.Flow.single_read src buf with
      | len -> Some (Cstruct.to_string ~len buf)
      | exception End_of_file -> None)

let feed_bigstring src =
  let open SZXX.Zip in
  let buf = Cstruct.create 4096 in
  Bigstring
    (fun () ->
      match Eio.Flow.single_read src buf with
      | len -> Some (Bigstring.sub_shared ~pos:0 ~len buf.buffer)
      | exception End_of_file -> None)

let fold env xlsx_filename json_filename () =
  let xlsx_path = sprintf "../../../test/files/%s" xlsx_filename in
  let json_path = sprintf "../../../test/files/%s" json_filename in
  let against = Eio.Path.(load (env#fs / json_path)) |> Yojson.Safe.from_string in
  let queue = Queue.create () in

  Switch.run (fun sw ->
    let src = Eio.Path.(open_in ~sw (env#fs / xlsx_path)) in
    let stream =
      SZXX.Zip.stream_files ~sw ~feed:(feed_bigstring src) (fun _ ->
        Fold_string { init = (); f = (fun _entry s () -> Queue.enqueue queue (`String s)) } )
    in
    let rec loop () =
      match Eio.Stream.take stream with
      | None -> ()
      | Some _ -> (loop [@tailcall]) ()
    in
    loop () );

  let parsed : Yojson.Safe.t = `Assoc [ "data", `List (List.rev (Queue.to_list queue)) ] in
  Json_diff.check parsed against

(* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
   Eio.Path.(env#fs / json_path)
   (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)) *)

let readme_example extract_filename src =
  let open SZXX in
  (* 1. Create a `feed` function. This README contains examples of this towards the end. *)
  let feed = feed_string src in

  (* 2. Create a callback function. Here we skip all files except the one we're interested in.
      If you deem the file(s) too large for `Action.String`,
      then look into `Action.Fold_string`, `Action.Fold_bigstring` and `Action.Angstrom` *)
  let callback = function
    | ({ filename; _ } : Zip.entry) when String.(filename = extract_filename) -> Zip.Action.String
    | _ -> Zip.Action.Skip
  in

  (* 3. Invoke `Zip.stream_files` *)
  Switch.run @@ fun sw ->
  let stream = Zip.stream_files ~sw ~feed callback in

  (* 4. Work with the stream, but DO NOT AWAIT this promise!
      Again, do not bind unto this promise yet! *)
  let unzipped =
    let rec loop () =
      match Eio.Stream.take stream with
      | Some (_entry, Zip.Data.String raw) -> Some raw
      | _ -> (loop [@tailcall]) ()
    in
    loop ()
  in

  (* 4. Bind/await the `success` promise to catch any error that may have terminated the stream early
      This could be due to a corrupted file or similar issues. *)

  (* 5. Finally we can bind/await the promise from step 3 and use it! *)
  match unzipped with
  | None -> failwithf "File `%s` not found in ZIP archive" extract_filename ()
  | Some _raw ->
    (* Use `raw` file contents *)
    ()

let test_readme_example env zip_path () =
  Eio.Path.with_open_in
    Eio.Path.(env#fs / sprintf "../../../test/files/%s" zip_path)
    (readme_example "xl/sharedStrings.xml")

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run ~verbose:true "SZXX ZIP"
    [
      ( "ZIP",
        [
          "financial.xlsx", `Quick, fold env "financial.xlsx" "chunks.json";
          "Readme example", `Quick, test_readme_example env "financial.xlsx";
        ] );
    ]
