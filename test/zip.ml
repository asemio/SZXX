let flags = Unix.[ O_RDONLY; O_NONBLOCK ]

open! Core
open Eio.Std

let get_file_path = sprintf "../../../test/files/%s"

let fold env xlsx_filename json_filename () =
  let xlsx_path = get_file_path xlsx_filename in
  let json_path = get_file_path json_filename in
  let against = Eio.Path.(load (Eio.Stdenv.fs env / json_path)) |> Yojson.Safe.from_string in
  let queue = Queue.create () in

  Switch.run (fun sw ->
    let src = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / xlsx_path)) in
    let seq =
      SZXX.Zip.stream_files ~sw ~feed:(SZXX.Feed.of_flow src) (fun _ ->
        Fold_string { init = (); f = (fun _entry s () -> Queue.enqueue queue (`String s)) } )
    in
    Sequence.iter seq ~f:(fun _ -> ());

    (* Test deadlocks: *)
    Sequence.iter seq ~f:(fun _ -> assert false) );

  let parsed : Yojson.Safe.t = `Assoc [ "data", `List (List.rev (Queue.to_list queue)) ] in
  Json_diff.check parsed against

(* Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
   Eio.Path.(Eio.Stdenv.fs env / json_path)
   (Eio.Flow.copy_string (Yojson.Safe.to_string parsed)) *)

let readme_example env filename () =
  let zip_path = get_file_path filename in
  let open SZXX in
  (* The Switch receives any parsing errors *)
  Switch.run @@ fun sw ->
  let file = Eio.Path.(open_in ~sw (Eio.Stdenv.fs env / zip_path)) in

  let callback = function
    | ({ filename; _ } : Zip.entry) when String.is_suffix (String.lowercase filename) ~suffix:".jpg" ->
      (* Here we'll simply extract each .jpg file into a string *)
      Zip.Action.String
    | _ -> Zip.Action.Skip
  in
  let seq = Zip.stream_files ~sw ~feed:(Feed.of_flow file) callback in

  let save_jpg ~filename:_ _contents = () in

  Sequence.iter seq ~f:(fun (entry, data) ->
    match data with
    | Zip.Data.String contents -> save_jpg ~filename:entry.filename contents
    | _ -> () )

let corrupted env filename () =
  try
    readme_example env filename ();
    raise Exit
  with
  | Failure msg when String.is_prefix msg ~prefix:"SZXX: Corrupted file." -> ()

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run ~verbose:true "SZXX ZIP"
    [
      ( "ZIP",
        [
          "financial.xlsx", `Quick, fold env "financial.xlsx" "chunks.json";
          "Readme example", `Quick, readme_example env "financial.xlsx";
          "Corrupted", `Quick, corrupted env "chunks.json";
        ] );
    ]
