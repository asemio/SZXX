open! Base

type mismatch =
  | Changed of (Yojson.Safe.t * Yojson.Safe.t)
  | Added of Yojson.Safe.t
  | Deleted of Yojson.Safe.t

let color s = function
| `Yellow -> Printf.sprintf "\x1b[0;33m%s\x1b[m" s
| `Green -> Printf.sprintf "\x1b[0;32m%s\x1b[m" s
| `Red -> Printf.sprintf "\x1b[0;31m%s\x1b[m" s

let flatten (json : Yojson.Safe.t) =
  let rec loop json prefix acc =
    match json with
    | `Assoc pairs ->
      Hashtbl.add_exn acc ~key:prefix ~data:(`Assoc []);
      List.iter pairs ~f:(fun (key, value) -> loop value (Printf.sprintf "%s.%s" prefix key) acc)
    | `List values ->
      Hashtbl.add_exn acc ~key:prefix ~data:(`List []);
      List.iteri values ~f:(fun key value -> loop value (Printf.sprintf "%s[%d]" prefix key) acc)
    | x -> Hashtbl.add_exn acc ~key:prefix ~data:x
  in
  match json with
  | `Assoc _ ->
    let acc = Hashtbl.create (module String) in
    loop json "" acc;
    acc
  | x -> Printf.failwithf !"Cannot flatten a non-object: %{Yojson.Safe}" x ()

let check left right =
  let errors = Queue.create () in
  let mismatch s =
    Queue.enqueue errors s;
    None
  in
  let stringify = Yojson.Safe.to_string in
  let _merged =
    Hashtbl.merge (flatten left) (flatten right) ~f:(fun ~key -> function
      | `Left x ->
        Printf.sprintf "%s: %s" (color (Printf.sprintf "+++ %s" key) `Green) (stringify x) |> mismatch
      | `Right y ->
        Printf.sprintf "%s: %s" (color (Printf.sprintf "--- %s" key) `Red) (stringify y) |> mismatch
      | `Both (x, y) when Yojson.Safe.equal x y -> None
      | `Both (x, y) ->
        Printf.sprintf "%s: %s %s %s"
          (color (Printf.sprintf "+/- %s" key) `Yellow)
          (stringify x) (color "!=" `Yellow) (stringify y)
        |> mismatch )
  in
  match Queue.length errors with
  | 0 -> ()
  | n ->
    Eio.traceln !"%{Yojson.Safe.pretty_to_string}" left;
    Eio.traceln "JSON Mismatch (%d errors):\n%s\n" n
      (Queue.to_array errors |> String.concat_array ~sep:"\n");
    failwith "JSON Mismatch"
