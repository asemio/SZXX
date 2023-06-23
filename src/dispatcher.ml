open Eio.Std

type job = Pack : (unit -> 'a) * ('a, exn) Result.t Eio.Promise.u -> job

type action =
  | Process of job
  | Quit of {
      atomic: int Atomic.t;
      target: int;
      all_done: unit Promise.u;
    }

type t = {
  stream: action Eio.Stream.t;
  num_domains: int;
  terminating: action Promise.t * action Promise.u;
  terminated: unit Promise.t * unit Promise.u;
}

let start_workers ~sw ~limit ~terminating stream =
  let capacity = ref limit in
  let condition = Eio.Condition.create () in
  let run_job job w =
    Fiber.fork ~sw (fun () ->
      decr capacity;
      Promise.resolve w
        (try Ok (job ()) with
        | exn -> Error exn);
      incr capacity;
      Eio.Condition.broadcast condition )
  in
  let rec loop () =
    let action =
      match Promise.peek terminating with
      | Some x -> x
      | None -> Fiber.first (fun () -> Eio.Stream.take stream) (fun () -> Promise.await terminating)
    in
    match action with
    | Process (Pack (job, w)) ->
      run_job job w;
      while !capacity = 0 do
        Eio.Condition.await_no_mutex condition
      done;
      (loop [@tailcall]) ()
    | Quit { atomic; target; all_done } ->
      while !capacity < limit do
        Eio.Condition.await_no_mutex condition
      done;
      if Atomic.fetch_and_add atomic 1 = target then Promise.resolve all_done ()
  in
  loop ()

let start_domain ~sw ~domain_mgr ~limit ~terminating ~transient stream =
  let go () =
    Eio.Domain_manager.run domain_mgr (fun () ->
      Switch.run @@ fun sw -> start_workers ~sw ~limit ~terminating stream )
  in
  match transient with
  | false -> Fiber.fork ~sw go
  | true ->
    Fiber.fork_daemon ~sw (fun () ->
      go ();
      `Stop_daemon )

let create ~sw ~num_domains ~domain_concurrency ?(capacity = 0) ?(transient = true) domain_mgr =
  let stream = Eio.Stream.create capacity in
  let instance =
    { stream; num_domains; terminating = Promise.create (); terminated = Promise.create () }
  in
  let terminating = fst instance.terminating in
  for _ = 1 to num_domains do
    start_domain ~sw ~domain_mgr ~limit:domain_concurrency ~terminating ~transient stream
  done;
  instance

let run { stream; _ } ~f =
  let p, w = Promise.create () in
  Eio.Stream.add stream (Process (Pack (f, w)));
  Promise.await p

let run_exn instance ~f =
  match run instance ~f with
  | Ok x -> x
  | Error exn -> raise exn

let terminate = function
| { terminating = p1, _; terminated = p2, _; _ } when Promise.is_resolved p1 -> Promise.await p2
| { num_domains; terminating = _, w1; terminated = p2, w2; _ } ->
  Promise.resolve w1 (Quit { atomic = Atomic.make 1; target = num_domains; all_done = w2 });
  Promise.await p2

let is_terminating { terminating = p, _; _ } = Promise.is_resolved p

let is_terminated { terminated = p, _; _ } = Promise.is_resolved p
