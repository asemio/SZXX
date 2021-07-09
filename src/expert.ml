open! Core_kernel

type 'a backpressure = {
  controlled_ic: Lwt_io.input_channel;
  push: 'a -> unit;
  finalize: unit -> unit Lwt.t;
  stream: 'a Lwt_stream.t;
}

let backpressure input_channel =
  let mutex = Lwt_mutex.create () in
  let queue = Queue.create () in
  let push = Queue.enqueue queue in
  let mvar = Lwt_mvar.create_empty () in
  let rec flush () =
    match Queue.dequeue queue with
    | Some row ->
      let%lwt () = Lwt_mvar.put mvar row in
      flush ()
    | None -> Lwt.return_unit
  in
  let stream = Lwt_stream.from (fun () -> Lwt_mvar.take mvar) in
  let ic =
    let size = Lwt_io.default_buffer_size () in
    let buffer = Bytes.create size in
    Lwt_io.make ~buffer:(Lwt_bytes.create size) ~mode:Input (fun bytes offset len ->
        Lwt_mutex.with_lock mutex (fun () ->
            let%lwt () = flush () in
            let%lwt written = Lwt_io.read_into input_channel buffer offset len in
            Lwt_bytes.blit_from_bytes buffer offset bytes offset written;
            Lwt.return written))
  in
  let finalize () =
    push None;
    let%lwt () = Lwt_mutex.with_lock mutex flush in
    let%lwt () = Lwt_io.close input_channel in
    Lwt_io.close ic
  in
  { controlled_ic = ic; stream; push = (fun x -> push (Some x)); finalize }
