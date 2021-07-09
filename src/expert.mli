open! Core_kernel

type 'a backpressure = {
  controlled_ic: Lwt_io.input_channel;
  push: 'a -> unit;
  finalize: unit -> unit Lwt.t;
  stream: 'a Lwt_stream.t;
}

(**
  [SZXX.Expert.backpressure ic]

  [controlled_ic]: Pass this channel to [SZXX.ZIP.stream_files].
  [push]: Calling this function will populate the [stream], which will indirectly slow down [controlled_ic]. If you're using [SZXX.Zip.Action.Chunk], this is the function you must pass to it.
  [finalize]: Await this promise along with the promise returned by [SZXX.ZIP.stream_files].
  [stream]: Pull from this stream. Pulling faster will also make [controlled_ic] go faster.
*)
val backpressure : Lwt_io.input_channel -> 'a backpressure
