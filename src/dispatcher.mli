open! Core

type t

(** Creates a new workpool with [num_domains].
    [domain_concurrency] is the maximum number of jobs that each domain can run at a time.
    [capacity] (default: 0) is identical to the [Eio.Stream.create] capacity parameter.
    [transient] (default: true). When true, the workpool will not block the Switch from completing.
      When false, [terminate] must be called to resolve the [sw] Switch. *)
val create :
  sw:Eio.Switch.t ->
  num_domains:int ->
  domain_concurrency:int ->
  ?capacity:int ->
  ?transient:bool ->
  #Eio.Domain_manager.t ->
  t

(** Run a job on this workpool. It is placed at the end of the queue. *)
val run : t -> f:(unit -> 'a) -> ('a, exn) result

val run_exn : t -> f:(unit -> 'a) -> 'a

(** Waits for all running jobs to complete, then returns.
    No new jobs are started, even if they were already enqueued.
    To abort all running jobs instead of waiting for them, call [Switch.fail] on the Switch used to create this workpool *)
val terminate : t -> unit

(** Returns true if the [terminate] function has been called on this workpool.
    Also returns true if the workpool has fully terminated. *)
val is_terminating : t -> bool

(** Returns true if the the workpool has fully terminated (all running jobs have finished). *)
val is_terminated : t -> bool
