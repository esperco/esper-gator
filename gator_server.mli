val create : ?port: int -> ?period: float -> unit -> unit Lwt.t
  (** Create a server listening on the given UDP port.
      See defaults in gator_default.ml. *)

val main : offset: int -> 'a
  (** Read command-line arguments and launch server. *)
