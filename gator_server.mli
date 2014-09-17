val create : ?port: int -> unit -> 'a
  (** Create a server listening on the given UDP port (default: 42222 *)

val main : offset: int -> 'a
  (** Read command-line arguments and launch server. *)
