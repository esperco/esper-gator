val create :
  ?namespace: string ->
  ?period: float ->
  ?port: int ->
  ?ec2_instance_id: string ->
  unit -> unit Lwt.t
  (** Create a server listening on the given UDP port.
      See defaults in gator_default.ml. *)

val main : offset: int -> 'a
  (** Read command-line arguments and launch server. *)
