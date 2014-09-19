val send : ?host: string -> ?port: int -> string -> float option -> unit Lwt.t
  (** Send a (key, value option) pair to the server.
      The key is a non-empty dot-separated path, each component
      being of the form [a-z][a-z0-9_]*
      Valid keys include:
        foo
        foo.bar
        a_b1.x.y.z

      The payload is (key ^ " " ^ string_of_float value).
      The total length of the payload is limited to 512 bytes
      (safe limit for UDP).

      The default host is "127.0.0.1" and the default port is 42222.
  *)

val main : offset:int -> unit
  (** Read command-line arguments and send an event to the server. *)
