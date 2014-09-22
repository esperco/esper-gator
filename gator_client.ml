open Printf
open Lwt

(*
   lazy computation that is retried until it returns a value (rather than
   raising an exception).
*)
let lazy_retry f =
  let r = ref None in
  fun () ->
    match !r with
    | Some x -> x
    | None ->
        let x = f () in
        r := Some x;
        x

let make_send
    ?(host = Gator_default.host)
    ?(port = Gator_default.port)
    () =

  let get_socket_portaddr =
    lazy_retry (fun () ->
      let protocol =
        try
          (Unix.getprotobynumber 17)
          (*Unix.getprotobyname "udp"*).Unix.p_proto
        with Not_found -> failwith "Protocol not found"
      in
      let socket =
        Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM protocol
      in

      let ipaddr =
        try (Unix.gethostbyname host).Unix.h_addr_list.(0)
        with Not_found -> failwith ("Invalid host " ^ host)
      in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      socket, portaddr
    )
  in

  let send key opt_value =
    let socket, portaddr = get_socket_portaddr () in
    let msg = Gator_request.make_request key opt_value in
    Lwt_unix.sendto
      socket msg 0 (String.length msg) [] portaddr >>= fun len ->
    assert (len = String.length msg);
    return ()
  in
  send


let main ~offset =
  let argv = Sys.argv in
  assert (offset <= Array.length argv - 1);

  let key = ref "" in
  let value = ref None in
  let host = ref Gator_default.host in
  let port = ref Gator_default.port in
  let options = [
    "-host", Arg.Set_string host,
    sprintf "
          Host (default: %s)" Gator_default.host;
    "-port", Arg.Set_int port,
    sprintf "
          Port (default: %i)" Gator_default.port;
  ]
  in

  let anon = ref [] in
  let anon_fun s = anon := s :: !anon in

  let usage_msg =
    sprintf
      "Usage: %s KEY [VALUE] [OPTIONS]\nSupported options:\n"
      argv.(offset)
  in
  let usage () = Arg.usage options usage_msg in
  try
    Arg.parse_argv
      ~current: (ref offset) argv
      options anon_fun usage_msg;

    (match List.rev !anon with
     | [k] ->
         Gator_common.validate_key k;
         key := k
     | [k; v] ->
         Gator_common.validate_key k;
         key := k;
         value := Some (Gator_common.parse_value v)
     | _ ->
         failwith "Invalid command-line arguments"
    );
    Lwt_main.run (
      let rec loop () =
        let send = make_send ~host: !host ~port: !port () in
        send !key !value >>= fun () ->
        loop ()
      in
      loop ()
    )

  with
  | Arg.Help _usage_msg -> usage (); exit 0
  | Arg.Bad _usage_msg -> usage (); exit 1
  | Failure s -> eprintf "%s\n%!" s; usage (); exit 1
