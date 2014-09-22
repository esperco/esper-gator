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
    | Some x ->
        return x
    | None ->
        f () >>= fun x ->
        r := Some x;
        return x

let make_send
    ?(host = Gator_default.host)
    ?(port = Gator_default.port)
    () =

  let get_socket_portaddr =
    lazy_retry (fun () ->
      let proto_number =
        (* not using Unix.getprotobyname because it raises Not_found
           (possibly followed by a segfault)
           on EC2 under certain unknown conditions *)
        17
      in
      let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM proto_number in

      Lwt_unix.gethostbyname host >>= fun h ->
      let ipaddr = h.Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      return (socket, portaddr)
    )
  in

  let send key opt_value =
    get_socket_portaddr () >>= fun (socket, portaddr) ->
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
