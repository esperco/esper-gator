open Printf
open Lwt

let send
    ?(host = Gator_default.host)
    ?(port = Gator_default.port)
    key value =

  let socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  in

  let ipaddr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  let portaddr = Unix.ADDR_INET (ipaddr, port) in
  let msg = Gator_request.make_request key value in
  Lwt_unix.sendto socket msg 0 (String.length msg) [] portaddr >>= fun len ->
  assert (len = String.length msg);
  return ()


let main ~offset =
  let argv = Sys.argv in
  assert (offset <= Array.length argv - 1);

  let key = ref "" in
  let value = ref 1.0 in
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
         value := Gator_common.parse_value v
     | _ ->
         failwith "Invalid command-line arguments"
    );
    Lwt_main.run (send ~host: !host ~port: !port !key !value)

  with
  | Arg.Help _usage_msg -> usage (); exit 0
  | Arg.Bad _usage_msg -> usage (); exit 1
  | Failure s -> eprintf "%s\n%!" s; usage (); exit 1
