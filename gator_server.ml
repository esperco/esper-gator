open Printf

let handle_request s =
  printf "Request: %S\n%!" s

(* Redirect stdout and stderr to the same log file *)
let redirect_stdout_stderr fname =
  let log_fd =
    Unix.openfile fname [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o666
  in
  Unix.dup2 log_fd Unix.stdout;
  Unix.dup2 log_fd Unix.stderr

let init_logging log_file =
  Printexc.record_backtrace true;
  redirect_stdout_stderr log_file

let create ?(port = Gator_default.port) () =
  let socket =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_any, port));

  let buf = String.create 65536 in
  while true do
    let len, sender_addr = Unix.recvfrom socket buf 0 (String.length buf) [] in
    if len >= 0 then
      handle_request (String.sub buf 0 len)
  done;
  assert false

let main ~offset =
    let argv = Sys.argv in
  assert (offset <= Array.length argv - 1);

  let foreground = ref false in
  let port = ref Gator_default.port in
  let log_filename = ref Gator_default.server_log in
  let options = [
    "-fg", Arg.Set foreground,
    "
          Run in the foreground, don't redirect stdout/stderr to log file";

    "-log", Arg.Set_string log_filename,
    sprintf "
          Log file (default: %s)" Gator_default.server_log;

    "-port", Arg.Set_int port,
    sprintf "
          Port (default: %i)" Gator_default.port;
  ]
  in

  let usage_msg =
    sprintf
      "Usage: %s [OPTIONS]\nSupported options:\n"
      argv.(offset)
  in
  let anon_fun s =
    failwith ("Invalid command-line argument: " ^ s)
  in
  let usage () = Arg.usage options usage_msg in
  try
    Arg.parse_argv
      ~current: (ref offset) argv
      options anon_fun usage_msg;

    let run () = create ~port: !port () in
    if !foreground then
      run ()
    else (
      init_logging !log_filename;
      if Unix.fork () = 0 then
        run ()
      else
        exit 0
    )

  with
  | Arg.Help _usage_msg -> usage (); exit 0
  | Arg.Bad _usage_msg -> usage (); exit 1
  | Failure s -> eprintf "%s\n%!" s; usage (); exit 1
