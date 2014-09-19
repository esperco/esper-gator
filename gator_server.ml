(* Event aggregator UDP server *)

(*
   Testing:

   echo "yo" | nc -uw0 localhost 42222
*)

open Printf
open Log
open Lwt


let rec create_timer period action =
  Lwt_unix.sleep period >>= fun () ->
  (* start next period immediately *)
  let next_timer = create_timer period action in
  catch
    (fun () -> action ())
    (fun e ->
       logf `Error "%s" (string_of_exn e);
      return ()
    )
  >>= fun () ->
  (* finish after timer or after action, whichever finishes last *)
  next_timer


(* Redirect stdout and stderr to the same log file *)
let redirect_stdout_stderr fname =
  let log_fd =
    Unix.openfile fname [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o666
  in
  Unix.dup2 log_fd Unix.stdout;
  Unix.dup2 log_fd Unix.stderr

let init_logging opt_file =
  Printexc.record_backtrace true;
  Log.service := "gator";
  match opt_file with
  | Some file -> redirect_stdout_stderr file
  | None -> ()

let create
    ?(namespace = Gator_default.namespace)
    ?(period = Gator_default.period)
    ?(port = Gator_default.port)
    () =
  let socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  in
  Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_any, port));

  let accumulators = Gator_acc.create_acc () in

  let buf = Lwt_bytes.create 65536 in
  let rec server_loop () =
    Lwt_bytes.recvfrom socket buf 0 (Lwt_bytes.length buf) []
    >>= fun (len, sender_addr) ->
    (if len >= 0 then (
       try
         let s = Lwt_bytes.to_string (Lwt_bytes.extract buf 0 len) in
         Gator_acc.handle_request accumulators s
       with e ->
         let msg = string_of_exn e in
         logf `Error "Exception %s" msg;
         return ()
     )
     else return ()
    ) >>= fun () ->
    server_loop ()
  in
  let periodic_job =
    create_timer period
      (fun () -> Gator_acc.flush_accumulators ~namespace ~period accumulators)
  in
  let all = join [ server_loop (); periodic_job ] in
  all

let main ~offset =
    let argv = Sys.argv in
  assert (offset <= Array.length argv - 1);

  let foreground = ref false in
  let log_filename = ref Gator_default.server_log in
  let namespace = ref Gator_default.namespace in
  let period = ref Gator_default.period in
  let port = ref Gator_default.port in
  let options = [
    "-fg", Arg.Set foreground,
    "
          Run in the foreground, don't redirect stdout/stderr to log file";

    "-log", Arg.Set_string log_filename,
    sprintf "
          Log file (default: %s)" Gator_default.server_log;

    "-ns", Arg.Set_string namespace,
    sprintf "
          Cloudwatch namespace (default: %s)" Gator_default.namespace;

    "-period", Arg.Set_float period,
    sprintf "
          Period between flushes (default: %g seconds)" Gator_default.period;

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

    let run () =
      let jobs =
        create ~namespace: !namespace ~port: !port ~period: !period () in
      Lwt_main.run jobs;
      assert false
    in
    if !foreground then (
      init_logging None;
      run ()
    )
    else if Lwt_unix.fork () = 0 then (
      init_logging (Some !log_filename);
      run ()
    )
    else
      exit 0

  with
  | Arg.Help _usage_msg -> usage (); exit 0
  | Arg.Bad _usage_msg -> usage (); exit 1
  | Failure s -> eprintf "%s\n%!" s; usage (); exit 1
