open Printf
open Lwt

let send
    ?(host = Gator_default.host)
    ?(port = Gator_default.port)
    ?(value = 1.)
    key =
  return ()

let validate_key s =
  if not (Gator_common.is_valid_key s) then
    failwith ("Not a valid key: " ^ s)

let parse_value s =
  try
    let v = float_of_string s in
    if v <= 0. || abs_float v = infinity || v <> v then
      raise Exit;
    v
  with _ ->
    failwith ("Not a positive number: " ^ s)

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
         validate_key k;
         key := k
     | [k; v] ->
         validate_key k;
         key := k;
         value := parse_value v
     | _ ->
         failwith "Invalid command-line arguments"
    )

  with
  | Arg.Help _usage_msg -> usage (); exit 0
  | Arg.Bad _usage_msg -> usage (); exit 1
  | Failure s -> eprintf "%s\n%!" s; usage (); exit 1
