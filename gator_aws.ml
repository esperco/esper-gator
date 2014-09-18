open Printf
open Log
open Lwt

let put_metric_data ~namespace ~metric_name ~value =
  let cmd_array = [|
    "aws"; "cloudwatch"; "put-metric-data";
    "--namespace"; namespace;
    "--metric-name"; metric_name;
    "--value"; sprintf "%g" value
  |] in
  let cmd = String.concat " " (Array.to_list cmd_array) in
  logf `Info "%s" cmd;
  Lwt_process.exec ("aws", cmd_array) >>= function
  | Unix.WEXITED 0 ->
      return ()
  | _ ->
      logf `Error "FAILED: %s" cmd;
      return ()
