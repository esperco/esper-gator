open Printf
open Log
open Lwt

open Gator_aws_t

let write_to_temp_file metric_data =
  let fname = Filename.temp_file "gator-aws-" ".json" in
  Lwt_io.with_file ~mode: Lwt_io.output fname (fun oc ->
    let s = Gator_aws_j.string_of_metric_data metric_data in
    Lwt_io.write oc s
  ) >>= fun () ->
  let file_url = "file://" ^ fname in
  let finish () =
    (try Sys.remove fname
     with _ ->
       logf `Error "Cannot remove temporary file %s" fname
    );
    return ()
  in
  return (file_url, finish)

let really_put_metric_data ~namespace metric_data =
  if List.length metric_data > 20 then
    invalid_arg "Gator_aws.really_put_metric_data";

  write_to_temp_file metric_data >>= fun (file_url, finish) ->
  catch
    (fun () ->
       let cmd_array = [|
         "aws"; "cloudwatch"; "put-metric-data";
         "--namespace"; namespace;
         "--metric-data"; file_url;
       |] in
       let cmd = String.concat " " (Array.to_list cmd_array) in
       logf `Info "%s" cmd;
       Lwt_process.exec ("aws", cmd_array) >>= function
       | Unix.WEXITED 0 ->
           finish ()
       | _ ->
           logf `Error "FAILED: %s" cmd;
           finish ()
    )
    (fun e ->
       finish ()
    )

let rec chunkify max_length l =
  let chunk, rest = BatList.takedrop max_length l in
  match rest with
  | [] -> [chunk]
  | rest -> chunk :: chunkify max_length rest

let put_metric_data ~namespace metric_data =
  Lwt_list.iter_p (fun chunk ->
    really_put_metric_data ~namespace chunk
  ) (chunkify 20 metric_data)
