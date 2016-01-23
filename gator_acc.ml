open Lwt

type acc_ev = (string, int) Hashtbl.t
  (* Counters only, used to compute an average rate only. *)

type acc_val = (string, float list) Hashtbl.t
  (* Events associated with a value,
     used to compute sampling rate and average value. *)

type maxrate_acc = {
  time: unit -> float; (* returns time in whole seconds *)
  mutable t0: float; (* 1-second precision time *)
  mutable sum: float; (* number of events or sum of their associated values *)
  mutable maxrate: float;
}

type maxrate_tbl = (string, maxrate_acc) Hashtbl.t

type acc = {
  acc_ev: acc_ev;
  acc_val: acc_val;
  maxrate_acc_ev: maxrate_tbl;
  maxrate_acc_val: maxrate_tbl;
}

let create_acc () = {
  acc_ev = Hashtbl.create 100;
  acc_val = Hashtbl.create 100;
  maxrate_acc_ev = Hashtbl.create 100;
  maxrate_acc_val = Hashtbl.create 100;
}

(*
   The time function must return the time as a whole number of seconds.
*)
let create_maxrate_acc ?(time = Unix.time) () = {
  time = time;
  t0 = time ();
  sum = 0.;
  maxrate = 0.;
}

let add_to_maxrate_acc acc inc =
  let t = acc.time () in
  if t <> acc.t0 then (
    let rate = inc in
    acc.maxrate <- max rate acc.maxrate;
    acc.t0 <- t;
    acc.sum <- inc
  )
  else (
    acc.sum <- acc.sum +. inc;
    let rate = acc.sum in
    acc.maxrate <- max rate acc.maxrate;
  )

let add_val_to_maxrate_acc acc k v =
  let x =
    try Hashtbl.find acc k
    with Not_found ->
      let x = create_maxrate_acc () in
      Hashtbl.add acc k x;
      x
  in
  add_to_maxrate_acc x v

let add_ev_to_maxrate_acc acc k =
  add_val_to_maxrate_acc acc k 1.

let add_without_value {acc_ev; maxrate_acc_ev} k =
  let v0 =
    try Hashtbl.find acc_ev k
    with Not_found -> 0
  in
  Hashtbl.replace acc_ev k (v0 + 1);
  add_ev_to_maxrate_acc maxrate_acc_ev k

let add_with_value {acc_val; maxrate_acc_ev; maxrate_acc_val} k v =
  let vl =
    try Hashtbl.find acc_val k
    with Not_found -> []
  in
  Hashtbl.replace acc_val k (v :: vl);
  add_ev_to_maxrate_acc maxrate_acc_ev k;
  add_val_to_maxrate_acc maxrate_acc_val k v

let sum l =
  List.fold_left (+.) 0. l

let average l =
  assert (l <> []);
  List.fold_left (+.) 0. l /. float (List.length l)

let maxl l =
  assert (l <> []);
  List.fold_left max (List.hd l) l

let minl l =
  assert (l <> []);
  List.fold_left min (List.hd l) l

let median l =
  assert (l <> []);
  let a = Array.of_list l in
  Array.sort compare a;
  let n = Array.length a in
  if n mod 2 = 1 then
    a.(n/2)
  else
    0.5 *. (a.(n/2-1) +. a.(n/2))

let get_dimensions opt_ec2_instance_id =
  match opt_ec2_instance_id with
  | None -> []
  | Some ec2_instance_id ->
      [
        {
          Gator_aws_t.name = "InstanceId";
          value = ec2_instance_id;
        }
      ]

let flush_accumulators ~namespace ~period ~ec2_instance_id acc =
  add_without_value acc "gator.flush";
  let dimensions = get_dimensions ec2_instance_id in
  let points1 =
    Hashtbl.fold (fun k count l ->
      let rate = float count /. period in
      let k1 = k ^ ".ev.rate" in
      Gator_aws_v.(
        create_metric_data_point
          ~metric_name: k1
          ~value: rate
          ~dimensions
          ()
      ) :: l
    ) acc.acc_ev []
  in
  let points2 =
    Hashtbl.fold (fun k vl l ->
      let data = [
        k ^ ".ev.rate", float (List.length vl) /. period;
        k ^ ".val.sum", sum vl;
        k ^ ".val.rate", sum vl /. period;
        k ^ ".val.average", average vl;
        k ^ ".val.median", median vl;
        k ^ ".val.min", minl vl;
        k ^ ".val.max", maxl vl;
      ] in
      let jobs =
        List.map (fun (k, v) ->
          Gator_aws_v.create_metric_data_point
            ~metric_name: k
            ~value: v
            ~dimensions
            ()
        ) data
      in
      jobs @ l
    ) acc.acc_val []
  in
  let points3 =
    Hashtbl.fold (fun k v l ->
      let maxrate = v.maxrate in
      let k1 = k ^ ".ev.maxrate" in
      Gator_aws_v.create_metric_data_point
        ~metric_name: k1
        ~value: maxrate
        ~dimensions
        ()
      :: l
    ) acc.maxrate_acc_ev []
  in
  let points4 =
    Hashtbl.fold (fun k v l ->
      let maxrate = v.maxrate in
      let k1 = k ^ ".val.maxrate" in
      Gator_aws_v.create_metric_data_point
        ~metric_name: k1
        ~value: maxrate
        ~dimensions
        ()
      :: l
    ) acc.maxrate_acc_val []
  in
  Hashtbl.clear acc.acc_ev;
  Hashtbl.clear acc.acc_val;
  Hashtbl.clear acc.maxrate_acc_ev;
  Hashtbl.clear acc.maxrate_acc_val;
  Gator_aws.put_metric_data ~namespace (points1 @ points2 @ points3 @ points4)

let handle_request acc s =
  (match Gator_request.parse_request s with
   | k, None -> add_without_value acc k
   | k, Some v -> add_with_value acc k v
  );
  return ()

let test_maxrate () =
  (* Using a fake clock so we control when time switches from a whole
     second to the next *)
  let current_time = ref 0. in
  let time () = !current_time in
  let tick () = current_time := !current_time +. 1. in

  let acc = create_maxrate_acc ~time () in
  assert (acc.maxrate = 0.);
  add_to_maxrate_acc acc 1.;
  add_to_maxrate_acc acc 1.;
  assert (acc.maxrate = 2.);

  tick ();
  assert (acc.maxrate = 2.);
  add_to_maxrate_acc acc 1.;
  assert (acc.maxrate = 2.);
  add_to_maxrate_acc acc 1.;
  add_to_maxrate_acc acc 3.2;
  assert (acc.maxrate = 5.2);

  true

let tests = [
  "maxrate", test_maxrate;
]
