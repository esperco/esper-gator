open Lwt

type acc1 = (string, int) Hashtbl.t
  (* Counters only, used to compute an average rate only. *)

type acc2 = (string, float list) Hashtbl.t
  (* Events associated with a value,
     used to compute sampling rate and average value. *)

type maxrate_acc = {
  time: unit -> float; (* returns time in whole seconds *)
  mutable t0: float; (* 1-second precision time *)
  mutable sum: int;
  mutable maxrate: float;
}

type acc3 = (string, maxrate_acc) Hashtbl.t

type acc = {
  acc1: acc1;
  acc2: acc2;
  acc3: acc3;
}

let create_acc () = {
  acc1 = Hashtbl.create 100;
  acc2 = Hashtbl.create 100;
  acc3 = Hashtbl.create 100;
}

(*
   The time function must return the time as a whole number of seconds.
*)
let create_maxrate_acc ?(time = Unix.time) () = {
  time = time;
  t0 = time ();
  sum = 0;
  maxrate = 0.;
}

let add_to_maxrate_acc acc =
  let t = acc.time () in
  if t <> acc.t0 then (
    let rate = 1. in
    acc.maxrate <- max rate acc.maxrate;
    acc.t0 <- t;
    acc.sum <- 1
  )
  else (
    acc.sum <- acc.sum + 1;
    let rate = float acc.sum in
    acc.maxrate <- max rate acc.maxrate;
  )

let add3 acc k =
  let v =
    try Hashtbl.find acc k
    with Not_found ->
      let v = create_maxrate_acc () in
      Hashtbl.add acc k v;
      v
  in
  add_to_maxrate_acc v

let add1 {acc1; acc3} k =
  let v0 =
    try Hashtbl.find acc1 k
    with Not_found -> 0
  in
  Hashtbl.replace acc1 k (v0 + 1);
  add3 acc3 k

let add2 {acc2; acc3} k v =
  let vl =
    try Hashtbl.find acc2 k
    with Not_found -> []
  in
  Hashtbl.replace acc2 k (v :: vl);
  add3 acc3 k

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

let flush_accumulators ~namespace ~period acc =
  add1 acc "gator.flush";
  let points1 =
    Hashtbl.fold (fun k count l ->
      let rate = float count /. period in
      let k1 = k ^ ".ev.rate" in
      Gator_aws_v.create_metric_data_point
        ~metric_name: k1
        ~value: rate
        ()
      :: l
    ) acc.acc1 []
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
            ()
        ) data
      in
      jobs @ l
    ) acc.acc2 []
  in
  let points3 =
    Hashtbl.fold (fun k v l ->
      let maxrate = v.maxrate in
      let k1 = k ^ ".ev.maxrate" in
      Gator_aws_v.create_metric_data_point
        ~metric_name: k1
        ~value: maxrate
        ()
      :: l
    ) acc.acc3 []
  in
  Hashtbl.clear acc.acc1;
  Hashtbl.clear acc.acc2;
  Hashtbl.clear acc.acc3;
  Gator_aws.put_metric_data ~namespace (points1 @ points2 @ points3)

let handle_request acc s =
  (match Gator_request.parse_request s with
   | k, None -> add1 acc k
   | k, Some v -> add2 acc k v
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
  add_to_maxrate_acc acc;
  add_to_maxrate_acc acc;
  assert (acc.maxrate = 2.);

  tick ();
  assert (acc.maxrate = 2.);
  add_to_maxrate_acc acc;
  assert (acc.maxrate = 2.);
  add_to_maxrate_acc acc;
  add_to_maxrate_acc acc;
  assert (acc.maxrate = 3.);

  true

let tests = [
  "maxrate", test_maxrate;
]
