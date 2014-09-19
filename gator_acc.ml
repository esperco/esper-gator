open Lwt

type acc1 = (string, int) Hashtbl.t
  (* Counters only, used to compute an average rate only. *)

type acc2 = (string, (int * float)) Hashtbl.t
  (* Events associated with a value,
     used to compute sampling rate and average value. *)

type acc = { acc1: acc1; acc2: acc2 }

let create_acc () = {
  acc1 = Hashtbl.create 100;
  acc2 = Hashtbl.create 100;
}

let add1 {acc1} k =
  let v0 =
    try Hashtbl.find acc1 k
    with Not_found -> 0
  in
  Hashtbl.replace acc1 k (v0 + 1)

let add2 {acc2} k v =
  let n0, v0 =
    try Hashtbl.find acc2 k
    with Not_found -> (0, 0.)
  in
  Hashtbl.replace acc2 k (n0 + 1, v0 +. v)

let flush_accumulators ~namespace ~period acc =
  add1 acc "gator.flush";
  let jobs1 =
    Hashtbl.fold (fun k count l ->
      let rate = float count /. period in
      let k1 = k ^ ".rate" in
      Gator_aws.put_metric_data ~namespace ~metric_name: k1  ~value: rate
      :: l
    ) acc.acc1 []
  in
  let jobs2 =
    Hashtbl.fold (fun k (count, sum) l ->
      let rate = float count /. period in
      let average = sum /. float count in
      let k1 = k ^ ".rate" in
      let k2 = k ^ ".average" in
      let job1 =
        Gator_aws.put_metric_data ~namespace ~metric_name: k1 ~value: rate
      in
      let job2 =
        Gator_aws.put_metric_data ~namespace ~metric_name: k2 ~value: average
      in
      job1 :: job2 :: l
    ) acc.acc2 []
  in
  Hashtbl.clear acc.acc1;
  Hashtbl.clear acc.acc2;
  join (jobs1 @ jobs2)

let handle_request acc s =
  (match Gator_request.parse_request s with
   | k, None -> add1 acc k
   | k, Some v -> add2 acc k v
  );
  return ()
