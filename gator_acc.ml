open Lwt

type acc1 = (string, int) Hashtbl.t
  (* Counters only, used to compute an average rate only. *)

type acc2 = (string, float list) Hashtbl.t
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
  let vl =
    try Hashtbl.find acc2 k
    with Not_found -> []
  in
  Hashtbl.replace acc2 k (v :: vl)

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
      let k1 = k ^ ".rate" in
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
        k ^ ".rate", float (List.length vl) /. period;
        k ^ ".average", average vl;
        k ^ ".median", median vl;
        k ^ ".min", minl vl;
        k ^ ".max", maxl vl;
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
  Hashtbl.clear acc.acc1;
  Hashtbl.clear acc.acc2;
  Gator_aws.put_metric_data ~namespace (points1 @ points2)

let handle_request acc s =
  (match Gator_request.parse_request s with
   | k, None -> add1 acc k
   | k, Some v -> add2 acc k v
  );
  return ()
