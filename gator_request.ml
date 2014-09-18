open Printf

let remove_trailing_newline s =
  let len = String.length s in
  if len > 0 && s.[len-1] = '\n' then
    String.sub s 0 (len-1)
  else
    s

let split_rex =
  Str.regexp " +"

let parse_request s =
  let s = remove_trailing_newline s in
  match Str.split split_rex s with
  | [k; v] ->
      Gator_common.validate_key k;
      let v = Gator_common.parse_value v in
      (k, v)
  | [k] ->
      Gator_common.validate_key k;
      (k, 1.)
  | _ ->
      failwith ("Malformed request: " ^ s)

let make_request key value =
  Gator_common.validate_key key;
  Gator_common.validate_value value;
  let s = sprintf "%s %g" key value in
  if String.length s > 512 then
    failwith "Gator request exceeds legal value of 512";
  s
