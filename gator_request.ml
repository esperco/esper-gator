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
      (k, Some v)
  | [k] ->
      Gator_common.validate_key k;
      (k, None)
  | _ ->
      failwith ("Malformed request: " ^ s)

let make_request key opt_value =
  Gator_common.validate_key key;
  let s =
    match opt_value with
    | None -> key
    | Some value ->
        Gator_common.validate_value value;
        sprintf "%s %g" key value
  in
  if String.length s > 512 then
    failwith "Gator request exceeds maximum value of 512";
  Bytes.of_string s
