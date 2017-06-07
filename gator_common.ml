(* Utilities shared between server and client *)

let key_rex =
  let pat = "^[a-z0-9_-]+\\(\\.[a-z0-9_-]+\\)*$" in
  Str.regexp pat

let is_valid_key s =
  Str.string_match key_rex s 0

let validate_key s =
  if not (is_valid_key s) then
    failwith ("Not a valid key: " ^ s)

let validate_value v =
  if v < 0. || abs_float v = infinity || v <> v then
    failwith ("Not a positive number: " ^ string_of_float v)

let parse_value s =
  let v =
    try float_of_string s
    with _ -> failwith ("Not a positive number: " ^ s)
  in
  validate_value v;
  v

let turn_into_key ~keep_periods s0 =
  let s = Bytes.lowercase_ascii (Bytes.of_string s0) in
  for i = 0 to Bytes.length s - 1 do
    match Bytes.get s i with
    | 'a'..'z'
    | '0'..'9'
    | '_' | '-' -> ()
    | '.' when keep_periods -> ()
    | _ -> Bytes.set s i '_'
  done;
  Bytes.to_string s
