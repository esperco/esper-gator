(* Utilities shared between server and client *)

let key_rex =
  let pat = "^[a-z][a-z0-9_]*\\(\\.[a-z][a-z0-9_]*\\)*$" in
  Str.regexp pat

let is_valid_key s =
  Str.string_match key_rex s 0

let validate_key s =
  if not (is_valid_key s) then
    failwith ("Not a valid key: " ^ s)

let parse_value s =
  try
    let v = float_of_string s in
    if v <= 0. || abs_float v = infinity || v <> v then
      raise Exit;
    v
  with _ ->
    failwith ("Not a positive number: " ^ s)
