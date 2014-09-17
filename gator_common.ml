(* Utilities shared between server and client *)

let key_rex =
  let pat = "^[a-z][a-z0-9_]*\\(\\.[a-z][a-z0-9_]*\\)*$" in
  Str.regexp pat

let is_valid_key s =
  Str.string_match key_rex s 0
