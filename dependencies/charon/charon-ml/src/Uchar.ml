include Stdlib.Uchar

let pp fmt u =
  if is_char u then Format.fprintf fmt "'%c'" (to_char u)
  else Format.fprintf fmt "'\\x%x'" (to_int u)

let to_string u = Format.asprintf "%a" pp u
