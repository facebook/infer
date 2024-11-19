type t = unit =
  | () 
val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : t -> string
