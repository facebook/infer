type t = bool =
  | false 
  | true 
val not : bool -> bool
external (&&) : bool -> bool -> bool = "%sequand"
external (||) : bool -> bool -> bool = "%sequor"
val equal : bool -> bool -> bool
val compare : bool -> bool -> int
val to_int : bool -> int
val to_float : bool -> float
val to_string : bool -> string
