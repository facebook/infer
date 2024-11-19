type t
val min : t
val max : t
val succ : t -> t
val pred : t -> t
val is_valid : int -> bool
val of_int : int -> t
val unsafe_of_int : int -> t
val to_int : t -> int
val is_char : t -> bool
val of_char : char -> t
val to_char : t -> char
val unsafe_to_char : t -> char
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
