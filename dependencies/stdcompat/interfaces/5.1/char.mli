external code : char -> int = "%identity"
val chr : int -> char
val escaped : char -> string
val lowercase_ascii : char -> char
val uppercase_ascii : char -> char
type t = char
val compare : t -> t -> int
val equal : t -> t -> bool
val seeded_hash : int -> t -> int
val hash : t -> int
external unsafe_chr : int -> char = "%identity"
