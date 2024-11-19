external code : char -> int = "%identity"
val chr : int -> char
val escaped : char -> string
val lowercase : char -> char[@@ocaml.deprecated
                              "Use Char.lowercase_ascii instead."]
val uppercase : char -> char[@@ocaml.deprecated
                              "Use Char.uppercase_ascii instead."]
val lowercase_ascii : char -> char
val uppercase_ascii : char -> char
type t = char
val compare : t -> t -> int
val equal : t -> t -> bool
external unsafe_chr : int -> char = "%identity"
