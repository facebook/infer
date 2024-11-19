module type S = sig
type t = char
(** Alias for {!Char.t} *)

val seeded_hash : int -> t -> int
(** @since 5.1.0: val seeded_hash : int -> t -> int *)

val hash : t -> int
(** @since 5.1.0: val hash : t -> int *)

val lowercase_ascii : char -> char
(** @since 4.03.0: val lowercase_ascii : char -> char *)

val uppercase_ascii : char -> char
(** @since 4.03.0: val uppercase_ascii : char -> char *)

val equal : t -> t -> bool
(** @since 4.03.0: val equal : t -> t -> bool *)

external code : char -> int = "%identity"
(** Alias for {!Char.code} *)

val chr : int -> char
(** Alias for {!Char.chr} *)

val escaped : char -> string
(** Alias for {!Char.escaped} *)

val compare : t -> t -> int
(** Alias for {!Char.compare} *)

external unsafe_chr : int -> char = "%identity"
(** Alias for {!Char.unsafe_chr} *)

end
