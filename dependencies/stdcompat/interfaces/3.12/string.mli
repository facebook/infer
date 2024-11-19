external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
external create : int -> string = "caml_create_string"
val make : int -> char -> string
val copy : string -> string
val sub : string -> int -> int -> string
val fill : string -> int -> int -> char -> unit
val blit : string -> int -> string -> int -> int -> unit
val concat : string -> string list -> string
val iter : (char -> unit) -> string -> unit
val escaped : string -> string
val index : string -> char -> int
val rindex : string -> char -> int
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
val contains : string -> char -> bool
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
type t = string
val compare : t -> t -> int
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  string -> int -> string -> int -> int -> unit = "caml_blit_string"
    "noalloc"
external unsafe_fill :
  string -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
