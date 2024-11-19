external length : bytes -> int = "%bytes_length"
external get : bytes -> int -> char = "%bytes_safe_get"
external set : bytes -> int -> char -> unit = "%bytes_safe_set"
external create : int -> bytes = "caml_create_bytes"
val make : int -> char -> bytes
val init : int -> f:(int -> char) -> bytes
val empty : bytes
val copy : bytes -> bytes
val of_string : string -> bytes
val to_string : bytes -> string
val sub : bytes -> pos:int -> len:int -> bytes
val sub_string : bytes -> pos:int -> len:int -> string
val extend : bytes -> left:int -> right:int -> bytes
val fill : bytes -> pos:int -> len:int -> char -> unit
val blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
val blit_string :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
val concat : sep:bytes -> bytes list -> bytes
val cat : bytes -> bytes -> bytes
val iter : f:(char -> unit) -> bytes -> unit
val iteri : f:(int -> char -> unit) -> bytes -> unit
val map : f:(char -> char) -> bytes -> bytes
val mapi : f:(int -> char -> char) -> bytes -> bytes
val trim : bytes -> bytes
val escaped : bytes -> bytes
val index : bytes -> char -> int
val index_opt : bytes -> char -> int option
val rindex : bytes -> char -> int
val rindex_opt : bytes -> char -> int option
val index_from : bytes -> int -> char -> int
val index_from_opt : bytes -> int -> char -> int option
val rindex_from : bytes -> int -> char -> int
val rindex_from_opt : bytes -> int -> char -> int option
val contains : bytes -> char -> bool
val contains_from : bytes -> int -> char -> bool
val rcontains_from : bytes -> int -> char -> bool
val uppercase : bytes -> bytes[@@ocaml.deprecated
                                "Use Bytes.uppercase_ascii instead."]
val lowercase : bytes -> bytes[@@ocaml.deprecated
                                "Use Bytes.lowercase_ascii instead."]
val capitalize : bytes -> bytes[@@ocaml.deprecated
                                 "Use Bytes.capitalize_ascii instead."]
val uncapitalize : bytes -> bytes[@@ocaml.deprecated
                                   "Use Bytes.uncapitalize_ascii instead."]
val uppercase_ascii : bytes -> bytes
val lowercase_ascii : bytes -> bytes
val capitalize_ascii : bytes -> bytes
val uncapitalize_ascii : bytes -> bytes
type t = bytes
val compare : t -> t -> int
val equal : t -> t -> bool
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
    "caml_blit_bytes"[@@noalloc ]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_bytes"[@@noalloc ]
val unsafe_to_string : bytes -> string
val unsafe_of_string : string -> bytes
