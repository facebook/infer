external length : bytes -> int = "%string_length"
external get : bytes -> int -> char = "%string_safe_get"
external set : bytes -> int -> char -> unit = "%string_safe_set"
external create : int -> bytes = "caml_create_string"
val make : int -> char -> bytes
val init : int -> f:(int -> char) -> bytes
val empty : bytes
val copy : bytes -> bytes
val of_string : string -> bytes
val to_string : bytes -> string
val sub : bytes -> pos:int -> len:int -> bytes
val sub_string : bytes -> int -> int -> string
val fill : bytes -> pos:int -> len:int -> char -> unit
val blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
val concat : sep:bytes -> bytes list -> bytes
val iter : f:(char -> unit) -> bytes -> unit
val iteri : f:(int -> char -> unit) -> bytes -> unit
val map : f:(char -> char) -> bytes -> bytes
val mapi : f:(int -> char -> char) -> bytes -> bytes
val trim : bytes -> bytes
val escaped : bytes -> bytes
val index : bytes -> char -> int
val rindex : bytes -> char -> int
val index_from : bytes -> int -> char -> int
val rindex_from : bytes -> int -> char -> int
val contains : bytes -> char -> bool
val contains_from : bytes -> int -> char -> bool
val rcontains_from : bytes -> int -> char -> bool
val uppercase : bytes -> bytes
val lowercase : bytes -> bytes
val capitalize : bytes -> bytes
val uncapitalize : bytes -> bytes
type t = bytes
val compare : t -> t -> int
external unsafe_get : bytes -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
    "caml_blit_string"[@@noalloc ]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_string"[@@noalloc
                                                                    ]
val unsafe_to_string : bytes -> string
val unsafe_of_string : string -> bytes
