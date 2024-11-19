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
val fold_left : f:('a -> char -> 'a) -> init:'a -> bytes -> 'a
val fold_right : f:(char -> 'a -> 'a) -> bytes -> init:'a -> 'a
val for_all : f:(char -> bool) -> bytes -> bool
val exists : f:(char -> bool) -> bytes -> bool
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
val uppercase : bytes -> bytes
val lowercase : bytes -> bytes
val capitalize : bytes -> bytes
val uncapitalize : bytes -> bytes
val uppercase_ascii : bytes -> bytes
val lowercase_ascii : bytes -> bytes
val capitalize_ascii : bytes -> bytes
val uncapitalize_ascii : bytes -> bytes
type t = bytes
val compare : t -> t -> int
val equal : t -> t -> bool
val starts_with : prefix:bytes -> bytes -> bool
val ends_with : suffix:bytes -> bytes -> bool
val unsafe_to_string : bytes -> string
val unsafe_of_string : string -> bytes
val split_on_char : sep:char -> bytes -> bytes list
val to_seq : t -> char Seq.t
val to_seqi : t -> (int * char) Seq.t
val of_seq : char Seq.t -> t
val get_uint8 : bytes -> int -> int
val get_int8 : bytes -> int -> int
val get_uint16_ne : bytes -> int -> int
val get_uint16_be : bytes -> int -> int
val get_uint16_le : bytes -> int -> int
val get_int16_ne : bytes -> int -> int
val get_int16_be : bytes -> int -> int
val get_int16_le : bytes -> int -> int
val get_int32_ne : bytes -> int -> int32
val get_int32_be : bytes -> int -> int32
val get_int32_le : bytes -> int -> int32
val get_int64_ne : bytes -> int -> int64
val get_int64_be : bytes -> int -> int64
val get_int64_le : bytes -> int -> int64
val set_uint8 : bytes -> int -> int -> unit
val set_int8 : bytes -> int -> int -> unit
val set_uint16_ne : bytes -> int -> int -> unit
val set_uint16_be : bytes -> int -> int -> unit
val set_uint16_le : bytes -> int -> int -> unit
val set_int16_ne : bytes -> int -> int -> unit
val set_int16_be : bytes -> int -> int -> unit
val set_int16_le : bytes -> int -> int -> unit
val set_int32_ne : bytes -> int -> int32 -> unit
val set_int32_be : bytes -> int -> int32 -> unit
val set_int32_le : bytes -> int -> int32 -> unit
val set_int64_ne : bytes -> int -> int64 -> unit
val set_int64_be : bytes -> int -> int64 -> unit
val set_int64_le : bytes -> int -> int64 -> unit
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
    "caml_blit_bytes"[@@noalloc ]
external unsafe_blit_string :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
    "caml_blit_string"[@@noalloc ]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_bytes"[@@noalloc ]
