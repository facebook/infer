type t
external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "obj_tag"
external set_tag : t -> int -> unit = "obj_set_tag"
external size : t -> int = "%obj_size"
external truncate : t -> int -> unit = "obj_truncate"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "obj_block"
external dup : t -> t = "obj_dup"
val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int
val double_tag : int
val double_array_tag : int
val custom_tag : int
val final_tag : int
val marshal : t -> string
val unmarshal : string -> int -> (t * int)
