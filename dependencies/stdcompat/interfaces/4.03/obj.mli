type t
external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
val double_field : t -> int -> float
val set_double_field : t -> int -> float -> unit
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int
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
val final_tag : int[@@ocaml.deprecated "Replaced by custom_tag."]
val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int
val extension_constructor : 'a -> extension_constructor
val extension_name : extension_constructor -> string
val extension_id : extension_constructor -> int
val marshal : t -> bytes[@@ocaml.deprecated "Use Marshal.to_bytes instead."]
val unmarshal : bytes -> int -> (t * int)[@@ocaml.deprecated
                                           "Use Marshal.from_bytes and Marshal.total_size instead."]
module Ephemeron :
sig
  type obj_t = t
  type t
  val create : int -> t
  val length : t -> int
  val get_key : t -> int -> obj_t option
  val get_key_copy : t -> int -> obj_t option
  val set_key : t -> int -> obj_t -> unit
  val unset_key : t -> int -> unit
  val check_key : t -> int -> bool
  val blit_key : t -> int -> t -> int -> int -> unit
  val get_data : t -> obj_t option
  val get_data_copy : t -> obj_t option
  val set_data : t -> obj_t -> unit
  val unset_data : t -> unit
  val check_data : t -> bool
  val blit_data : t -> t -> unit
end
