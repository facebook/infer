val zero : nativeint
val one : nativeint
val minus_one : nativeint
external neg : nativeint -> nativeint = "%nativeint_neg"
external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
val unsigned_div : nativeint -> nativeint -> nativeint
external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
val unsigned_rem : nativeint -> nativeint -> nativeint
val succ : nativeint -> nativeint
val pred : nativeint -> nativeint
val abs : nativeint -> nativeint
val size : int
val max_int : nativeint
val min_int : nativeint
external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
val lognot : nativeint -> nativeint
external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical :
  nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int : int -> nativeint = "%nativeint_of_int"
external to_int : nativeint -> int = "%nativeint_to_int"
val unsigned_to_int : nativeint -> int option
external of_float :
  float -> nativeint = "caml_nativeint_of_float"
    "caml_nativeint_of_float_unboxed"[@@unboxed ][@@noalloc ]
external to_float :
  nativeint -> float = "caml_nativeint_to_float"
    "caml_nativeint_to_float_unboxed"[@@unboxed ][@@noalloc ]
external of_int32 : int32 -> nativeint = "%nativeint_of_int32"
external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
external of_string : string -> nativeint = "caml_nativeint_of_string"
val of_string_opt : string -> nativeint option
val to_string : nativeint -> string
type t = nativeint
val compare : t -> t -> int
val unsigned_compare : t -> t -> int
val equal : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
external format : string -> nativeint -> string = "caml_nativeint_format"
