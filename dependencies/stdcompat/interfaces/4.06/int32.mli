val zero : int32
val one : int32
val minus_one : int32
external neg : int32 -> int32 = "%int32_neg"
external add : int32 -> int32 -> int32 = "%int32_add"
external sub : int32 -> int32 -> int32 = "%int32_sub"
external mul : int32 -> int32 -> int32 = "%int32_mul"
external div : int32 -> int32 -> int32 = "%int32_div"
external rem : int32 -> int32 -> int32 = "%int32_mod"
val succ : int32 -> int32
val pred : int32 -> int32
val abs : int32 -> int32
val max_int : int32
val min_int : int32
external logand : int32 -> int32 -> int32 = "%int32_and"
external logor : int32 -> int32 -> int32 = "%int32_or"
external logxor : int32 -> int32 -> int32 = "%int32_xor"
val lognot : int32 -> int32
external shift_left : int32 -> int -> int32 = "%int32_lsl"
external shift_right : int32 -> int -> int32 = "%int32_asr"
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
external of_int : int -> int32 = "%int32_of_int"
external to_int : int32 -> int = "%int32_to_int"
external of_float :
  float -> int32 = "caml_int32_of_float" "caml_int32_of_float_unboxed"
[@@unboxed ][@@noalloc ]
external to_float :
  int32 -> float = "caml_int32_to_float" "caml_int32_to_float_unboxed"
[@@unboxed ][@@noalloc ]
external of_string : string -> int32 = "caml_int32_of_string"
val of_string_opt : string -> int32 option
val to_string : int32 -> string
external bits_of_float :
  float -> int32 = "caml_int32_bits_of_float"
    "caml_int32_bits_of_float_unboxed"[@@unboxed ][@@noalloc ]
external float_of_bits :
  int32 -> float = "caml_int32_float_of_bits"
    "caml_int32_float_of_bits_unboxed"[@@unboxed ][@@noalloc ]
type t = int32
val compare : t -> t -> int
val equal : t -> t -> bool
external format : string -> int32 -> string = "caml_int32_format"
