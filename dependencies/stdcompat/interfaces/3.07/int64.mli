val zero : int64
val one : int64
val minus_one : int64
external neg : int64 -> int64 = "%int64_neg"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external rem : int64 -> int64 -> int64 = "%int64_mod"
val succ : int64 -> int64
val pred : int64 -> int64
val abs : int64 -> int64
val max_int : int64
val min_int : int64
external logand : int64 -> int64 -> int64 = "%int64_and"
external logor : int64 -> int64 -> int64 = "%int64_or"
external logxor : int64 -> int64 -> int64 = "%int64_xor"
val lognot : int64 -> int64
external shift_left : int64 -> int -> int64 = "%int64_lsl"
external shift_right : int64 -> int -> int64 = "%int64_asr"
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
external of_int : int -> int64 = "%int64_of_int"
external to_int : int64 -> int = "%int64_to_int"
external of_float : float -> int64 = "int64_of_float"
external to_float : int64 -> float = "int64_to_float"
external of_int32 : int32 -> int64 = "%int64_of_int32"
external to_int32 : int64 -> int32 = "%int64_to_int32"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"
external of_string : string -> int64 = "int64_of_string"
val to_string : int64 -> string
external bits_of_float : float -> int64 = "int64_bits_of_float"
external float_of_bits : int64 -> float = "int64_float_of_bits"
type t = int64
val compare : t -> t -> int
external format : string -> int64 -> string = "int64_format"
