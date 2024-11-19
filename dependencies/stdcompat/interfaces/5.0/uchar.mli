type t[@@immediate ]
val min : t
val max : t
val bom : t
val rep : t
val succ : t -> t
val pred : t -> t
val is_valid : int -> bool
val of_int : int -> t
val unsafe_of_int : int -> t
val to_int : t -> int
val is_char : t -> bool
val of_char : char -> t
val to_char : t -> char
val unsafe_to_char : t -> char
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
type utf_decode[@@immediate ]
val utf_decode_is_valid : utf_decode -> bool
val utf_decode_uchar : utf_decode -> t
val utf_decode_length : utf_decode -> int
val utf_decode : int -> t -> utf_decode
val utf_decode_invalid : int -> utf_decode
val utf_8_byte_length : t -> int
val utf_16_byte_length : t -> int
