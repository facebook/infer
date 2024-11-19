external neg : float -> float = "%negfloat"
external add : float -> float -> float = "%addfloat"
external sub : float -> float -> float = "%subfloat"
external mul : float -> float -> float = "%mulfloat"
external div : float -> float -> float = "%divfloat"
external rem : float -> float -> float = "caml_fmod_float" "fmod"[@@unboxed ]
[@@noalloc ]
external abs : float -> float = "%absfloat"
val infinity : float
val neg_infinity : float
val nan : float
val pi : float
val max_float : float
val min_float : float
val epsilon : float
external of_int : int -> float = "%floatofint"
external to_int : float -> int = "%intoffloat"
external of_string : string -> float = "caml_float_of_string"
val of_string_opt : string -> float option
val to_string : float -> string
type fpclass = fpclass =
  | FP_normal 
  | FP_subnormal 
  | FP_zero 
  | FP_infinite 
  | FP_nan 
external classify_float :
  ((float)[@unboxed ]) -> fpclass = "caml_classify_float"
    "caml_classify_float_unboxed"[@@noalloc ]
external pow : float -> float -> float = "caml_power_float" "pow"[@@unboxed ]
[@@noalloc ]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"[@@unboxed ]
[@@noalloc ]
external exp : float -> float = "caml_exp_float" "exp"[@@unboxed ][@@noalloc
                                                                    ]
external log : float -> float = "caml_log_float" "log"[@@unboxed ][@@noalloc
                                                                    ]
external log10 : float -> float = "caml_log10_float" "log10"[@@unboxed ]
[@@noalloc ]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"[@@unboxed ]
[@@noalloc ]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"[@@unboxed ]
[@@noalloc ]
external cos : float -> float = "caml_cos_float" "cos"[@@unboxed ][@@noalloc
                                                                    ]
external sin : float -> float = "caml_sin_float" "sin"[@@unboxed ][@@noalloc
                                                                    ]
external tan : float -> float = "caml_tan_float" "tan"[@@unboxed ][@@noalloc
                                                                    ]
external acos : float -> float = "caml_acos_float" "acos"[@@unboxed ]
[@@noalloc ]
external asin : float -> float = "caml_asin_float" "asin"[@@unboxed ]
[@@noalloc ]
external atan : float -> float = "caml_atan_float" "atan"[@@unboxed ]
[@@noalloc ]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
[@@unboxed ][@@noalloc ]
external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
[@@unboxed ][@@noalloc ]
external cosh : float -> float = "caml_cosh_float" "cosh"[@@unboxed ]
[@@noalloc ]
external sinh : float -> float = "caml_sinh_float" "sinh"[@@unboxed ]
[@@noalloc ]
external tanh : float -> float = "caml_tanh_float" "tanh"[@@unboxed ]
[@@noalloc ]
external ceil : float -> float = "caml_ceil_float" "ceil"[@@unboxed ]
[@@noalloc ]
external floor : float -> float = "caml_floor_float" "floor"[@@unboxed ]
[@@noalloc ]
external copysign :
  float -> float -> float = "caml_copysign_float" "caml_copysign"[@@unboxed ]
[@@noalloc ]
external frexp : float -> (float * int) = "caml_frexp_float"
external ldexp :
  ((float)[@unboxed ]) -> ((int)[@untagged ]) -> ((float)[@unboxed ]) =
    "caml_ldexp_float" "caml_ldexp_float_unboxed"[@@noalloc ]
external modf : float -> (float * float) = "caml_modf_float"
type t = float
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
module Array :
sig
  type t = floatarray
  external create : int -> t = "caml_floatarray_create"
  external length : t -> int = "%floatarray_length"
  external get : t -> int -> float = "%floatarray_safe_get"
  external set : t -> int -> float -> unit = "%floatarray_safe_set"
  external unsafe_get : t -> int -> float = "%floatarray_unsafe_get"
  external unsafe_set : t -> int -> float -> unit = "%floatarray_unsafe_set"
end
