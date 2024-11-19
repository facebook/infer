external raise : exn -> 'a = "%raise"
val invalid_arg : string -> 'a
val failwith : string -> 'a
exception Exit 
external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"
external not : bool -> bool = "%boolnot"
external (&&) : bool -> bool -> bool = "%sequand"
external (&) : bool -> bool -> bool = "%sequand"
external (||) : bool -> bool -> bool = "%sequor"
external (or) : bool -> bool -> bool = "%sequor"
external (~-) : int -> int = "%negint"
external (~+) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"
val abs : int -> int
val max_int : int
val min_int : int
external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"
val lnot : int -> int
external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"
external (~-.) : float -> float = "%negfloat"
external (~+.) : float -> float = "%identity"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
external exp : float -> float = "caml_exp_float" "exp" "float"
external log : float -> float = "caml_log_float" "log" "float"
external log10 : float -> float = "caml_log10_float" "log10" "float"
external expm1 : float -> float = "caml_expm1_float" "caml_expm1" "float"
external log1p : float -> float = "caml_log1p_float" "caml_log1p" "float"
external cos : float -> float = "caml_cos_float" "cos" "float"
external sin : float -> float = "caml_sin_float" "sin" "float"
external tan : float -> float = "caml_tan_float" "tan" "float"
external acos : float -> float = "caml_acos_float" "acos" "float"
external asin : float -> float = "caml_asin_float" "asin" "float"
external atan : float -> float = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
external cosh : float -> float = "caml_cosh_float" "cosh" "float"
external sinh : float -> float = "caml_sinh_float" "sinh" "float"
external tanh : float -> float = "caml_tanh_float" "tanh" "float"
external ceil : float -> float = "caml_ceil_float" "ceil" "float"
external floor : float -> float = "caml_floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float :
  float -> float -> float = "caml_fmod_float" "fmod" "float"
external frexp : float -> (float * int) = "caml_frexp_float"
external ldexp : float -> int -> float = "caml_ldexp_float"
external modf : float -> (float * float) = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
val infinity : float
val neg_infinity : float
val nan : float
val max_float : float
val min_float : float
val epsilon_float : float
type fpclass =
  | FP_normal 
  | FP_subnormal 
  | FP_zero 
  | FP_infinite 
  | FP_nan 
external classify_float : float -> fpclass = "caml_classify_float"
val (^) : string -> string -> string
external int_of_char : char -> int = "%identity"
val char_of_int : int -> char
external ignore : 'a -> unit = "%ignore"
val string_of_bool : bool -> string
val bool_of_string : string -> bool
val string_of_int : int -> string
external int_of_string : string -> int = "caml_int_of_string"
val string_of_float : float -> string
external float_of_string : string -> float = "caml_float_of_string"
external fst : ('a * 'b) -> 'a = "%field0"
external snd : ('a * 'b) -> 'b = "%field1"
val (@) : 'a list -> 'a list -> 'a list
type in_channel
type out_channel
val stdin : in_channel
val stdout : out_channel
val stderr : out_channel
val print_char : char -> unit
val print_string : string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_endline : string -> unit
val print_newline : unit -> unit
val prerr_char : char -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_float : float -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit
val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float
type open_flag =
  | Open_rdonly 
  | Open_wronly 
  | Open_append 
  | Open_creat 
  | Open_trunc 
  | Open_excl 
  | Open_binary 
  | Open_text 
  | Open_nonblock 
val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
val flush : out_channel -> unit
val flush_all : unit -> unit
val output_char : out_channel -> char -> unit
val output_string : out_channel -> string -> unit
val output : out_channel -> string -> int -> int -> unit
val output_byte : out_channel -> int -> unit
val output_binary_int : out_channel -> int -> unit
val output_value : out_channel -> 'a -> unit
val seek_out : out_channel -> int -> unit
val pos_out : out_channel -> int
val out_channel_length : out_channel -> int
val close_out : out_channel -> unit
val close_out_noerr : out_channel -> unit
val set_binary_mode_out : out_channel -> bool -> unit
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_in_gen : open_flag list -> int -> string -> in_channel
val input_char : in_channel -> char
val input_line : in_channel -> string
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
val input_byte : in_channel -> int
val input_binary_int : in_channel -> int
val input_value : in_channel -> 'a
val seek_in : in_channel -> int -> unit
val pos_in : in_channel -> int
val in_channel_length : in_channel -> int
val close_in : in_channel -> unit
val close_in_noerr : in_channel -> unit
val set_binary_mode_in : in_channel -> bool -> unit
module LargeFile :
sig
  val seek_out : out_channel -> int64 -> unit
  val pos_out : out_channel -> int64
  val out_channel_length : out_channel -> int64
  val seek_in : in_channel -> int64 -> unit
  val pos_in : in_channel -> int64
  val in_channel_length : in_channel -> int64
end
type 'a ref = {
  mutable contents: 'a }
external ref : 'a -> 'a ref = "%makemutable"
external (!) : 'a ref -> 'a = "%field0"
external (:=) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6 =
    "%identity"
val (^^) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
    ('f, 'b, 'c, 'e, 'g, 'h) format6 -> ('a, 'b, 'c, 'd, 'g, 'h) format6
val exit : int -> 'a
val at_exit : (unit -> unit) -> unit
val valid_float_lexem : string -> string
val unsafe_really_input : in_channel -> string -> int -> int -> unit
val do_at_exit : unit -> unit
