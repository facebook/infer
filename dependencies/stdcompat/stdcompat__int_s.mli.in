module type S = sig
  type t = int
  val zero : int
  val one : int
  val minus_one : int
  external neg : int -> int = "%negint"
  external add : int -> int -> int = "%addint"
  external sub : int -> int -> int = "%subint"
  external mul : int -> int -> int = "%mulint"
  external div : int -> int -> int = "%divint"
  external rem : int -> int -> int = "%modint"
  external succ : int -> int = "%succint"
  external pred : int -> int = "%predint"
  val abs : int -> int
  val max_int : int
  val min_int : int
  external logand : int -> int -> int = "%andint"
  external logor : int -> int -> int = "%orint"
  external logxor : int -> int -> int = "%xorint"
  val lognot : int -> int
  external shift_left : int -> int -> int = "%lslint"
  external shift_right : int -> int -> int = "%asrint"
  external shift_right_logical : int -> int -> int = "%lsrint"
  val equal : int -> int -> bool
  val compare : int -> int -> int
  external to_float : int -> float = "%floatofint"
  external of_float : float -> int = "%intoffloat"
  val to_string : int -> string
end
