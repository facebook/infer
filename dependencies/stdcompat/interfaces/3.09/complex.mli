type t = {
  re: float ;
  im: float }
val zero : t
val one : t
val i : t
val neg : t -> t
val conj : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val inv : t -> t
val div : t -> t -> t
val sqrt : t -> t
val norm2 : t -> float
val norm : t -> float
val arg : t -> float
val polar : float -> float -> t
val exp : t -> t
val log : t -> t
val pow : t -> t -> t
