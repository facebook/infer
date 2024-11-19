val init : int -> unit
val full_init : int array -> unit
val self_init : unit -> unit
val bits : unit -> int
val int : int -> int
val full_int : int -> int
val int_in_range : min:int -> max:int -> int
val int32 : Int32.t -> Int32.t
val int32_in_range : min:int32 -> max:int32 -> int32
val nativeint : Nativeint.t -> Nativeint.t
val nativeint_in_range : min:nativeint -> max:nativeint -> nativeint
val int64 : Int64.t -> Int64.t
val int64_in_range : min:int64 -> max:int64 -> int64
val float : float -> float
val bool : unit -> bool
val bits32 : unit -> Int32.t
val bits64 : unit -> Int64.t
val nativebits : unit -> Nativeint.t
module State :
sig
  type t
  val make : int array -> t
  val make_self_init : unit -> t
  val copy : t -> t
  val bits : t -> int
  val int : t -> int -> int
  val full_int : t -> int -> int
  val int_in_range : t -> min:int -> max:int -> int
  val int32 : t -> Int32.t -> Int32.t
  val int32_in_range : t -> min:int32 -> max:int32 -> int32
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val nativeint_in_range : t -> min:nativeint -> max:nativeint -> nativeint
  val int64 : t -> Int64.t -> Int64.t
  val int64_in_range : t -> min:int64 -> max:int64 -> int64
  val float : t -> float -> float
  val bool : t -> bool
  val bits32 : t -> Int32.t
  val bits64 : t -> Int64.t
  val nativebits : t -> Nativeint.t
  val split : t -> t
  val to_binary_string : t -> string
  val of_binary_string : string -> t
end
val get_state : unit -> State.t
val set_state : State.t -> unit
val split : unit -> State.t
