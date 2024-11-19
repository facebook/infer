val init : int -> unit
val full_init : int array -> unit
val self_init : unit -> unit
val bits : unit -> int
val int : int -> int
val full_int : int -> int
val int32 : Int32.t -> Int32.t
val nativeint : Nativeint.t -> Nativeint.t
val int64 : Int64.t -> Int64.t
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
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
  val bits32 : t -> Int32.t
  val bits64 : t -> Int64.t
  val nativebits : t -> Nativeint.t
end
val get_state : unit -> State.t
val set_state : State.t -> unit
