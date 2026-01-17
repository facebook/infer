open! IStd

module Address : sig
  type t =
    | NonTop of int
    | Top

  val of_int : int -> t
  val leq : lhs:t -> rhs:t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val widen : prev:t -> next:t -> num_iters:int -> t
  val pp : Format.formatter -> t -> unit
end

module BlockId : sig
  type t = int

  val fresh : unit -> t
  val pp : Format.formatter -> t -> unit
end

module Value : sig
  type t =
    | Int of int
    | Ptr of { block : BlockId.t; offset : Address.t }
    | Top

  val of_int : int -> t
  val of_ptr : BlockId.t -> Address.t -> t
  val of_addr : Address.t -> t
  val eval_binop : Binop.t -> t -> t -> t
  val leq : lhs:t -> rhs:t -> bool
  val join : t -> t -> t
  val widen : prev:t -> next:t -> num_iters:int -> t
  val pp : Format.formatter -> t -> unit
end

type t

val empty : t

val pp : Format.formatter -> t -> unit

val leq : lhs:t -> rhs:t -> bool

val join : t -> t -> t

val widen : prev:t -> next:t -> num_iters:int -> t

val alloc_block : Value.t -> t -> t * Value.t

val free_block : BlockId.t -> t -> t * bool

val is_freed : BlockId.t -> t -> bool

val base : Address.t -> t -> Address.t option

val end_ : Address.t -> t -> Address.t option

val lookup_var : Var.t -> t -> Value.t

val store_var : Var.t -> Value.t -> t -> t