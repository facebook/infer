(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions

    Pure (heap-independent) expressions are complex arithmetic,
    bitwise-logical, etc. operations over literal values and registers. *)

type op1 =
  | Convert of {dst: Typ.t; signed: bool}
      (** Convert between specified types, possibly with loss of information *)
  | Select of int  (** Select an index from a record *)
[@@deriving compare, equal, hash, sexp]

type op2 =
  | Eq  (** Equal test *)
  | Dq  (** Disequal test *)
  | Gt  (** Greater-than test *)
  | Ge  (** Greater-than-or-equal test *)
  | Lt  (** Less-than test *)
  | Le  (** Less-than-or-equal test *)
  | Ugt  (** Unsigned greater-than test *)
  | Uge  (** Unsigned greater-than-or-equal test *)
  | Ult  (** Unsigned less-than test *)
  | Ule  (** Unsigned less-than-or-equal test *)
  | Ord  (** Ordered test (neither arg is nan) *)
  | Uno  (** Unordered test (some arg is nan) *)
  | Add  (** Addition *)
  | Sub  (** Subtraction *)
  | Mul  (** Multiplication *)
  | Div  (** Division *)
  | Rem  (** Remainder of division *)
  | Udiv  (** Unsigned division *)
  | Urem  (** Remainder of unsigned division *)
  | And  (** Conjunction, boolean or bitwise *)
  | Or  (** Disjunction, boolean or bitwise *)
  | Xor  (** Exclusive-or, bitwise *)
  | Shl  (** Shift left, bitwise *)
  | Lshr  (** Logical shift right, bitwise *)
  | Ashr  (** Arithmetic shift right, bitwise *)
  | Update of int  (** Constant record with updated index *)
[@@deriving compare, equal, hash, sexp]

type op3 = Conditional  (** If-then-else *)
[@@deriving compare, equal, hash, sexp]

type opN =
  | Record  (** Record (array / struct) constant *)
  | Struct_rec
      (** Struct constant that may recursively refer to itself
          (transitively) from [elts]. NOTE: represented by cyclic values. *)
[@@deriving compare, equal, hash, sexp]

type t =
  | Reg of {name: string; typ: Typ.t; global: bool}  (** Virtual register *)
  | Nondet of {msg: string; typ: Typ.t}
      (** Anonymous register with arbitrary value, representing
          non-deterministic approximation of value described by [msg] *)
  | Label of {parent: string; name: string}
      (** Address of named code block within parent function *)
  | Integer of {data: Z.t; typ: Typ.t}  (** Integer constant *)
  | Float of {data: string; typ: Typ.t}  (** Floating-point constant *)
  | Ap1 of op1 * Typ.t * t
  | Ap2 of op2 * Typ.t * t * t
  | Ap3 of op3 * Typ.t * t * t * t
  | ApN of opN * Typ.t * t vector
[@@deriving compare, equal, hash, sexp]

include Comparator.S with type t := t

val pp : t pp

include Invariant.S with type t := t

type exp = t

(** Exp.Reg is re-exported as Reg *)
module Reg : sig
  type t = private exp [@@deriving compare, equal, hash, sexp]
  type reg = t

  include Comparator.S with type t := t

  module Set : sig
    type t = (reg, comparator_witness) Set.t
    [@@deriving compare, equal, sexp]

    val pp : t pp
    val empty : t
    val of_list : reg list -> t
    val of_vector : reg vector -> t
    val union_list : t list -> t
  end

  module Map : sig
    type 'a t = (reg, 'a, comparator_witness) Map.t
    [@@deriving compare, equal, sexp]

    val empty : 'a t
  end

  val pp : t pp
  val pp_demangled : t pp

  include Invariant.S with type t := t

  val of_exp : exp -> t option
  val program : ?global:unit -> Typ.t -> string -> t
  val name : t -> string
  val global : t -> bool
end

(** Construct *)

val reg : Reg.t -> t
val nondet : Typ.t -> string -> t
val label : parent:string -> name:string -> t
val null : t
val bool : bool -> t
val integer : Typ.t -> Z.t -> t
val float : Typ.t -> string -> t
val eq : Typ.t -> t -> t -> t
val dq : Typ.t -> t -> t -> t
val gt : Typ.t -> t -> t -> t
val ge : Typ.t -> t -> t -> t
val lt : Typ.t -> t -> t -> t
val le : Typ.t -> t -> t -> t
val ugt : Typ.t -> t -> t -> t
val uge : Typ.t -> t -> t -> t
val ult : Typ.t -> t -> t -> t
val ule : Typ.t -> t -> t -> t
val ord : Typ.t -> t -> t -> t
val uno : Typ.t -> t -> t -> t
val add : Typ.t -> t -> t -> t
val sub : Typ.t -> t -> t -> t
val mul : Typ.t -> t -> t -> t
val div : Typ.t -> t -> t -> t
val rem : Typ.t -> t -> t -> t
val udiv : Typ.t -> t -> t -> t
val urem : Typ.t -> t -> t -> t
val and_ : Typ.t -> t -> t -> t
val or_ : Typ.t -> t -> t -> t
val xor : Typ.t -> t -> t -> t
val shl : Typ.t -> t -> t -> t
val lshr : Typ.t -> t -> t -> t
val ashr : Typ.t -> t -> t -> t
val conditional : Typ.t -> cnd:t -> thn:t -> els:t -> t
val record : Typ.t -> t vector -> t
val select : Typ.t -> t -> int -> t
val update : Typ.t -> rcd:t -> int -> elt:t -> t

val struct_rec :
     (module Hashtbl.Key with type t = 'id)
  -> (id:'id -> Typ.t -> t lazy_t vector -> t) Staged.t
(** [struct_rec Id id element_thunks] constructs a possibly-cyclic [Struct]
    value. Cycles are detected using [Id]. The caller of [struct_rec Id]
    must ensure that a single unstaging of [struct_rec Id] is used for each
    complete cyclic value. Also, the caller must ensure that recursive calls
    to [struct_rec Id] provide [id] values that uniquely identify at least
    one point on each cycle. Failure to obey these requirements will lead to
    stack overflow. *)

val convert : dst:Typ.t -> ?signed:bool -> src:Typ.t -> t -> t

(** Access *)

val fold_regs : t -> init:'a -> f:('a -> Reg.t -> 'a) -> 'a

(** Query *)

val is_true : t -> bool
val is_false : t -> bool
val typ : t -> Typ.t
