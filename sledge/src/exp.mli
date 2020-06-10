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
  | Signed of {bits: int}
      (** [Ap1 (Signed {bits= n}, dst, arg)] is [arg] interpreted as an
          [n]-bit signed integer and injected into the [dst] type. That is,
          it two's-complement--decodes the low [n] bits of the infinite
          two's-complement encoding of [arg]. The injection into [dst] is a
          no-op, so [dst] must be an integer type with bitwidth at least
          [n]. *)
  | Unsigned of {bits: int}
      (** [Ap1 (Unsigned {bits= n}, dst, arg)] is [arg] interpreted as an
          [n]-bit unsigned integer and injected into the [dst] type. That
          is, it unsigned-binary--decodes the low [n] bits of the infinite
          two's-complement encoding of [arg]. The injection into [dst] is a
          no-op, so [dst] must be an integer type with bitwidth greater than
          [n]. *)
  | Convert of {src: Typ.t}
      (** [Ap1 (Convert {src}, dst, arg)] is [arg] converted from type [src]
          to type [dst], possibly with loss of information. The [src] and
          [dst] types must be [Typ.convertible] and must not both be
          [Integer] types. *)
  | Splat  (** Iterated concatenation of a single byte *)
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
  | Div  (** Division, for integers result is truncated toward zero *)
  | Rem
      (** Remainder of division, satisfies [a = b * div a b + rem a b] and
          for integers [rem a b] has same sign as [a], and [|rem a b| < |b|] *)
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

type opN = Record  (** Record (array / struct) constant *)
[@@deriving compare, equal, hash, sexp]

type t = private
  | Reg of {name: string; global: bool; typ: Typ.t}  (** Virtual register *)
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
  | ApN of opN * Typ.t * t iarray
  | RecRecord of int * Typ.t  (** Reference to ancestor recursive record *)
[@@deriving compare, equal, hash, sexp]

val pp : t pp

include Invariant.S with type t := t

(** Exp.Reg is re-exported as Reg *)
module Reg : sig
  type exp := t
  type t = private exp [@@deriving compare, equal, hash, sexp]

  module Set : sig
    include Set.S with type elt := t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val pp : t pp
    val vars : t -> Var.Set.t
  end

  module Map : Map.S with type key := t

  val demangle : (string -> string option) ref
  val pp : t pp
  val pp_demangled : t pp

  include Invariant.S with type t := t

  val of_exp : exp -> t option
  val program : ?global:unit -> Typ.t -> string -> t
  val var : t -> Var.t
  val name : t -> string
  val typ : t -> Typ.t
end

(** Construct *)

(* registers *)
val reg : Reg.t -> t

(* constants *)
val nondet : Typ.t -> string -> t
val label : parent:string -> name:string -> t
val null : t
val bool : bool -> t
val true_ : t
val false_ : t
val integer : Typ.t -> Z.t -> t
val float : Typ.t -> string -> t

(* type conversions *)
val signed : int -> t -> to_:Typ.t -> t
val unsigned : int -> t -> to_:Typ.t -> t
val convert : Typ.t -> to_:Typ.t -> t -> t

(* comparisons *)
val eq : ?typ:Typ.t -> t -> t -> t
val dq : ?typ:Typ.t -> t -> t -> t
val gt : ?typ:Typ.t -> t -> t -> t
val ge : ?typ:Typ.t -> t -> t -> t
val lt : ?typ:Typ.t -> t -> t -> t
val le : ?typ:Typ.t -> t -> t -> t
val ugt : ?typ:Typ.t -> t -> t -> t
val uge : ?typ:Typ.t -> t -> t -> t
val ult : ?typ:Typ.t -> t -> t -> t
val ule : ?typ:Typ.t -> t -> t -> t
val ord : ?typ:Typ.t -> t -> t -> t
val uno : ?typ:Typ.t -> t -> t -> t

(* arithmetic *)
val add : ?typ:Typ.t -> t -> t -> t
val sub : ?typ:Typ.t -> t -> t -> t
val mul : ?typ:Typ.t -> t -> t -> t
val div : ?typ:Typ.t -> t -> t -> t
val rem : ?typ:Typ.t -> t -> t -> t
val udiv : ?typ:Typ.t -> t -> t -> t
val urem : ?typ:Typ.t -> t -> t -> t

(* boolean / bitwise *)
val and_ : ?typ:Typ.t -> t -> t -> t
val or_ : ?typ:Typ.t -> t -> t -> t

(* bitwise *)
val xor : ?typ:Typ.t -> t -> t -> t
val shl : ?typ:Typ.t -> t -> t -> t
val lshr : ?typ:Typ.t -> t -> t -> t
val ashr : ?typ:Typ.t -> t -> t -> t

(* if-then-else *)
val conditional : ?typ:Typ.t -> cnd:t -> thn:t -> els:t -> t

(* memory *)
val splat : Typ.t -> t -> t

(* records (struct / array values) *)
val record : Typ.t -> t iarray -> t
val select : Typ.t -> t -> int -> t
val update : Typ.t -> rcd:t -> int -> elt:t -> t
val rec_record : int -> Typ.t -> t

(** Traverse *)

val fold_regs : t -> init:'a -> f:('a -> Reg.t -> 'a) -> 'a

(** Query *)

val term : t -> Term.t
val is_true : t -> bool
val is_false : t -> bool
