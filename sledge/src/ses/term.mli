(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms

    Pure (heap-independent) terms are arithmetic, bitwise-logical, etc.
    operations over literal values and variables. *)

type op1 =
  | Signed of {bits: int}
      (** [Ap1 (Signed {bits= n}, arg)] is [arg] interpreted as an [n]-bit
          signed integer. That is, it two's-complement--decodes the low [n]
          bits of the infinite two's-complement encoding of [arg]. *)
  | Unsigned of {bits: int}
      (** [Ap1 (Unsigned {bits= n}, arg)] is [arg] interpreted as an [n]-bit
          unsigned integer. That is, it unsigned-binary--decodes the low [n]
          bits of the infinite two's-complement encoding of [arg]. *)
  | Splat  (** Iterated concatenation of a single byte *)
  | Select of int  (** Select an index from a record *)
[@@deriving compare, equal, sexp]

type op2 =
  | Eq  (** Equal test *)
  | Dq  (** Disequal test *)
  | Lt  (** Less-than test *)
  | Le  (** Less-than-or-equal test *)
  | Div  (** Division, for integers result is truncated toward zero *)
  | Rem
      (** Remainder of division, satisfies [a = b * div a b + rem a b] and
          for integers [rem a b] has same sign as [a], and [|rem a b| < |b|] *)
  | Xor  (** Exclusive-or, bitwise *)
  | Shl  (** Shift left, bitwise *)
  | Lshr  (** Logical shift right, bitwise *)
  | Ashr  (** Arithmetic shift right, bitwise *)
  | Sized  (** Size-tagged sequence *)
  | Update of int  (** Constant record with updated index *)
[@@deriving compare, equal, sexp]

type op3 =
  | Conditional  (** If-then-else *)
  | Extract  (** Extract a slice of an sequence value *)
[@@deriving compare, equal, sexp]

type opN =
  | Concat  (** Byte-array concatenation *)
  | Record  (** Record (array / struct) constant *)
[@@deriving compare, equal, sexp]

module rec Set : sig
  include NS.Set.S with type elt := T.t

  val t_of_sexp : Sexp.t -> t
end

and Qset : sig
  include NS.Qset.S with type elt := T.t

  val t_of_sexp : Sexp.t -> t
end

and T : sig
  type set = Set.t [@@deriving compare, equal, sexp]

  type qset = Qset.t [@@deriving compare, equal, sexp]

  and t = private
    | Var of {id: int; name: string}
        (** Local variable / virtual register *)
    | Ap1 of op1 * t  (** Unary application *)
    | Ap2 of op2 * t * t  (** Binary application *)
    | Ap3 of op3 * t * t * t  (** Ternary application *)
    | ApN of opN * t iarray  (** N-ary application *)
    | And of set  (** Conjunction, boolean or bitwise *)
    | Or of set  (** Disjunction, boolean or bitwise *)
    | Add of qset  (** Sum of terms with rational coefficients *)
    | Mul of qset  (** Product of terms with rational exponents *)
    | Integer of {data: Z.t}  (** Integer constant *)
    | Rational of {data: Q.t}  (** Rational constant *)
    | RecRecord of int  (** Reference to ancestor recursive record *)
    | Apply of Funsym.t * t iarray
        (** Uninterpreted function application *)
    | PosLit of Predsym.t * t iarray
    | NegLit of Predsym.t * t iarray
  [@@deriving compare, equal, sexp]
end

include module type of T with type t = T.t

(** Term.Var is re-exported as Var *)
module Var : sig
  include Var_intf.VAR with type t = private T.t

  val of_ : T.t -> t
  (** [var (of_ e)] = [e] if [e] matches [Var _], otherwise undefined *)

  val of_term : T.t -> t option
end

module Map : sig
  include Map.S with type key := t

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end

val ppx : Var.strength -> t pp
val pp : t pp
val pp_diff : (t * t) pp
val invariant : t -> unit

(** Construct *)

(* variables *)
val var : Var.t -> t

(* constants *)
val bool : bool -> t
val true_ : t
val false_ : t
val integer : Z.t -> t
val zero : t
val one : t
val minus_one : t
val rational : Q.t -> t

(* type conversions *)
val signed : int -> t -> t
val unsigned : int -> t -> t

(* comparisons *)
val eq : t -> t -> t
val dq : t -> t -> t
val lt : t -> t -> t
val le : t -> t -> t

(* arithmetic *)
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mulq : Q.t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val rem : t -> t -> t

(* boolean / bitwise *)
val and_ : t -> t -> t
val or_ : t -> t -> t
val not_ : t -> t

(* bitwise *)
val xor : t -> t -> t
val shl : t -> t -> t
val lshr : t -> t -> t
val ashr : t -> t -> t

(* if-then-else *)
val conditional : cnd:t -> thn:t -> els:t -> t

(* sequence sizes *)
val seq_size_exn : t -> t
val seq_size : t -> t option

(* sequences *)
val splat : t -> t
val sized : seq:t -> siz:t -> t
val extract : seq:t -> off:t -> len:t -> t
val concat : t array -> t

(* records (struct / array values) *)
val record : t iarray -> t
val select : rcd:t -> idx:int -> t
val update : rcd:t -> idx:int -> elt:t -> t
val rec_record : int -> t

(* uninterpreted *)
val apply : Funsym.t -> t iarray -> t
val poslit : Predsym.t -> t iarray -> t
val neglit : Predsym.t -> t iarray -> t

(** Destruct *)

val d_int : t -> Z.t option

(** Access *)

val const_of : t -> Q.t option

(** Transform *)

val map : t -> f:(t -> t) -> t

val map_rec_pre : t -> f:(t -> t option) -> t
(** Pre-order transformation. Each subterm [x] from root to leaves is
    presented to [f]. If [f x = Some x'] then the subterms of [x] are not
    traversed and [x] is transformed to [x']. Otherwise traversal proceeds
    to the subterms of [x], followed by rebuilding the term structure on the
    transformed subterms. *)

val fold_map : t -> init:'a -> f:('a -> t -> 'a * t) -> 'a * t

val fold_map_rec_pre :
  t -> init:'a -> f:('a -> t -> ('a * t) option) -> 'a * t

val disjuncts : t -> t list
val rename : (Var.t -> Var.t) -> t -> t

(** Traverse *)

val iter : t -> f:(t -> unit) -> unit
val exists : t -> f:(t -> bool) -> bool
val fold : t -> init:'a -> f:(t -> 'a -> 'a) -> 'a
val fold_vars : t -> init:'a -> f:('a -> Var.t -> 'a) -> 'a
val fold_terms : t -> init:'a -> f:('a -> t -> 'a) -> 'a

(** Query *)

val fv : t -> Var.Set.t
val is_true : t -> bool
val is_false : t -> bool

val is_constant : t -> bool
(** Test if a term's semantics is independent of the values of variables. *)

val height : t -> int

(** Solve *)

val solve_zero_eq : ?for_:t -> t -> (t * t) option
(** [solve_zero_eq d] is [Some (e, f)] if [d = 0] can be equivalently
    expressed as [e = f] for some monomial subterm [e] of [d]. If [for_] is
    passed, then the subterm [e] must be [for_]. *)
