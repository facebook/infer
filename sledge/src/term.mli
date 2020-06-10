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
  | Convert of {src: Typ.t; dst: Typ.t}
      (** [Ap1 (Convert {src; dst}, arg)] is [arg] converted from type [src]
          to type [dst], possibly with loss of information. The [src] and
          [dst] types must be [Typ.convertible] and must not both be
          [Integer] types. *)
  | Splat  (** Iterated concatenation of a single byte *)
  | Select of int  (** Select an index from a record *)
[@@deriving compare, equal, hash, sexp]

type op2 =
  | Eq  (** Equal test *)
  | Dq  (** Disequal test *)
  | Lt  (** Less-than test *)
  | Le  (** Less-than-or-equal test *)
  | Ord  (** Ordered test (neither arg is nan) *)
  | Uno  (** Unordered test (some arg is nan) *)
  | Div  (** Division, for integers result is truncated toward zero *)
  | Rem
      (** Remainder of division, satisfies [a = b * div a b + rem a b] and
          for integers [rem a b] has same sign as [a], and [|rem a b| < |b|] *)
  | Xor  (** Exclusive-or, bitwise *)
  | Shl  (** Shift left, bitwise *)
  | Lshr  (** Logical shift right, bitwise *)
  | Ashr  (** Arithmetic shift right, bitwise *)
  | Memory  (** Size-tagged byte-array *)
  | Update of int  (** Constant record with updated index *)
[@@deriving compare, equal, hash, sexp]

type op3 =
  | Conditional  (** If-then-else *)
  | Extract  (** Extract a slice of an aggregate value *)
[@@deriving compare, equal, hash, sexp]

type opN =
  | Concat  (** Byte-array concatenation *)
  | Record  (** Record (array / struct) constant *)
[@@deriving compare, equal, hash, sexp]

module rec Set : sig
  include NS.Set.S with type elt := T.t

  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
end

and Qset : sig
  include NS.Qset.S with type elt := T.t

  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
end

and T : sig
  type set = Set.t [@@deriving compare, equal, hash, sexp]

  type qset = Qset.t [@@deriving compare, equal, hash, sexp]

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
    | Label of {parent: string; name: string}
        (** Address of named code block within parent function *)
    | Nondet of {msg: string}
        (** Anonymous local variable with arbitrary value, representing
            non-deterministic approximation of value described by [msg] *)
    | Float of {data: string}  (** Floating-point constant *)
    | Integer of {data: Z.t}  (** Integer constant *)
    | Rational of {data: Q.t}  (** Rational constant *)
    | RecRecord of int  (** Reference to ancestor recursive record *)
  [@@deriving compare, equal, hash, sexp]
end

include module type of T with type t = T.t

(** Term.Var is re-exported as Var *)
module Var : sig
  type term := t
  type t = private term [@@deriving compare, equal, hash, sexp]
  type strength = t -> [`Universal | `Existential | `Anonymous] option

  module Map : Map.S with type key := t

  module Set : sig
    include NS.Set.S with type elt := t

    val hash_fold_t : t Hash.folder
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val ppx : strength -> t pp
    val pp : t pp
    val pp_xs : t pp
  end

  val pp : t pp

  include Invariant.S with type t := t

  val name : t -> string
  val id : t -> int
  val is_global : t -> bool
  val of_ : term -> t
  val of_term : term -> t option
  val program : ?global:unit -> string -> t
  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Variable with the given [id]. Variables are compared by [id] alone,
      [name] is used only for printing. The only way to ensure [identified]
      variables do not clash with [fresh] variables is to pass the
      [identified] variables to [fresh] in [wrt]:
      [Var.fresh name ~wrt:(Var.Set.of_ (Var.identified ~name ~id))]. *)

  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> t
    val invert : t -> t
    val restrict : t -> Set.t -> t
    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val apply_set : t -> Set.t -> Set.t
    val fold : t -> init:'a -> f:(var -> var -> 'a -> 'a) -> 'a
  end
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
val nondet : string -> t
val label : parent:string -> name:string -> t
val null : t
val bool : bool -> t
val true_ : t
val false_ : t
val integer : Z.t -> t
val zero : t
val one : t
val minus_one : t
val rational : Q.t -> t
val float : string -> t

(* type conversions *)
val signed : int -> t -> t
val unsigned : int -> t -> t
val convert : Typ.t -> to_:Typ.t -> t -> t

(* comparisons *)
val eq : t -> t -> t
val dq : t -> t -> t
val lt : t -> t -> t
val le : t -> t -> t
val ord : t -> t -> t
val uno : t -> t -> t

(* arithmetic *)
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
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

(* aggregate sizes *)
val agg_size_exn : t -> t
val agg_size : t -> t option

(* aggregates (memory contents) *)
val splat : t -> t
val memory : siz:t -> arr:t -> t
val extract : agg:t -> off:t -> len:t -> t
val concat : t array -> t
val eq_concat : t * t -> (t * t) array -> t

(* records (struct / array values) *)
val record : t iarray -> t
val select : rcd:t -> idx:int -> t
val update : rcd:t -> idx:int -> elt:t -> t
val rec_record : int -> t

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

val rename : Var.Subst.t -> t -> t

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
