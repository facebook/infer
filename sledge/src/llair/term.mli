(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms

    Pure (heap-independent) terms are complex arithmetic, bitwise-logical,
    etc. operations over literal values and variables. *)

type comparator_witness

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
  | Select of int  (** Select an index from a record *)
[@@deriving compare, equal, hash, sexp]

type op2 =
  | Eq  (** Equal test *)
  | Dq  (** Disequal test *)
  | Lt  (** Less-than test *)
  | Le  (** Less-than-or-equal test *)
  | Ord  (** Ordered test (neither arg is nan) *)
  | Uno  (** Unordered test (some arg is nan) *)
  | Div  (** Division *)
  | Rem  (** Remainder of division *)
  | And  (** Conjunction, boolean or bitwise *)
  | Or  (** Disjunction, boolean or bitwise *)
  | Xor  (** Exclusive-or, bitwise *)
  | Shl  (** Shift left, bitwise *)
  | Lshr  (** Logical shift right, bitwise *)
  | Ashr  (** Arithmetic shift right, bitwise *)
  | Splat  (** Iterated concatenation of a single byte *)
  | Memory  (** Size-tagged byte-array *)
  | Update of int  (** Constant record with updated index *)
[@@deriving compare, equal, hash, sexp]

type op3 = Conditional  (** If-then-else *)
[@@deriving compare, equal, hash, sexp]

type opN =
  | Concat  (** Byte-array concatenation *)
  | Record  (** Record (array / struct) constant *)
[@@deriving compare, equal, hash, sexp]

type recN = Record  (** Recursive record (array / struct) constant *)
[@@deriving compare, equal, hash, sexp]

type qset = (t, comparator_witness) Qset.t

and t = private
  | Add of qset  (** Sum of terms with rational coefficients *)
  | Mul of qset  (** Product of terms with rational exponents *)
  | Var of {id: int; name: string}  (** Local variable / virtual register *)
  | Ap1 of op1 * t  (** Unary application *)
  | Ap2 of op2 * t * t  (** Binary application *)
  | Ap3 of op3 * t * t * t  (** Ternary application *)
  | ApN of opN * t vector  (** N-ary application *)
  | RecN of recN * t vector
      (** Recursive n-ary application, may recursively refer to itself
          (transitively) from its args. NOTE: represented by cyclic values. *)
  | Label of {parent: string; name: string}
      (** Address of named code block within parent function *)
  | Nondet of {msg: string}
      (** Anonymous local variable with arbitrary value, representing
          non-deterministic approximation of value described by [msg] *)
  | Float of {data: string}  (** Floating-point constant *)
  | Integer of {data: Z.t}  (** Integer constant *)
[@@deriving compare, equal, hash, sexp]

val comparator : (t, comparator_witness) Comparator.t
val pp_full : ?is_x:(t -> bool) -> t pp
val pp : t pp
val invariant : t -> unit

type term = t

(** Term.Var is re-exported as Var *)
module Var : sig
  type t = private term [@@deriving compare, equal, hash, sexp]
  type var = t

  include Comparator.S with type t := t

  module Set : sig
    type t = (var, comparator_witness) Set.t
    [@@deriving compare, equal, sexp]

    val pp_full : ?is_x:(term -> bool) -> t pp
    val pp : t pp
    val empty : t
    val of_ : var -> t
    val of_option : var option -> t
    val of_list : var list -> t
    val of_vector : var vector -> t
  end

  val pp : t pp

  include Invariant.S with type t := t

  val of_term : term -> t option
  val program : ?global:unit -> string -> t
  val fresh : string -> wrt:Set.t -> t * Set.t
  val name : t -> string
  val global : t -> bool

  module Subst : sig
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

(* memory contents *)
val splat : byt:t -> siz:t -> t
val memory : siz:t -> arr:t -> t
val concat : t array -> t

(* records (struct / array values) *)
val record : t vector -> t
val select : rcd:t -> idx:int -> t
val update : rcd:t -> idx:int -> elt:t -> t

(* recursive n-ary application *)
val rec_app :
     (module Hashtbl.Key with type t = 'id)
  -> (id:'id -> recN -> t lazy_t vector -> t) Staged.t

val size_of : Typ.t -> t

(** Transform *)

val map : t -> f:(t -> t) -> t
val rename : Var.Subst.t -> t -> t

(** Traverse *)

val iter : t -> f:(t -> unit) -> unit
val fold : t -> init:'a -> f:(t -> 'a -> 'a) -> 'a
val fold_vars : t -> init:'a -> f:('a -> Var.t -> 'a) -> 'a
val fold_terms : t -> init:'a -> f:('a -> t -> 'a) -> 'a

(** Query *)

val fv : t -> Var.Set.t
val is_true : t -> bool
val is_false : t -> bool
val classify : t -> [> `Atomic | `Interpreted | `Simplified | `Uninterpreted]
val solve : t -> t -> (t, t, comparator_witness) Map.t option
