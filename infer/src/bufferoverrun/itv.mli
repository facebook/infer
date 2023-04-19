(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module Bound = Bounds.Bound
module SymbolPath = Symb.SymbolPath
module SymbolSet = Symb.SymbolSet

module ItvRange : sig
  type t

  val to_top_lifted_polynomial : t -> Polynomials.NonNegativePolynomial.t
end

module ItvPure : sig
  type t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val pp_mark : markup:bool -> F.formatter -> t -> unit

  val mone : t

  val zero : t

  val of_int : int -> t

  val lb : t -> Bound.t

  val ub : t -> Bound.t

  val is_finite : t -> bool

  val is_invalid : t -> bool

  val is_lb_infty : t -> bool

  val is_nat : t -> bool

  val is_symbolic : t -> bool

  val is_top : t -> bool

  val is_zero : t -> bool

  val is_one : t -> bool

  val is_ge_zero : t -> bool

  val is_le_zero : t -> bool

  val is_le_mone : t -> bool

  val leq : lhs:t -> rhs:t -> bool

  val have_similar_bounds : t -> t -> bool

  val has_infty : t -> bool

  val has_void_ptr_symb : t -> bool

  val make_non_negative : t -> t

  val join : t -> t -> t

  val le_sem : t -> t -> Boolean.t

  val lt_sem : t -> t -> Boolean.t

  val widen : prev:t -> next:t -> num_iters:int -> t

  val xcompare :
       lhs:t
    -> rhs:t
    -> [ `Equal
       | `LeftSmallerThanRight
       | `RightSmallerThanLeft
       | `NotComparable
       | `LeftSubsumesRight
       | `RightSubsumesLeft ]

  val get_symbols : t -> SymbolSet.t

  val subst : t -> Bound.eval_sym -> t bottom_lifted

  val neg : t -> t

  val plus : t -> t -> t

  val minus : t -> t -> t

  val succ : t -> t

  val mult : t -> t -> t

  val exists_str : f:(string -> bool) -> t -> bool
end

include module type of AbstractDomain.BottomLifted (ItvPure)

val widen_thresholds : thresholds:Z.t list -> prev:t -> next:t -> num_iters:int -> t

val compare : t -> t -> int

val bot : t
(** _|_ *)

val zero_255 : t
(** [0, 255] *)

val m1_255 : t
(** [-1, 255] *)

val nat : t
(** [0, +oo] *)

val pos : t
(** [1, +oo] *)

val top : t
(** [-oo, +oo] *)

val zero : t
(** 0 *)

val one : t
(** 1 *)

val zero_one : t
(** [0, 1] *)

val unknown_bool : t
(** [0, 1] *)

val get_range_of_iterator : t -> t

val of_bool : Boolean.t -> t

val of_int : int -> t

val of_big_int : Z.t -> t

val of_int_lit : IntLit.t -> t

val get_const : t -> Z.t option

val is_zero : t -> bool

val is_one : t -> bool

val is_mone : t -> bool

val get_bound : t -> Symb.BoundEnd.t -> Bound.t bottom_lifted

val is_false : t -> bool

val decr : t -> t

val decr_length : t -> t

val incr : t -> t

val set_lb : Bound.t -> t -> t

val set_lb_zero : t -> t

val neg : t -> t

val normalize : t -> t

val is_symbolic : t -> bool

val is_top : t -> bool

val get_symbols : t -> SymbolSet.t

val eq : t -> t -> bool

val le : lhs:t -> rhs:t -> bool

val lnot : t -> Boolean.t

val range : Location.t -> t -> ItvRange.t

val div : t -> t -> t

val div_const : t -> Z.t -> t

val minus : t -> t -> t

val mult : t -> t -> t

val mult_const : t -> Z.t -> t

val plus : t -> t -> t

val shiftlt : t -> t -> t

val shiftrt : t -> t -> t

val band_sem : t -> t -> t

val eq_sem : t -> t -> Boolean.t

val ge_sem : t -> t -> Boolean.t

val gt_sem : t -> t -> Boolean.t

val land_sem : t -> t -> Boolean.t

val le_sem : t -> t -> Boolean.t

val lor_sem : t -> t -> Boolean.t

val lt_sem : t -> t -> Boolean.t

val min_sem : ?use_minmax_bound:bool -> t -> t -> t

val max_sem : ?use_minmax_bound:bool -> t -> t -> t

val mod_sem : t -> t -> t

val ne_sem : t -> t -> Boolean.t

val prune_eq_zero : t -> t

val prune_ne_zero : t -> t

val prune_ge_one : t -> t

val prune_binop : Binop.t -> t -> t -> t

val prune_eq : t -> t -> t

val prune_ne : t -> t -> t

val prune_lt : t -> t -> t

val prune_le : t -> t -> t

val subst : t -> Bound.eval_sym -> t

val max_of_ikind : IntegerWidths.t -> Typ.ikind -> t

val of_normal_path : unsigned:bool -> ?non_int:bool -> Symb.SymbolPath.partial -> t

val of_offset_path : is_void:bool -> Symb.SymbolPath.partial -> t

val of_length_path : is_void:bool -> Symb.SymbolPath.partial -> t

val of_modeled_path : Symb.SymbolPath.partial -> t

val is_offset_path_of : Symb.SymbolPath.partial -> t -> bool

val is_length_path_of : Symb.SymbolPath.partial -> t -> bool

val has_only_non_int_symbols : t -> bool

val is_incr_of : Symb.SymbolPath.partial -> t -> bool
(** Check if [itv] is [path+1] when called [is_incr_of path itv] *)
