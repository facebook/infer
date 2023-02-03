(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module ArrInfo : sig
  type t

  val byte_size : t -> Itv.t
  (** Return size of array block as bytes *)

  val get_offset : t -> Itv.t
  (** Return current offset of array block *)

  val get_size : t -> Itv.t
  (** Return size of array block, i.e., number of cells *)
end

include AbstractDomain.MapS with type key = AbsLoc.Allocsite.t and type value = ArrInfo.t

val compare : t -> t -> int

val bot : t

val make_c : AbsLoc.Allocsite.t -> offset:Itv.t -> size:Itv.t -> stride:Itv.t -> t
(** Make an array block for C *)

val make_java : AbsLoc.Allocsite.t -> length:Itv.t -> t
(** Make an array block for Java *)

val unknown : t

val get_pow_loc : t -> AbsLoc.PowLoc.t
(** Return all allocsites as [PowLoc.t] *)

val is_bot : t -> bool

val is_unknown : t -> bool

val is_symbolic : t -> bool
(** Check if there is a symbolic integer value in its offset or size *)

val lift_cmp_itv : (Itv.t -> Itv.t -> Boolean.t) -> Boolean.EqualOrder.t -> t -> t -> Boolean.t
(** Lift a comparison of [Itv.t] and [Loc.t] to that of [t]. The comparison for [Itv.t] is used for
    integer values such as offset and size, and the comparison for [Loc.t] is used for allocsites. *)

val transform_length : f:(Itv.t -> Itv.t) -> t -> t
(** Apply [f] to all sizes *)

val prune_binop : Binop.t -> t -> t -> t
(** [prune_binop bop x y] returns a pruned value of [x] by [bop] and [y]. *)

val prune_eq : t -> t -> t
(** [prune_eq x y] returns a pruned value of [x] by [== y]. *)

val prune_ne : t -> t -> t
(** [prune_ne x y] returns a pruned value of [x] by [!= y]. *)

val prune_offset_le_size : t -> t
(** Prune offset by [offset <= size] *)

val minus_offset : t -> Itv.t -> t

val plus_offset : t -> Itv.t -> t

val diff : t -> t -> Itv.t
(** Return difference of offsets between given array blocks *)

val normalize : t -> t
(** Normalize all interval values such as offset and size in it. Thus, if an interval value is
    invalid, the interval value is replaced with bottom. *)

val subst : t -> Bounds.Bound.eval_sym -> AbsLoc.PowLoc.eval_locpath -> AbsLoc.PowLoc.t * t
(** Substitute symbolic abstract locations and symbolic interval value in the array block.
    [eval_sym] is to get substituted interval values and [eval_locpath] is to get substituted
    abstract locaion values. It also returns a set of abstract locations containing non-allocsite
    locations from the substitution results. Since the key of [ArrayBlk.t] is [AbsLoc.Allocsite.t],
    they cannot be written in this domain. *)

val set_length : Itv.t -> t -> t

val set_offset : Itv.t -> t -> t

val set_stride : Z.t -> t -> t

val get_symbols : t -> Symb.SymbolSet.t
(** Return all symbols for integer values in it *)

val get_offset : ?cost_mode:bool -> t -> Itv.t
(** Return offset of the array block. If [cost_mode] is [true], it returns a conservative (bigger
    than correct one), but not correct offset results. *)

val get_size : ?cost_mode:bool -> t -> Itv.t
(** Return size of the array block. If [cost_mode] is [true], it returns a conservative (bigger than
    correct one), but not correct size results. *)
