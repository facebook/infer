(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for Theorem Proving *)

open Sil

val atom_negate : Tenv.t -> Sil.atom -> Sil.atom
(** Negate an atom *)

(** {2 Ordinary Theorem Proving} *)

val check_zero : Tenv.t -> Exp.t -> bool
(** Check [ |- e=0].  Result [false] means "don't know". *)

val check_equal : Tenv.t -> Prop.normal Prop.t -> Exp.t -> Exp.t -> bool
(** Check [prop |- exp1=exp2].  Result [false] means "don't know". *)

val check_disequal : Tenv.t -> Prop.normal Prop.t -> Exp.t -> Exp.t -> bool
(** Check whether [prop |- exp1!=exp2].  Result [false] means "don't know". *)

val type_size_comparable : Typ.t -> Typ.t -> bool
(** Return true if the two types have sizes which can be compared *)

val check_type_size_leq : Typ.t -> Typ.t -> bool
(** Check <= on the size of comparable types *)

val check_atom : Tenv.t -> Prop.normal Prop.t -> atom -> bool
(** Check whether [prop |- a].  Result [false] means "don't know". *)

val check_inconsistency_base : Tenv.t -> Prop.normal Prop.t -> bool
(** Inconsistency checking ignoring footprint. *)

val check_inconsistency : Tenv.t -> Prop.normal Prop.t -> bool
(** Inconsistency checking. *)

val check_allocatedness : Tenv.t -> Prop.normal Prop.t -> Exp.t -> bool
(** Check whether [prop |- allocated(exp)]. *)

val is_root : Tenv.t -> Prop.normal Prop.t -> Exp.t -> Exp.t -> offset list option
(** [is_root prop base_exp exp] checks whether [base_exp =
    exp.offlist] for some list of offsets [offlist]. If so, it returns
    [Some(offlist)]. Otherwise, it returns [None]. Assumes that
    [base_exp] points to the beginning of a structure, not the middle. *)

val expand_hpred_pointer : Tenv.t -> bool -> Sil.hpred -> bool * bool * Sil.hpred
(** [expand_hpred_pointer calc_index_frame hpred] expands [hpred] if it is a |-> whose lhs is a Lfield or Lindex or ptr+off.
    Return [(changed, calc_index_frame', hpred')] where [changed] indicates whether the predicate has changed. *)

val get_bounds : Tenv.t -> Prop.normal Prop.t -> Exp.t -> IntLit.t option * IntLit.t option
(** Get upper and lower bounds of an expression, if any *)

(** {2 Abduction prover} *)

val check_implication :
  Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.exposed Prop.t -> bool
(** [check_implication p1 p2] returns true if [p1|-p2] *)

type check = Bounds_check | Class_cast_check of Exp.t * Exp.t * Exp.t

val d_typings : (Exp.t * Exp.t) list -> unit

type implication_result =
  | ImplOK of
      ( check list
      * Sil.subst
      * Sil.subst
      * Sil.hpred list
      * Sil.atom list
      * Sil.hpred list
      * Sil.hpred list
      * Sil.hpred list
      * (Exp.t * Exp.t) list
      * (Exp.t * Exp.t) list )
  | ImplFail of check list

val check_implication_for_footprint :
  Typ.Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.exposed Prop.t -> implication_result
(** [check_implication_for_footprint p1 p2] returns
    [Some(sub, frame, missing)] if [sub(p1 * missing) |- sub(p2 *
    frame)] where [sub] is a substitution which instantiates the
    primed vars of [p1] and [p2], which are assumed to be disjoint. *)

(** {2 Cover: minimum set of pi's whose disjunction is equivalent to true} *)

val find_minimum_pure_cover :
  Tenv.t -> (Sil.atom list * 'a) list -> (Sil.atom list * 'a) list option
(** Find minimum set of pi's  in [cases] whose disjunction covers true *)

(** {2 Subtype checking} *)

module Subtyping_check : sig
  val check_subtype : Tenv.t -> Typ.t -> Typ.t -> bool
  (** check_subtype t1 t2 checks whether t1 is a subtype of t2, given the type environment tenv. *)

  val subtype_case_analysis : Tenv.t -> Exp.t -> Exp.t -> Exp.t option * Exp.t option
  (** subtype_case_analysis tenv tecp1 texp2 performs case analysis on [texp1 <: texp2],
      and returns the updated types in the true and false case, if they are possible *)
end
