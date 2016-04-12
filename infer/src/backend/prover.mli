(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Functions for Theorem Proving *)

open Sil

(** {2 Ordinary Theorem Proving} *)

(** Check [ |- e=0].  Result [false] means "don't know". *)
val check_zero : exp -> bool

(** Check [prop |- exp1=exp2].  Result [false] means "don't know". *)
val check_equal : Prop.normal Prop.t -> exp -> exp -> bool

(** Check whether [prop |- exp1!=exp2].  Result [false] means "don't know". *)
val check_disequal : Prop.normal Prop.t -> exp -> exp -> bool

val check_le : Prop.normal Prop.t -> exp -> exp -> bool

(** Return true if the two types have sizes which can be compared *)
val type_size_comparable : Sil.typ -> Sil.typ -> bool

(** Check <= on the size of comparable types *)
val check_type_size_leq : Sil.typ -> Sil.typ -> bool

(** Check < on the size of comparable types *)
val check_type_size_lt : Sil.typ -> Sil.typ -> bool

(** Check whether [prop |- a].  Result [false] means "don't know". *)
val check_atom : Prop.normal Prop.t -> atom -> bool

(** Inconsistency checking ignoring footprint. *)
val check_inconsistency_base : Prop.normal Prop.t -> bool

(** Inconsistency checking. *)
val check_inconsistency : Prop.normal Prop.t -> bool

(** Check whether [prop |- allocated(exp)]. *)
val check_allocatedness : Prop.normal Prop.t -> exp -> bool

(** [is_root prop base_exp exp] checks whether [base_exp =
    exp.offlist] for some list of offsets [offlist]. If so, it returns
    [Some(offlist)]. Otherwise, it returns [None]. Assumes that
    [base_exp] points to the beginning of a structure, not the middle. *)
val is_root : Prop.normal Prop.t -> exp -> exp -> offset list option

(** [expand_hpred_pointer calc_index_frame hpred] expands [hpred] if it is a |-> whose lhs is a Lfield or Lindex or ptr+off.
    Return [(changed, calc_index_frame', hpred')] where [changed] indicates whether the predicate has changed. *)
val expand_hpred_pointer : bool -> Sil.hpred -> bool * bool * Sil.hpred

(** Get upper and lower bounds of an expression, if any *)
val get_bounds : Prop.normal Prop.t -> Sil.exp -> Sil.Int.t option * Sil.Int.t option

(** {2 Abduction prover} *)

(** [check_implication p1 p2] returns true if [p1|-p2] *)
val check_implication : Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.exposed Prop.t -> bool

type check =
  | Bounds_check
  | Class_cast_check of Sil.exp * Sil.exp * Sil.exp

val d_typings : (Sil.exp * Sil.exp) list -> unit

type implication_result =
  | ImplOK of (check list * Sil.subst * Sil.subst * Sil.hpred list * (Sil.atom list) * (Sil.hpred list) * (Sil.hpred list) * (Sil.hpred list) * ((Sil.exp * Sil.exp) list) * ((Sil.exp * Sil.exp) list))
  | ImplFail of check list

(** [check_implication_for_footprint p1 p2] returns
    [Some(sub, frame, missing)] if [sub(p1 * missing) |- sub(p2 *
    frame)] where [sub] is a substitution which instantiates the
    primed vars of [p1] and [p2], which are assumed to be disjoint. *)
val check_implication_for_footprint :
  Procname.t -> Tenv.t -> Prop.normal Prop.t -> Prop.exposed Prop.t -> implication_result

(** {2 Cover: miminum set of pi's whose disjunction is equivalent to true} *)

(** Find miminum set of pi's  in [cases] whose disjunction covers true *)
val find_minimum_pure_cover : (Sil.atom list * 'a) list -> (Sil.atom list * 'a) list option

(** {2 Compute various lower or upper bounds} *)

(** Computer an upper bound of an expression *)
val compute_upper_bound_of_exp : Prop.normal Prop.t -> Sil.exp -> Sil.Int.t option

(** {2 Subtype checking} *)

module Subtyping_check :
sig

  (** check_subtype t1 t2 checks whether t1 is a subtype of t2, given the type environment tenv. *)
  val check_subtype : Tenv.t -> Sil.typ -> Sil.typ -> bool

  (** subtype_case_analysis tenv tecp1 texp2 performs case analysis on [texp1 <: texp2],
      and returns the updated types in the true and false case, if they are possible *)
  val subtype_case_analysis : Tenv.t -> Sil.exp -> Sil.exp -> Sil.exp option * Sil.exp option

end

val get_overrides_of : Tenv.t -> Sil.typ -> Procname.t -> (typ * Procname.t) list



