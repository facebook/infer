(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Implementation of "Smart" Pattern Matching for higher order singly-linked list predicate.

    Used for detecting on a given program if some data scructures are matching some predefined
    higher-order list predicates. When it is the case, these predicates can be used as possible
    candidates for abstracting the data-structures. See
    {{:http://dx.doi.org/10.1007/978-3-540-73368-3_22} CAV 2007} for the therory involved. *)

(* TODO: missing documentation *)

val hpara_match_with_impl : Tenv.t -> bool -> Predicates.hpara -> Predicates.hpara -> bool

val hpara_dll_match_with_impl :
  Tenv.t -> bool -> Predicates.hpara_dll -> Predicates.hpara_dll -> bool

(** Type for a hpred pattern. [flag=false] means that the implication between hpreds is not
    considered, and [flag = true] means that it is considered during pattern matching. *)
type hpred_pat = {hpred: Predicates.hpred; flag: bool}

type sidecondition = Prop.normal Prop.t -> Predicates.subst -> bool

val prop_match_with_impl :
     Tenv.t
  -> Prop.normal Prop.t
  -> sidecondition
  -> Ident.t list
  -> hpred_pat
  -> hpred_pat list
  -> (Predicates.subst * Prop.normal Prop.t) option
(** [prop_match_with_impl p condition vars hpat hpats] returns [(subst, p_leftover)] such that

    + [dom(subst) = vars]
    + [p |- (hpat.hpred * hpats.hpred)[subst] * p_leftover].

    Using the flag [field], we can control the strength of |-. *)

val find_partial_iso :
     Tenv.t
  -> (Exp.t -> Exp.t -> bool)
  -> (Exp.t * Exp.t) list
  -> (Exp.t * Exp.t) list
  -> Predicates.hpred list
  -> ((Exp.t * Exp.t) list * Predicates.hpred list * Predicates.hpred list * Predicates.hpred list)
     option
(** [find_partial_iso] finds disjoint isomorphic sub-sigmas inside a given sigma. The first argument
    is an equality checker. The function returns a partial iso and three sigmas. The first sigma is
    the first copy of the two isomorphic sigmas, so it uses expressions in the domain of the
    returned isomorphism. The second is the second copy of the two isomorphic sigmas, and it uses
    expressions in the range of the isomorphism. The third is the unused part of the input sigma. *)

val hpara_iso : Tenv.t -> Predicates.hpara -> Predicates.hpara -> bool
(** [hpara_iso] soundly checks whether two hparas are isomorphic. *)

val hpara_dll_iso : Tenv.t -> Predicates.hpara_dll -> Predicates.hpara_dll -> bool
(** [hpara_dll_iso] soundly checks whether two hpara_dlls are isomorphic. *)

val hpara_create :
     Tenv.t
  -> (Exp.t * Exp.t) list
  -> Predicates.hpred list
  -> Exp.t
  -> Exp.t
  -> Predicates.hpara * Exp.t list
(** [hpara_create] takes a correspondence, and a sigma, a root and a next for the first part of this
    correspondence. Then, it creates a hpara and discovers a list of shared expressions that are
    passed as arguments to hpara. Both of them are returned as a result. *)

val hpara_dll_create :
     Tenv.t
  -> (Exp.t * Exp.t) list
  -> Predicates.hpred list
  -> Exp.t
  -> Exp.t
  -> Exp.t
  -> Predicates.hpara_dll * Exp.t list
(** [hpara_dll_create] takes a correspondence, and a sigma, a root, a blink and a flink for the
    first part of this correspondence. Then, it creates a hpara_dll and discovers a list of shared
    expressions that are passed as arguments to hpara. Both of them are returned as a result. *)
