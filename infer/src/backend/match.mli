(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Implementation of "Smart" Pattern Matching for higher order singly-linked list predicate.

    Used for detecting on a given program if some data scructures are matching some predefined higher-order list predicates. When it is the case, these predicates can be used as possible candidates for abstracting the data-structures.
    See {{: http://dx.doi.org/10.1007/978-3-540-73368-3_22 }  CAV 2007 } for the therory involved.
*)

(* TODO: missing documentation *)

val hpara_match_with_impl : Tenv.t -> bool -> Sil.hpara -> Sil.hpara -> bool

val hpara_dll_match_with_impl : Tenv.t -> bool -> Sil.hpara_dll -> Sil.hpara_dll -> bool

(** Type for a hpred pattern. [flag=false] means that the implication
    between hpreds is not considered, and [flag = true] means that it is
    considered during pattern matching. *)
type hpred_pat = {hpred: Sil.hpred; flag: bool}

val pp_hpat : Pp.env -> Format.formatter -> hpred_pat -> unit

val pp_hpat_list : Pp.env -> Format.formatter -> hpred_pat list -> unit

type sidecondition = Prop.normal Prop.t -> Sil.exp_subst -> bool

val prop_match_with_impl :
  Tenv.t -> Prop.normal Prop.t -> sidecondition -> Ident.t list -> hpred_pat -> hpred_pat list
  -> (Sil.exp_subst * Prop.normal Prop.t) option
(** [prop_match_with_impl p condition vars hpat hpats]
    returns [(subst, p_leftover)] such that
    1) [dom(subst) = vars]
    2) [p |- (hpat.hpred * hpats.hpred)[subst] * p_leftover].
    Using the flag [field], we can control the strength of |-. *)

val find_partial_iso :
  Tenv.t -> (Exp.t -> Exp.t -> bool) -> (Exp.t * Exp.t) list -> (Exp.t * Exp.t) list
  -> Sil.hpred list
  -> ((Exp.t * Exp.t) list * Sil.hpred list * Sil.hpred list * Sil.hpred list) option
(** [find_partial_iso] finds disjoint isomorphic sub-sigmas inside a given sigma.
    The first argument is an equality checker.
    The function returns a partial iso and three sigmas. The first sigma is the first
    copy of the two isomorphic sigmas, so it uses expressions in the domain of
    the returned isomorphism. The second is the second copy of the two isomorphic sigmas,
    and it uses expressions in the range of the isomorphism. The third is the unused
    part of the input sigma. *)

(** This mode expresses the flexibility allowed during the isomorphism check *)
type iso_mode = Exact | LFieldForget | RFieldForget

val find_partial_iso_from_two_sigmas :
  Tenv.t -> iso_mode -> (Exp.t -> Exp.t -> bool) -> (Exp.t * Exp.t) list -> (Exp.t * Exp.t) list
  -> Sil.hpred list -> Sil.hpred list
  -> ((Exp.t * Exp.t) list * Sil.hpred list * Sil.hpred list * (Sil.hpred list * Sil.hpred list))
     option
(** [find_partial_iso_from_two_sigmas] finds isomorphic sub-sigmas inside two
    given sigmas. The second argument is an equality checker.
    The function returns a partial iso and four sigmas. The first
    sigma is the first copy of the two isomorphic sigmas, so it uses expressions in the domain of
    the returned isomorphism. The second is the second copy of the two isomorphic sigmas,
    and it uses expressions in the range of the isomorphism. The third and fourth
    are the unused parts of the two input sigmas. *)

val hpara_iso : Tenv.t -> Sil.hpara -> Sil.hpara -> bool
(** [hpara_iso] soundly checks whether two hparas are isomorphic. *)

val hpara_dll_iso : Tenv.t -> Sil.hpara_dll -> Sil.hpara_dll -> bool
(** [hpara_dll_iso] soundly checks whether two hpara_dlls are isomorphic. *)

val hpara_create :
  Tenv.t -> (Exp.t * Exp.t) list -> Sil.hpred list -> Exp.t -> Exp.t -> Sil.hpara * Exp.t list
(** [hpara_create] takes a correspondence, and a sigma, a root
    and a next for the first part of this correspondence. Then, it creates a
    hpara and discovers a list of shared expressions that are
    passed as arguments to hpara. Both of them are returned as a result. *)

val hpara_dll_create :
  Tenv.t -> (Exp.t * Exp.t) list -> Sil.hpred list -> Exp.t -> Exp.t -> Exp.t
  -> Sil.hpara_dll * Exp.t list
(** [hpara_dll_create] takes a correspondence, and a sigma, a root,
    a blink and a flink for the first part of this correspondence. Then,
    it creates a hpara_dll and discovers a list of shared expressions that are
    passed as arguments to hpara. Both of them are returned as a result. *)
