(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val update_objc_method_posts :
     PulseSummary.t InterproceduralAnalysis.t
  -> initial_astate:ExecutionDomain.t
  -> posts:ExecutionDomain.t list
  -> ExecutionDomain.t list
(** For ObjC instance methods: appends additional nil summary and replaces must be valid reason for
    non-pod return type. Does nothing to posts for other kinds of methods *)

val append_objc_actual_self_positive :
     Procdesc.t
  -> ((AbstractValue.t * ValueHistory.t) * Typ.t) option
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val append_objc_self_positive :
  PulseSummary.t InterproceduralAnalysis.t -> ExecutionDomain.t -> ExecutionDomain.t list
(** For ObjC instance methods: adds path condition `self > 0` to a given post. Does nothing to posts
    for other kinds of methods *)

val mk_objc_method_nil_summary :
  Tenv.t -> Procdesc.t -> ExecutionDomain.t -> AbductiveDomain.t AccessResult.t option
