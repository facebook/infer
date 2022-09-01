(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val append_objc_actual_self_positive :
     Procname.t
  -> ProcAttributes.t
  -> ((AbstractValue.t * ValueHistory.t) * Typ.t) option
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val initial_with_positive_self :
  Procname.t -> ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t
(** The initial state of the analysis, with the additional path condition [self > 0] for Objective-C
    instance methods. *)

val mk_nil_messaging_summary :
  Tenv.t -> Procname.t -> ProcAttributes.t -> ExecutionDomain.summary option
