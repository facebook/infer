(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val apply_prepost :
     Procname.t
  -> Location.t
  -> callee_prepost:AbductiveDomain.t
  -> captured_vars_with_actuals:(Var.t * (AbstractValue.t * ValueHistory.t)) list
  -> formals:Var.t list
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t) option) PulseReport.access_result
     SatUnsat.t
