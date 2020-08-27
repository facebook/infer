(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

val apply_prepost :
     Procname.t
  -> Location.t
  -> callee_prepost:PulseAbductiveDomain.t
  -> captured_vars_with_actuals:(Var.t * (AbstractValue.t * ValueHistory.t)) list
  -> formals:Var.t list
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> PulseAbductiveDomain.t
  -> ( (PulseAbductiveDomain.t * (AbstractValue.t * ValueHistory.t) option) option
     , Diagnostic.t * PulseAbductiveDomain.t )
     result
(** return the abstract state after the call along with an optional return value, or [None] if the
    precondition could not be satisfied (e.g. some aliasing constraints were not satisfied) *)
