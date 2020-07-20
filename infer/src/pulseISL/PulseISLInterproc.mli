(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseISLBasicInterface

val apply_prepost :
     Procname.t
  -> Location.t
  -> callee_prepost:PulseISLAbductiveDomain.t
  -> formals:Var.t list
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> PulseISLAbductiveDomain.t
  -> ( (PulseISLAbductiveDomain.t * (AbstractValue.t * ValueHistory.t) option *  ((AbstractValue.t * ValueHistory.t)  AbstractValue.Map.t)) option
     , Diagnostic.t * PulseISLAbductiveDomain.t )
     result
(** return the abstract state after the call along with an optional return value, or [None] if the
    precondition could not be satisfied (e.g. some aliasing constraints were not satisfied) *)
