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
     PathContext.t
  -> is_isl_error_prepost:bool
  -> Procname.t
  -> Location.t
  -> callee_prepost:AbductiveDomain.t
  -> captured_vars_with_actuals:
       ((Var.t * Typ.t) * ((AbstractValue.t * ValueHistory.t) * Typ.t)) list
  -> formals:(Var.t * Typ.t) list
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> AbductiveDomain.t
  -> ( AbductiveDomain.t
     * (AbstractValue.t * ValueHistory.t) option
     * (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t )
     AccessResult.t
     SatUnsat.t
(** result of applying one pre/post pair of a callee's summary:

    - {!SatUnsat.Unsat} if that path in the callee is infeasible
    - otherwise, there can be an error detected
    - otherwise, the result is a new abstract state, an optional return value, and a substitution
      [callee_abstract_value -> caller_abstract_value] mapping callee's abstract values to what they
      became in the new (caller) state *)
