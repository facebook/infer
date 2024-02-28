(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

val check_retain_cycles_store :
  Tenv.t -> AbstractValue.t * ValueHistory.t -> AbductiveDomain.t -> (unit, base_error) pulse_result

val check_retain_cycles_call :
     Tenv.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> (AbstractValue.t * ValueHistory.t) option
  -> AbductiveDomain.t
  -> (unit, base_error) pulse_result
