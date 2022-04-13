(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val make_specialized_call_exp :
     'a InterproceduralAnalysis.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> Procname.t
  -> PulseOperations.call_kind
  -> PathContext.t
  -> Location.t
  -> AbductiveDomain.t
  -> (Procname.t * Exp.t * AbductiveDomain.t) option
