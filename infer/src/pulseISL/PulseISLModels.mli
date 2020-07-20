(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseISLBasicInterface
open PulseISLDomainInterface

type model =
     PulseISLSummary.t InterproceduralAnalysis.t
  -> callee_procname:Procname.t
  -> Location.t
  -> ret:Ident.t * Typ.t
  -> AbductiveDomain.t
  -> ExecutionDomain.t list PulseISLOperations.access_result

val dispatch :
     Tenv.t
  -> Procname.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> model option
