(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface
open PulseDomainInterface

type model_data =
  { analysis_data: PulseSummary.t InterproceduralAnalysis.t
  ; path: PathContext.t
  ; callee_procname: Procname.t
  ; location: Location.t
  ; ret: Ident.t * Typ.t }

type model = model_data -> AbductiveDomain.t -> ExecutionDomain.t AccessResult.t list

val dispatch :
     Tenv.t
  -> Procname.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> model option
