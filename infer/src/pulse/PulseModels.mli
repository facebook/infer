(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface

type model =
     caller_summary:Summary.t
  -> callee_procname:Procname.t
  -> Location.t
  -> ret:Ident.t * Typ.t
  -> PulseAbductiveDomain.t
  -> PulseAbductiveDomain.t list PulseOperations.access_result

val dispatch :
     Tenv.t
  -> Procname.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> model option
