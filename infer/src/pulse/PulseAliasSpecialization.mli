(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module FuncArg = ProcnameDispatcher.Call.FuncArg
open PulseBasicInterface

val make_specialized_call_exp :
     Procname.t
  -> (AbstractValue.t * ValueHistory.t) FuncArg.t list
  -> PulseOperations.call_kind
  -> PulsePathContext.t
  -> Location.t
  -> PulseAbductiveDomain.t
  -> (Procname.t * Exp.t * PulseAbductiveDomain.t) option
