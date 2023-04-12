(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module FuncArg = ProcnameDispatcher.Call.FuncArg
open PulseBasicInterface

val make_specialization :
     Procname.t
  -> ((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> PulseOperations.call_kind
  -> PulsePathContext.t
  -> Location.t
  -> PulseAbductiveDomain.t
  -> Specialization.Pulse.Aliases.t option
