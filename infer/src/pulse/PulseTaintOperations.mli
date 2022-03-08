(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val call :
     PathContext.t
  -> Location.t
  -> Procname.t option
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> Ident.t * Typ.t
  -> ExecutionDomain.t AccessResult.t list
  -> ExecutionDomain.t AccessResult.t list
(** add sources and sinks coming from a particular call site *)
