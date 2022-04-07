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
  -> Ident.t * Typ.t
  -> Procname.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t
(** add sources and sinks coming from a particular call site *)

val check_not_tainted_wrt_sink :
     Location.t
  -> Taint.t * Trace.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t, AccessResult.error) result
