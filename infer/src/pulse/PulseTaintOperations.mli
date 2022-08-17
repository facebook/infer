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
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> Ident.t * Typ.t
  -> call_was_unknown:bool
  -> (Exp.t, Procname.t) Either.t
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t
(** add sources and sinks coming from a particular call site *)

val taint_allocation :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> typ_desc:Typ.desc
  -> alloc_desc:string
  -> allocator:Attribute.allocator option
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val check_flows_wrt_sink :
     PathContext.t
  -> Location.t
  -> Taint.t * Trace.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val taint_initial :
  Tenv.t -> Procname.t -> ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t
