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
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t
(** add sources and sinks coming from a particular call site *)

val store :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> lhs:Exp.t
  -> rhs:Exp.t * ValueOrigin.t * Typ.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val load :
     Procname.t
  -> Tenv.t
  -> PathContext.t
  -> Location.t
  -> lhs:Ident.t * Typ.t
  -> rhs:Exp.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val taint_allocation :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> typ_desc:Typ.desc
  -> alloc_desc:string
  -> allocator:Attribute.allocator option
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)

val check_flows_wrt_sink :
     PathContext.t
  -> Location.t
  -> sink:TaintItem.t * Trace.t
  -> source:AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val taint_initial : Tenv.t -> ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t

val dedup_reports :
     ('a ExecutionDomain.base_t, AccessResult.error) pulse_result list
  -> ('a ExecutionDomain.base_t, AccessResult.error) pulse_result list

val procedure_matches_source : Tenv.t -> Procname.t -> bool
