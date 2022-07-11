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

val gather_taint_dependencies : AbstractValue.t -> AbductiveDomain.t -> AbstractValue.t list
(** Preorder traversal of the tree formed by taint dependencies of [v] in [astate] *)

val report_flows_to_callee :
     PathContext.t
  -> Location.t
  -> Procname.t
  -> (BaseStack.value * Typ.t) list
  -> AbductiveDomain.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t
(** Add callee as a taint procedure and report taint flows to callee actuals *)

val check_flows_wrt_sink :
     PathContext.t
  -> Location.t
  -> Taint.t * Trace.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val taint_initial :
  Tenv.t -> Procname.t -> ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t
