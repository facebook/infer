(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val matcher_of_config :
     default_taint_target:Pulse_config_t.taint_target
  -> option_name:string
  -> Pulse_config_j.matcher list
  -> TaintConfig.Unit.t list

val procedure_matches :
     Tenv.t
  -> TaintConfig.Unit.t list
  -> ?block_passed_to:Procname.t
  -> Procname.t
  -> 'a ProcnameDispatcher.Call.FuncArg.t list
  -> TaintConfig.Unit.t list

val get_tainted :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> TaintConfig.Unit.t list
  -> (Ident.t * Typ.t) option
  -> has_added_return_param:bool
  -> ?block_passed_to:Procname.t
  -> Procname.t
  -> BaseStack.value ProcnameDispatcher.Call.FuncArg.t list
  -> AbductiveDomain.t
  -> AbductiveDomain.t * (TaintItem.t * (BaseStack.value * Typ.t * Exp.t option)) list
