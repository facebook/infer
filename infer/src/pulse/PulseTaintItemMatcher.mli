(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val procedure_matches_any :
  Tenv.t -> Procname.t -> ProcAttributes.t option -> TaintConfig.Unit.procedure_unit list -> bool

val get_tainted :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> procedure_matchers:TaintConfig.Unit.procedure_unit list
  -> block_matchers:TaintConfig.Unit.procedure_unit list
  -> field_matchers:TaintConfig.Unit.field_unit list
  -> (Ident.t * Typ.t) option
  -> has_added_return_param:bool
  -> ?proc_attributes:ProcAttributes.t
  -> TaintItem.value
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
  -> AbductiveDomain.t
  -> AbductiveDomain.t
     * (TaintItem.t * ((AbstractValue.t * ValueHistory.t) * Typ.t * Exp.t option)) list
