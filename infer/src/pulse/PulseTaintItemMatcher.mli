(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type taint_match =
  {taint: TaintItem.t; addr_hist: AbstractValue.t * ValueHistory.t; typ: Typ.t; exp: Exp.t option}

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
  -> AbductiveDomain.t * taint_match list
