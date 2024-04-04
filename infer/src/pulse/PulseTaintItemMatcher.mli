(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type taint_match = {taint: TaintItem.t; value_origin: ValueOrigin.t; typ: Typ.t; exp: Exp.t option}

val procedure_matches :
     Tenv.t
  -> TaintConfig.Unit.procedure_unit list
  -> ?block_passed_to:Procname.t
  -> ?proc_attributes:ProcAttributes.t
  -> Procname.t
  -> 'a ProcnameDispatcher.Call.FuncArg.t list
  -> TaintConfig.Unit.procedure_unit list

val procedure_matches_any :
  Tenv.t -> Procname.t -> ProcAttributes.t option -> TaintConfig.Unit.procedure_unit list -> bool

val procedure_matching_kinds :
     Tenv.t
  -> Procname.t
  -> ProcAttributes.t option
  -> TaintConfig.Unit.procedure_unit list
  -> TaintConfig.Kind.Set.t

val match_procedure_call :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> ?proc_attributes:ProcAttributes.t
  -> has_added_return_param:bool
  -> Procname.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> Ident.t * Typ.t
  -> TaintConfig.Unit.procedure_unit list
  -> AbductiveDomain.t
  -> AbductiveDomain.t * taint_match list

val match_procedure :
     Tenv.t
  -> ProcAttributes.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> TaintConfig.Unit.procedure_unit list
  -> AbductiveDomain.t
  -> AbductiveDomain.t * taint_match list

val match_block :
     Tenv.t
  -> Location.t
  -> ?proc_attributes:ProcAttributes.t
  -> Procname.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> TaintConfig.Unit.procedure_unit list
  -> AbductiveDomain.t
  -> AbductiveDomain.t * taint_match list

val match_field :
     Tenv.t
  -> Location.t
  -> Fieldname.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t
     (* TODO(arr): FuncArg.t is incidental here (matches the shape of data, but not its semantics
        which is a source/dest of store/load instruction. *)
  -> TaintConfig.Unit.field_unit list
  -> AbductiveDomain.t
  -> AbductiveDomain.t * taint_match list
