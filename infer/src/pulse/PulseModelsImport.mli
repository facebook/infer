(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type arg_payload = AbstractValue.t * ValueHistory.t

type model_data =
  { analysis_data: PulseSummary.t InterproceduralAnalysis.t
  ; dispatch_call_eval_args:
         PulseSummary.t InterproceduralAnalysis.t
      -> PathContext.t
      -> Ident.t * Typ.t
      -> Exp.t
      -> (Exp.t * Typ.t) list
      -> (AbstractValue.t * ValueHistory.t) PulseAliasSpecialization.FuncArg.t list
      -> Location.t
      -> CallFlags.t
      -> AbductiveDomain.t
      -> Procname.t option
      -> ExecutionDomain.t AccessResult.t list
  ; path: PathContext.t
  ; callee_procname: Procname.t
  ; location: Location.t
  ; ret: Ident.t * Typ.t }

type model = model_data -> AbductiveDomain.t -> ExecutionDomain.t AccessResult.t list

type matcher = (Tenv.t * Procname.t, model, arg_payload) ProcnameDispatcher.Call.matcher

module Hist : sig
  val alloc_event : PathContext.t -> Location.t -> ?more:string -> string -> ValueHistory.event

  val call_event : PathContext.t -> Location.t -> ?more:string -> string -> ValueHistory.event

  val add_event : PathContext.t -> ValueHistory.event -> ValueHistory.t -> ValueHistory.t

  val single_event : PathContext.t -> ValueHistory.event -> ValueHistory.t

  val add_call :
    PathContext.t -> Location.t -> string -> ?more:string -> ValueHistory.t -> ValueHistory.t

  val single_call : PathContext.t -> Location.t -> ?more:string -> string -> ValueHistory.t

  val single_alloc : PathContext.t -> Location.t -> ?more:string -> string -> ValueHistory.t

  val binop : PathContext.t -> Binop.t -> ValueHistory.t -> ValueHistory.t -> ValueHistory.t

  val hist : PathContext.t -> ValueHistory.t -> ValueHistory.t
end

module Basic : sig
  val continue : 'a -> 'a execution_domain_base_t

  val ok_continue : 'a -> ('a execution_domain_base_t, 'b) pulse_result list

  val map_continue : ('a, 'b) pulse_result -> ('a execution_domain_base_t, 'b) pulse_result

  val shallow_copy_value :
       PathContext.t
    -> Location.t
    -> ValueHistory.event
    -> Ident.t
    -> AbstractValue.t * ValueHistory.t
    -> AbstractValue.t * ValueHistory.t
    -> AbductiveDomain.t
    -> AbductiveDomain.t execution_domain_base_t AccessResult.t list

  val shallow_copy :
       PathContext.t
    -> Location.t
    -> ValueHistory.event
    -> Ident.t
    -> AbstractValue.t * ValueHistory.t
    -> AbstractValue.t * ValueHistory.t
    -> AbductiveDomain.t
    -> AbductiveDomain.t execution_domain_base_t AccessResult.t list

  val shallow_copy_model :
    string -> AbstractValue.t * ValueHistory.t -> AbstractValue.t * ValueHistory.t -> model

  val deep_copy :
       PathContext.t
    -> Location.t
    -> value:AbstractValue.t * ValueHistory.t
    -> desc:string
    -> AbductiveDomain.t
    -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t), base_error) PulseOperationResult.t

  val alloc_value_address :
       Typ.t
    -> desc:string
    -> model_data
    -> AbductiveDomain.t
    -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t), base_error) PulseOperationResult.t

  val early_exit : model

  val return_int : desc:string -> int64 -> model

  val nondet : desc:string -> model

  val skip : model

  val id_first_arg : desc:string -> AbstractValue.t * ValueHistory.t -> model

  val free_or_delete :
       [< `Delete | `Free]
    -> Invalidation.t
    -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t
    -> model

  val alloc_not_null :
       ?desc:string
    -> Attribute.allocator
    -> Exp.t option
    -> initialize:bool
    -> model_data
    -> AbductiveDomain.t
    -> AbductiveDomain.t AccessResult.t SatUnsat.t

  val alloc_no_leak_not_null :
       ?desc:string
    -> Exp.t option
    -> initialize:bool
    -> model_data
    -> AbductiveDomain.t
    -> AbductiveDomain.t AccessResult.t SatUnsat.t

  val call_constructor :
       Typ.name
    -> Typ.t list
    -> (AbstractValue.t * ValueHistory.t) PulseAliasSpecialization.FuncArg.t list
    -> Exp.t
    -> model_data
    -> AbductiveDomain.t
    -> ExecutionDomain.t AccessResult.t list

  val assert_ : (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t -> model

  val unknown_call :
    string -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list -> model

  val matchers : matcher list
end
