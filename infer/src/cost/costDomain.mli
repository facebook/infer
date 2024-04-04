(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module BasicCost : sig
  include
    module type of Polynomials.NonNegativePolynomial
      with type t = Polynomials.NonNegativePolynomial.t

  val version : int
  (** version used to consistently compare at infer-reportdiff phase *)
end

module BasicCostWithReason : sig
  (** This is for [Call] instruction. Most top cost function is caused by calling top-costed
      function. The extension aims to find the root callee with top cost.

      If the callee top cost and thus the caller has top cost, then

      1. if for callee, [top_pname_opt] is [None], then callee is itself top cost, so for the
      caller, we record [top_pname_opt = callee] ;

      2. if for callee, [top_pname_opt] is [f], then we know that callee calls [f], which is top
      cost, so for the caller, we record [top_pname_opt = f] *)

  type t = {cost: BasicCost.t; top_pname_opt: Procname.t option}

  val one : t

  val zero : t

  val is_top : t -> bool

  val of_basic_cost : BasicCost.t -> t

  val is_unreachable : t -> bool

  val subst :
       Procname.t
    -> Location.t
    -> t
    -> Bounds.Bound.eval_sym
    -> FuncPtr.Set.eval_func_ptrs
    -> (Procname.t -> t option)
    -> default_closure_cost:Ints.NonNegativeInt.t
    -> t

  val degree : t -> Polynomials.Degree.t option

  val polynomial_traces : t -> Errlog.loc_trace

  val pp_hum : Format.formatter -> t -> unit
end

module VariantCostMap : sig
  type t = BasicCostWithReason.t CostIssues.CostKindMap.t

  val pp : F.formatter -> t -> unit
end

type t = VariantCostMap.t

type summary = {post: t; is_on_ui_thread: bool}

val pp_summary : F.formatter -> summary -> unit

val get_cost_kind : CostKind.t -> t -> BasicCostWithReason.t

val get_operation_cost : t -> BasicCostWithReason.t

val set_operation_cost_zero : t -> t

val find_opt : CostKind.t -> t -> BasicCostWithReason.t option

val construct : f:(CostKind.t -> BasicCostWithReason.t) -> t

val zero_record : t
(** Map representing cost record \{OperationCost:0; AllocationCost:0\} *)

val mult_by : t -> nb_exec:BasicCost.t -> t
(** Special map where each element is multipli by the number of executions *)

val plus : t -> t -> t
(** Union of two maps where common costs are added together *)

val unit_cost_atomic_operation : t
(** Map representing cost record \{OperationCost:1; AllocationCost:0\} *)
