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

module VariantCostMap : sig
  type t = BasicCost.t CostIssues.CostKindMap.t

  val pp : F.formatter -> t -> unit
end

type t = VariantCostMap.t

type summary = {post: t; is_on_ui_thread: bool}

val pp_summary : F.formatter -> summary -> unit

val get_cost_kind : CostKind.t -> t -> BasicCost.t

val get_operation_cost : t -> BasicCost.t

val map : f:(BasicCost.t -> BasicCost.t) -> t -> t

val zero_record : t
(** Map representing cost record {OperationCost:0; AllocationCost:0; IOCost:0} *)

val mult_by_scalar : t -> BasicCost.t -> t
(** Map where each element is multiplied by a cost *)

val plus : t -> t -> t
(** Union of two maps where common costs are added together *)

val unit_cost_atomic_operation : t
(** Map representing cost record {OperationCost:1; AllocationCost:0; IOCost:0} *)

val unit_cost_allocation : t
(** Map representing cost record {OperationCost:0; AllocationCost:1; IOCost:0} *)

val of_operation_cost : BasicCost.t -> t
(** Map representing cost record {OperationCost:operation_cost; AllocationCost:0; IOCost:0} *)
