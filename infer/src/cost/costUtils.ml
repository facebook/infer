(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost

module CostModelEnv = struct
  type cost_model_env =
    { model_env: BufferOverrunUtils.ModelEnv.model_env
    ; get_summary: Procname.t -> CostDomain.summary option }
end

type model =
  CostModelEnv.cost_model_env -> ret:Ident.t * Typ.t -> BufferOverrunDomain.Mem.t -> BasicCost.t

let unit_cost_of ~of_function loc =
  Bounds.NonNegativeBound.of_modeled_function of_function loc Bounds.Bound.one
  |> BasicCost.of_non_negative_bound ~degree_kind:Polynomials.DegreeKind.Linear


let cost_of_itv ~(itv : Itv.t) ~degree_kind ~of_function loc =
  let upper_bound =
    match itv with Bottom -> Bounds.Bound.pinf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
  in
  Bounds.NonNegativeBound.of_modeled_function of_function loc upper_bound
  |> BasicCost.of_non_negative_bound ~degree_kind


let string_len_range_itv model_env exp ~from mem =
  let itv =
    BufferOverrunModels.JavaString.get_length model_env exp mem |> BufferOverrunDomain.Val.get_itv
  in
  Option.value_map from ~default:itv ~f:(fun (start_exp, integer_type_widths) ->
      let start_itv =
        BufferOverrunSemantics.eval integer_type_widths start_exp mem
        |> BufferOverrunDomain.Val.get_itv
      in
      Itv.minus itv start_itv )
  |> Itv.set_lb_zero


module type S = sig
  val length : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
end

module Array : S = struct
  let length arr_exp inferbo_mem =
    BufferOverrunSemantics.eval_array_locs_length
      (BufferOverrunSemantics.eval_locs arr_exp inferbo_mem)
      inferbo_mem
end

module Collection : S = struct
  let length coll_exp inferbo_mem =
    BufferOverrunModels.Collection.eval_collection_length coll_exp inferbo_mem
end

module Container : S = struct
  let length coll_exp inferbo_mem =
    BufferOverrunModels.Container.eval_collection_length coll_exp inferbo_mem
end

module NSCollection : S = struct
  let length coll_exp inferbo_mem =
    BufferOverrunModels.NSCollection.eval_collection_length coll_exp inferbo_mem
end

module CString : S = struct
  let length exp inferbo_mem = BufferOverrunSemantics.eval_string_len exp inferbo_mem
end
