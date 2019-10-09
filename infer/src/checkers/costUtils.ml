(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv

type model = model_env -> ret:Ident.t * Typ.t -> BufferOverrunDomain.Mem.t -> BasicCost.t

let of_itv ~(itv : Itv.t) ~degree_kind ~of_function loc =
  let upper_bound =
    match itv with Bottom -> Bounds.Bound.pinf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
  in
  Bounds.NonNegativeBound.of_modeled_function of_function loc upper_bound
  |> BasicCost.of_non_negative_bound ~degree_kind


(** Given a string of length n and an optional starting index i (0 by
   default), return itv [0, n_u-i_l] *)
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
