(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost

type model = BufferOverrunDomain.Mem.t -> BasicCost.t

module Collections = struct
  let eval_collection_length coll_exp inferbo_mem =
    let upper_bound =
      let itv =
        BufferOverrunModels.Collection.eval_collection_length coll_exp inferbo_mem
        |> BufferOverrunDomain.Val.get_itv
      in
      match itv with Bottom -> Bounds.Bound.PInf | NonBottom itv_pure -> Itv.ItvPure.ub itv_pure
    in
    Bounds.NonNegativeBound.of_bound upper_bound


  let n_log_n b =
    let n = BasicCost.of_non_negative_bound b in
    let log_n = BasicCost.of_non_negative_bound ~degree_kind:Polynomials.DegreeKind.Log b in
    BasicCost.mult n log_n


  let sort coll_exp inferbo_mem =
    let length = eval_collection_length coll_exp inferbo_mem in
    n_log_n length
end

module Call = struct
  let dispatch : (unit, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher [-"java.util.Collections" &:: "sort" $ capt_exp $--> Collections.sort]
end
