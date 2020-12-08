(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
module BasicCostWithReason = CostDomain.BasicCostWithReason
open BufferOverrunUtils.ModelEnv
open CostUtils.CostModelEnv

let unit_cost {model_env= {pname; location}} ~ret:_ _inferbo_mem =
  let autoreleasepool_trace =
    Bounds.BoundTrace.of_modeled_function (Procname.to_string pname) location
  in
  BasicCost.one ~autoreleasepool_trace ()


module NSArray = struct
  let index_of_object_passing_test array ({get_summary; model_env= {pname}} as cost_model_env) ~ret
      inferbo_mem =
    match pname with
    | WithBlockParameters (_, [block_name]) -> (
      match get_summary (Procname.Block block_name) with
      | Some {CostDomain.post= callee_summary} ->
          let {BasicCostWithReason.cost= callee_cost} =
            CostDomain.get_cost_kind AutoreleasepoolSize callee_summary
          in
          let length =
            CostModels.BoundsOfNSCollection.linear_length
              ~of_function:(Procname.to_simplified_string pname)
              array cost_model_env ~ret inferbo_mem
          in
          BasicCost.mult_loop ~iter:length ~body:callee_cost
      | None ->
          BasicCost.zero )
    | _ ->
        BasicCost.zero
end

module Call = struct
  let dispatch : (Tenv.t, CostUtils.model, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ +PatternMatch.ObjectiveC.implements "NSObject" &:: "autorelease" &--> unit_cost
      ; -"CFAutorelease" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "indexOfObjectPassingTest:" $ capt_exp $+ any_arg
        $--> NSArray.index_of_object_passing_test
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingFromData:error:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingWithData:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClass:fromData:error:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClasses:fromData:error:" &--> unit_cost ]
end
