(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
open BufferOverrunUtils.ModelEnv

let unit_cost {pname; location} ~ret:_ _inferbo_mem =
  let autoreleasepool_trace =
    Bounds.BoundTrace.of_modeled_function (Procname.to_string pname) location
  in
  BasicCost.one ~autoreleasepool_trace ()


module Call = struct
  let dispatch : (Tenv.t, CostUtils.model, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ +PatternMatch.ObjectiveC.implements "NSObject" &:: "autorelease" &--> unit_cost
      ; -"CFAutorelease" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingFromData:error:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingWithData:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClass:fromData:error:" &--> unit_cost
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClasses:fromData:error:" &--> unit_cost ]
end
