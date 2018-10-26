(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type cause =
  | CppDelete of AccessExpression.t
  | CppDestructor of Typ.Procname.t * AccessExpression.t
  | StdVectorPushBack of AccessExpression.t
[@@deriving compare]

type t = {cause: cause; location: Location.t} [@@deriving compare]

let issue_type_of_cause = function
  | CppDelete _ ->
      IssueType.use_after_delete
  | CppDestructor _ ->
      IssueType.use_after_lifetime
  | StdVectorPushBack _ ->
      IssueType.use_after_lifetime


let pp f ({cause; location}[@warning "+9"]) =
  match cause with
  | CppDelete access_expr ->
      F.fprintf f "invalidated by call to `delete %a` at %a" AccessExpression.pp access_expr
        Location.pp location
  | CppDestructor (proc_name, access_expr) ->
      F.fprintf f "invalidated by destructor call `%a(%a)` at %a" Typ.Procname.pp proc_name
        AccessExpression.pp access_expr Location.pp location
  | StdVectorPushBack access_expr ->
      F.fprintf f "potentially invalidated by call to `std::vector::push_back(%a, ..)` at %a"
        AccessExpression.pp access_expr Location.pp location


module Domain : AbstractDomain.S with type astate = t = struct
  type astate = t

  let pp = pp

  let join i1 i2 =
    if [%compare.equal: t] i1 i2 then i1
    else
      (* take the max, but it should be unusual for the same location to be invalidated in two
         different ways *)
      let kept, forgotten = if compare i1 i2 >= 0 then (i1, i2) else (i2, i1) in
      L.debug Analysis Quiet
        "forgetting about invalidation %a for address already invalidated by %a@\n" pp forgotten pp
        kept ;
      kept


  let ( <= ) ~lhs ~rhs = compare lhs rhs <= 0

  let widen ~prev ~next ~num_iters:_ = join prev next
end

include Domain
