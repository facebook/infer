(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* file used only to apply the deriver through [inline] and commit the generated functions
   so as to track changes *)

type and_domain = {boolean_and: AbstractDomain.BooleanAnd.t} [@@deriving_inline abstract_domain]

let _ = fun (_ : and_domain) -> ()

let join_and_domain lhs rhs =
  if phys_equal lhs rhs then lhs
  else
    let boolean_and = AbstractDomain.BooleanAnd.join lhs.boolean_and rhs.boolean_and in
    if phys_equal boolean_and lhs.boolean_and then lhs
    else if phys_equal boolean_and rhs.boolean_and then rhs
    else {boolean_and}


let _ = join_and_domain

let leq_and_domain ~lhs ~rhs =
  phys_equal lhs rhs || AbstractDomain.BooleanAnd.leq ~lhs:lhs.boolean_and ~rhs:rhs.boolean_and


let _ = leq_and_domain

let widen_and_domain ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let boolean_and =
      AbstractDomain.BooleanAnd.widen ~prev:prev.boolean_and ~next:next.boolean_and ~num_iters
    in
    if phys_equal boolean_and next.boolean_and then next
    else if phys_equal boolean_and prev.boolean_and then prev
    else {boolean_and}


let _ = widen_and_domain

[@@@end]

module MaxCount = struct
  let max = 5
end

module MaxCountDomain = AbstractDomain.CountDomain (MaxCount)

type two_count_domains = {c1: MaxCountDomain.t; c2: MaxCountDomain.t}
[@@deriving_inline abstract_domain]

let _ = fun (_ : two_count_domains) -> ()

let join_two_count_domains lhs rhs =
  if phys_equal lhs rhs then lhs
  else
    let c2 = MaxCountDomain.join lhs.c2 rhs.c2 in
    let c1 = MaxCountDomain.join lhs.c1 rhs.c1 in
    if phys_equal c2 lhs.c2 && phys_equal c1 lhs.c1 then lhs
    else if phys_equal c2 rhs.c2 && phys_equal c1 rhs.c1 then rhs
    else {c1; c2}


let _ = join_two_count_domains

let leq_two_count_domains ~lhs ~rhs =
  phys_equal lhs rhs
  || (MaxCountDomain.leq ~lhs:lhs.c1 ~rhs:rhs.c1 && MaxCountDomain.leq ~lhs:lhs.c2 ~rhs:rhs.c2)


let _ = leq_two_count_domains

let widen_two_count_domains ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let c2 = MaxCountDomain.widen ~prev:prev.c2 ~next:next.c2 ~num_iters in
    let c1 = MaxCountDomain.widen ~prev:prev.c1 ~next:next.c1 ~num_iters in
    if phys_equal c2 next.c2 && phys_equal c1 next.c1 then next
    else if phys_equal c2 prev.c2 && phys_equal c1 prev.c1 then prev
    else {c1; c2}


let _ = widen_two_count_domains

[@@@end]
