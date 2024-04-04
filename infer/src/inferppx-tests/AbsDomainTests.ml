(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type and_domain = {boolean_and: AbstractDomain.BooleanAnd.t} [@@deriving abstract_domain]

let%test "and_join" =
  let t1 = {boolean_and= true} in
  let t2 = {boolean_and= true} in
  let join_t = join_and_domain t1 t2 in
  join_t.boolean_and


let%test "and_leq" =
  let t1 = {boolean_and= true} in
  let t2 = {boolean_and= false} in
  leq_and_domain ~lhs:t1 ~rhs:t2


module MaxCount = struct
  let max = 5
end

module MaxCountDomain = AbstractDomain.CountDomain (MaxCount)

type two_count_domains = {c1: MaxCountDomain.t; c2: MaxCountDomain.t} [@@deriving abstract_domain]

let zero = MaxCountDomain.bottom

let one = MaxCountDomain.increment zero

let five = MaxCountDomain.top

let zero_zero = {c1= zero; c2= zero}

let zero_one = {c1= zero; c2= one}

let five_zero = {c1= five; c2= zero}

let%test "count_leq" =
  leq_two_count_domains ~lhs:zero_zero ~rhs:zero_one
  && leq_two_count_domains ~lhs:zero_zero ~rhs:five_zero
  && not (leq_two_count_domains ~lhs:zero_one ~rhs:five_zero)


let%test "count_join_physeq1" =
  let join_t = join_two_count_domains zero_zero zero_zero in
  phys_equal zero_zero join_t


let%test "count_join_physeq2" =
  let join_t = join_two_count_domains zero_zero zero_one in
  phys_equal zero_one join_t


let%test "count_join" =
  let join_t = join_two_count_domains five_zero zero_one in
  Int.equal (join_t.c1 :> int) 5 && Int.equal (join_t.c2 :> int) 1


let%test "count_widen" =
  let widen_t = widen_two_count_domains ~prev:five_zero ~next:zero_one ~num_iters:0 in
  Int.equal (widen_t.c1 :> int) 5 && Int.equal (widen_t.c2 :> int) 1
