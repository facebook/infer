(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type total = [`LeftSmallerThanRight | `Equal | `RightSmallerThanLeft]

type t = [total | `NotComparable]

let of_le ~le ~lhs ~rhs =
  let ller = le lhs rhs in
  let rlel = le rhs lhs in
  match (ller, rlel) with
  | true, true ->
      `Equal
  | true, false ->
      `LeftSmallerThanRight
  | false, true ->
      `RightSmallerThanLeft
  | false, false ->
      `NotComparable
