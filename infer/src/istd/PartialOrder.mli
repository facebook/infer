(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type total = [`LeftSmallerThanRight | `Equal | `RightSmallerThanLeft]

type t = [total | `NotComparable]

val of_le : le:('a -> 'a -> bool) -> lhs:'a -> rhs:'a -> t
