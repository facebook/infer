(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type total = [`LeftSmallerThanRight | `Equal | `RightSmallerThanLeft]

type t = [total | `NotComparable]

val join : [< t] -> [< t] -> t

type 'a xcompare = lhs:'a -> rhs:'a -> t

type 'a xcompare_total = lhs:'a -> rhs:'a -> total

val of_compare : compare:('a -> 'a -> int) -> 'a xcompare_total

val of_le : le:('a -> 'a -> bool) -> 'a xcompare

val of_opt : xcompare_elt:'a xcompare -> 'a option xcompare

val container : fold:('t, 'a * 'a, t) Container.fold -> 't -> xcompare_elt:'a xcompare -> t
