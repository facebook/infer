(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type total = [`LeftSmallerThanRight | `Equal | `RightSmallerThanLeft]

type t = [total | `NotComparable]

let join t1 t2 =
  match (t1, t2) with
  | `Equal, `Equal ->
      `Equal
  | (`LeftSmallerThanRight | `Equal), (`LeftSmallerThanRight | `Equal) ->
      `LeftSmallerThanRight
  | (`RightSmallerThanLeft | `Equal), (`RightSmallerThanLeft | `Equal) ->
      `RightSmallerThanLeft
  | `LeftSmallerThanRight, `RightSmallerThanLeft
  | `RightSmallerThanLeft, `LeftSmallerThanRight
  | _, `NotComparable
  | `NotComparable, _ ->
      `NotComparable


type 'a xcompare = lhs:'a -> rhs:'a -> t

type 'a xcompare_total = lhs:'a -> rhs:'a -> total

let of_compare ~compare ~lhs ~rhs =
  let r = compare lhs rhs in
  if r < 0 then `LeftSmallerThanRight else if r > 0 then `RightSmallerThanLeft else `Equal


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


let of_opt ~xcompare_elt ~lhs ~rhs =
  match (lhs, rhs) with
  | None, None ->
      `Equal
  | None, Some _ ->
      `LeftSmallerThanRight
  | Some _, None ->
      `RightSmallerThanLeft
  | Some lhs, Some rhs ->
      xcompare_elt ~lhs ~rhs


let join_lazy t1 ~xcompare ~lhs ~rhs =
  match t1 with `NotComparable -> `NotComparable | _ -> join t1 (xcompare ~lhs ~rhs)


let container ~(fold : ('t, 'a * 'a, t) Container.fold) cont ~xcompare_elt =
  fold cont ~init:`Equal ~f:(fun acc (lhs, rhs) -> join_lazy acc ~xcompare:xcompare_elt ~lhs ~rhs)
