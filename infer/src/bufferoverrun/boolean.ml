(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Bottom | False | True | Top [@@deriving compare]

let equal = [%compare.equal: t]

let of_bool = function false -> False | true -> True

let is_false = function False -> true | _ -> false

let is_true = function True -> true | _ -> false

let not_ = function Bottom -> Bottom | False -> True | True -> False | Top -> Top

let and_ x y =
  match (x, y) with
  | Bottom, _ | _, Bottom ->
      Bottom
  | False, _ | _, False ->
      False
  | True, b | b, True ->
      b
  | Top, Top ->
      Top


let or_ x y =
  match (x, y) with
  | Bottom, _ | _, Bottom ->
      Bottom
  | False, b | b, False ->
      b
  | True, _ | _, True ->
      True
  | Top, Top ->
      Top


module EqualOrder = struct
  type b = t

  type t = {on_equal: b; on_not_equal: b}

  let eq = {on_equal= True; on_not_equal= False}

  let ne = {on_equal= False; on_not_equal= True}

  let strict_cmp = {on_equal= False; on_not_equal= Top}

  let loose_cmp = {on_equal= True; on_not_equal= Top}

  let top = {on_equal= Top; on_not_equal= Top}

  let of_equal t equal =
    match equal with
    | Bottom ->
        Bottom
    | True ->
        t.on_equal
    | False ->
        t.on_not_equal
    | Top ->
        (* join t.on_equal t.on_not_equal is always Top *) Top
end
