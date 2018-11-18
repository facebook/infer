(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Bottom | False | True | Top [@@deriving compare]

let equal = [%compare.equal: t]

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
