(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace intended to be opened in each source file *)

include NS0

(** Map-and-construct operations that preserve physical equality *)

let map1 f e cons x =
  let x' = f x in
  if x == x' then e else cons x'

let map2 f e cons x y =
  let x' = f x in
  let y' = f y in
  if x == x' && y == y' then e else cons x' y'

let map3 f e cons x y z =
  let x' = f x in
  let y' = f y in
  let z' = f z in
  if x == x' && y == y' && z == z' then e else cons x' y' z'

let mapN f e cons xs =
  let xs' = Array.map_endo ~f xs in
  if xs' == xs then e else cons xs'

module Comparer = Comparer
module Array = Array
module Float = Float
module HashSet = HashSet
module HashTable = HashTable
module IArray = IArray
include IArray.Import
module Int = Int
module List = List
module Map = NSMap
module Monad = Monad
module Multiset = Multiset
module Option = Option
include Option.Import
module Q = Q_ext
module Set = NSSet
module Sign = Sign
module String = String
module Sys = Sys
module Timer = Timer
module Z = Z_ext
