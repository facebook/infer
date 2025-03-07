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


let map4 f e cons w x y z =
  let w' = f w in
  let x' = f x in
  let y' = f y in
  let z' = f z in
  if w == w' && x == x' && y == y' && z == z' then e else cons w' x' y' z'


let mapN f e cons xs =
  let xs' = NSArray.map_endo ~f xs in
  if xs' == xs then e else cons xs'


module Comparer = NSComparer
module Array = NSArray
module Float = NSFloat
module HashSet = NSHashSet
module HashTable = NSHashTable
module IArray = NSIArray
include IArray.Import
module Int = NSInt
module List = NSList
module Map = NSMap
module Monad = NSMonad
module Multiset = NSMultiset
module Option = NSOption
include Option.Import
module Q = NSQ_ext
module RAL = NSRal
module Result = NSResult
module Set = NSSet
module Sign = NSSign
module String = NSString
module Sys = NSSys
module Timer = NSTimer
module Z = NSZ_ext
