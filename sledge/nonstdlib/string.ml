(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
module String = ContainersLabels.String

let rtake n s =
  let l = String.length s in
  if n >= l then s else String.sub s ~pos:(l - n) ~len:n

let rdrop n s =
  let l = String.length s in
  if n >= l then "" else String.sub s ~pos:0 ~len:(l - n)

let rtake_drop n s = (rtake n s, rdrop n s)

module T = struct
  type t = string [@@deriving compare, equal, hash, sexp]
end

include T
include String

let index_exn = index
let index = index_opt
let index_from_exn = index_from
let index_from = index_from_opt
let rindex_exn = rindex
let rindex = rindex_opt
let rindex_from_exn = rindex_from
let rindex_from = rindex_from_opt

module Set = NSSet.Make (T)
module Map = NSMap.Make (T)
module Tbl = HashTable.Make (T)
