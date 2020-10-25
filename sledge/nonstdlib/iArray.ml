(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** IArray - Immutable view of an array *)

open! NS0
include Array

type 'a t = 'a array

let hash_fold_t = hash_fold_array_frozen

module Import = struct
  type 'a iarray = 'a t [@@deriving compare, equal, hash, sexp]
end

let to_array xs = xs
let of_array xs = xs
