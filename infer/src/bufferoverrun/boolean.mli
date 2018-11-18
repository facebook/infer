(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Bottom | False | True | Top

val equal : t -> t -> bool

val is_false : t -> bool

val is_true : t -> bool

val not_ : t -> t

val and_ : t -> t -> t

val or_ : t -> t -> t
