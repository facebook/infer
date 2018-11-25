(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Bottom | False | True | Top

val of_bool : bool -> t

val equal : t -> t -> bool

val is_false : t -> bool

val is_true : t -> bool

val not_ : t -> t

val and_ : t -> t -> t

val or_ : t -> t -> t

module EqualOrder : sig
  (**
  This module provides abstract transfer functions for comparisons on unordered values, only based on equality, like abstract locations.
*)

  type b = t

  type t = {on_equal: b; on_not_equal: b}

  val eq : t

  val ne : t

  val strict_cmp : t
  (** > or < *)

  val loose_cmp : t
  (** >= or <= *)

  val top : t

  val of_equal : t -> b -> b
end
