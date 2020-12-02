(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

include
  module type of Containers.Option
    with module Infix := Containers.Option.Infix

type 'a t = 'a option [@@deriving compare, equal, hash, sexp]

include Monad_intf.S with type 'a t := 'a t

val pp : ('a_pp -> 'a -> unit, unit) fmt -> 'a_pp -> 'a option pp
val map_or : 'a t -> default:'b -> f:('a -> 'b) -> 'b
val flat_map : 'a t -> f:('a -> 'b t) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val fold : 'a t -> 's -> f:('a -> 's -> 's) -> 's
