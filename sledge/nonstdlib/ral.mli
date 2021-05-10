(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of CCRAL

val pp :
     ?pre:(unit, unit) fmt
  -> ?suf:(unit, unit) fmt
  -> (unit, unit) fmt
  -> 'a pp
  -> 'a t pp

val fold : 'a t -> 's -> f:('a -> 's -> 's) -> 's
