(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include module type of Base.Option

val pp : ('a_pp -> 'a -> unit, unit) fmt -> 'a_pp -> 'a option pp
(** Pretty-print an option. *)

val cons : 'a t -> 'a list -> 'a list

module Monad_syntax : Monad_syntax with type 'a t = 'a option
