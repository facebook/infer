(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of IterLabels

module Import : sig
  type 'a iter = 'a t
end

val pop : 'a iter -> ('a * 'a iter) option
val contains_dup : 'a iter -> cmp:('a -> 'a -> int) -> bool

val fold_opt : 'a t -> init:'s -> f:('s -> 'a -> 's option) -> 's option
(** [fold_option t ~init ~f] is a short-circuiting version of [fold] that
    runs in the [Option] monad. If [f] returns [None], [None] is returned
    without any additional invocations of [f]. *)

val fold_until :
     'a t
  -> init:'s
  -> f:('s -> 'a -> [`Continue of 's | `Stop of 'b])
  -> finish:('s -> 'b)
  -> 'b
