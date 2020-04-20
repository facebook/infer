(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include module type of Core.List

val pp :
     ?pre:(unit, unit) fmt
  -> ?suf:(unit, unit) fmt
  -> (unit, unit) fmt
  -> 'a pp
  -> 'a list pp
(** Pretty-print a list. *)

val pp_diff :
     compare:('a -> 'a -> int)
  -> (unit, unit) fmt
  -> 'a pp
  -> ('a list * 'a list) pp

val pop_exn : 'a list -> 'a * 'a list

val find_map_remove :
  'a list -> f:('a -> 'b option) -> ('b * 'a list) option

val fold_option :
  'a t -> init:'accum -> f:('accum -> 'a -> 'accum option) -> 'accum option
(** [fold_option t ~init ~f] is a short-circuiting version of [fold] that
    runs in the [Option] monad. If [f] returns [None], that value is
    returned without any additional invocations of [f]. *)

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like map, but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val filter_map_endo : 'a t -> f:('a -> 'a option) -> 'a t
(** Like filter_map, but specialized to require [f] to be an endofunction,
    which enables preserving [==] if [f] preserves [==] of every element. *)

val rev_map_unzip : 'a t -> f:('a -> 'b * 'c) -> 'b list * 'c list
(** [rev_map_unzip ~f xs] is [unzip (rev_map ~f xs)] but more efficient. *)

val remove_exn : ?equal:('a -> 'a -> bool) -> 'a list -> 'a -> 'a list
(** Returns the input list without the first element [equal] to the
    argument, or raise [Not_found_s] if no such element exists. [equal]
    defaults to physical equality. *)

val remove : ?equal:('a -> 'a -> bool) -> 'a list -> 'a -> 'a list option
val rev_init : int -> f:(int -> 'a) -> 'a list

val symmetric_diff :
  compare:('a -> 'a -> int) -> 'a t -> 'a t -> ('a, 'a) Either.t t
