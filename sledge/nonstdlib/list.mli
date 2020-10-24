(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of ContainersLabels.List

type 'a t = 'a list [@@deriving compare, equal, hash, sexp]

val pp :
     ?pre:(unit, unit) fmt
  -> ?suf:(unit, unit) fmt
  -> (unit, unit) fmt
  -> 'a pp
  -> 'a list pp
(** Pretty-print a list. *)

val pp_diff :
     cmp:('a -> 'a -> int)
  -> (unit, unit) fmt
  -> 'a pp
  -> ('a list * 'a list) pp

val hd : 'a t -> 'a option
val hd_exn : 'a t -> 'a
val tl : 'a t -> 'a t option
val tl_exn : 'a t -> 'a t
val pop_exn : 'a list -> 'a * 'a list
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val find : 'a t -> f:('a -> bool) -> 'a option
val find_exn : 'a t -> f:('a -> bool) -> 'a
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b

val remove_one_exn : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
(** Returns the input list without the first element [eq]ual to the
    argument, or raise [Not_found] if no such element exists. *)

val remove_one : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list option
val remove : eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
val map : 'a t -> f:('a -> 'b) -> 'b t

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like [map], but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val rev_map_split : 'a t -> f:('a -> 'b * 'c) -> 'b list * 'c list
(** [rev_map_split ~f xs] is [split (rev_map ~f xs)] but more efficient. *)

val combine : 'a t -> 'b t -> ('a * 'b) t option
val combine_exn : 'a t -> 'b t -> ('a * 'b) t
val fold : 'a list -> init:'s -> f:('s -> 'a -> 's) -> 's
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val fold2_exn : 'a t -> 'b t -> init:'s -> f:('s -> 'a -> 'b -> 's) -> 's

val symmetric_diff :
  cmp:('a -> 'a -> int) -> 'a t -> 'a t -> ('a, 'a) Either.t t
