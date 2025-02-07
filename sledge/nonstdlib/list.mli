(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of ContainersLabels.List

type 'a t = 'a list [@@deriving compare, equal, sexp]

val pp :
     ?pre:(unit, unit) fmt
  -> ?suf:(unit, unit) fmt
  -> (unit, unit) fmt
  -> 'a pp
  -> 'a list pp
(** Pretty-print a list. *)

val pp_diff :
     cmp:('a -> 'a -> int)
  -> ?pre:(unit, unit) fmt
  -> ?suf:(unit, unit) fmt
  -> (unit, unit) fmt
  -> 'a pp
  -> ('a list * 'a list) pp

val hd : 'a t -> 'a option
val hd_exn : 'a t -> 'a
val tl : 'a t -> 'a t option
val tl_exn : 'a t -> 'a t
val pop_exn : 'a list -> 'a * 'a list
val mem : 'a -> 'a t -> eq:('a -> 'a -> bool) -> bool
val iter : 'a t -> f:('a -> unit) -> unit
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
val filter : 'a list -> f:('a -> bool) -> 'a list

val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t
(** [partition xs ~f] returns a pair of lists [(ts, fs)], where [ts] is the
    list of all the elements of [xs] that satisfy the predicate [f], and
    [fs] is the list of all the elements of [xs] that do not satisfy [f].
    The order of the elements in the input list is preserved. *)

val partition_map : 'a t -> f:('a -> ('l, 'r) Either.t) -> 'l t * 'r t
(** [partition_map xs ~f] returns a pair of lists [(ls, rs)] such that, for
    each element [x] of the input list [xs]:

    - if [f x] is [Left l], then [l] is in [ls], and
    - if [f x] is [Right r], then [r] is in [rs].

    The output elements are included in [ls] and [rs] in the same relative
    order as the corresponding input elements in [xs]. *)

val rev_partition_map : 'a t -> f:('a -> ('l, 'r) Either.t) -> 'l t * 'r t
(** [rev_partition_map xs ~f] is [(rev ls, rev rs)] where
    [partition_map xs ~f = (ls, rs)]. This avoids the [rev] that
    [partition_map] performs. *)

val partition_map_endo : 'a t -> f:('a -> ('a, 'r) Either.t) -> 'a t * 'r t
(** [partition_map_endo xs ~f] is equivalent to [partition_map xs ~f] but
    specialized to the case where the left and input elements have the same
    type. Preserves [==] if [f x] is [Left x] for all [x] in [xs]. *)

val fold_partition_map :
  'a t -> 's -> f:('a -> 's -> ('l, 'r) Either.t * 's) -> 'l t * 'r t * 's
(** [fold_partition_map] is like [partition_map] but threads an accumulator
    through the calls to [f]. *)

val fold_partition_map_endo :
  'a t -> 's -> f:('a -> 's -> ('a, 'r) Either.t * 's) -> 'a t * 'r t * 's
(** [fold_partition_map_endo xs s ~f] is equivalent to
    [fold_partition_map xs s ~f] but specialized to the case where the left
    and input elements have the same type. Preserves [==] if [f x] is
    [Left x] for all [x] in [xs]. *)

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like [map], but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val rev_map_split : 'a t -> f:('a -> 'b * 'c) -> 'b list * 'c list
(** [rev_map_split ~f xs] is [split (rev_map ~f xs)] but more efficient. *)

val combine : 'a t -> 'b t -> ('a * 'b) t option
val combine_exn : 'a t -> 'b t -> ('a * 'b) t

val group_by :
  'a t -> hash:('a -> int) -> eq:('a -> 'a -> bool) -> 'a list t

val join_by :
     eq:('key -> 'key -> bool)
  -> hash:('key -> int)
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a -> 'b -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t

val join_all_by :
     eq:('key -> 'key -> bool)
  -> hash:('key -> int)
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a list -> 'b list -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t

val group_join_by :
     eq:('a -> 'a -> bool)
  -> hash:('a -> int)
  -> ('b -> 'a)
  -> 'a t
  -> 'b t
  -> ('a * 'b list) t

val fold : 'a list -> 's -> f:('a -> 's -> 's) -> 's
val fold_left : 'a list -> 's -> f:('s -> 'a -> 's) -> 's
val fold_right : 'a list -> 's -> f:('a -> 's -> 's) -> 's
val foldi : 'a t -> 's -> f:(int -> 'a -> 's -> 's) -> 's
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val fold_diagonal : 'a list -> 's -> f:('a -> 'a -> 's -> 's) -> 's
val fold_map : 'a t -> 's -> f:('a -> 's -> 'b * 's) -> 'b t * 's
val fold2_exn : 'a t -> 'b t -> 's -> f:('a -> 'b -> 's -> 's) -> 's

val symmetric_diff :
  cmp:('a -> 'a -> int) -> 'a t -> 'a t -> ('a, 'a) Either.t t

module Assoc : sig
  type ('k, 'v) t = ('k * 'v) list [@@deriving compare, equal, sexp_of]

  include module type of Assoc with type ('k, 'v) t := ('k, 'v) t

  val mem : 'a -> ('a, _) t -> eq:('a -> 'a -> bool) -> bool
end

val mem_assoc : 'a -> ('a * _) t -> eq:('a -> 'a -> bool) -> bool
