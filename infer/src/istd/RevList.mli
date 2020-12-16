(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val cons : 'a -> 'a t -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

val exists : 'a t -> f:('a -> bool) -> bool

val map : 'a t -> f:('a -> 'b) -> 'b t

val rev_map : 'a t -> f:('a -> 'b) -> 'b list

val rev_map_append : 'a t -> 'b list -> f:('a -> 'b) -> 'b list

val rev_partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b list * 'c list

val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b list

val rev_concat_map : 'a t -> f:('a -> 'b list) -> 'b list

val rev_append : 'a list -> 'a t -> 'a t

val rev_append2 : 'a t -> 'a list -> 'a list

val dedup_and_sort : compare:('a -> 'a -> int) -> 'a t -> 'a list

val to_rev_seq : 'a t -> 'a Seq.t
