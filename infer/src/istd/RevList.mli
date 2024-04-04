(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t [@@deriving compare, equal]

val empty : 'a t
(** Return empty list *)

val is_empty : 'a t -> bool
(** Check if the list is empty *)

val cons : 'a -> 'a t -> 'a t
(** Add an element to the end of list *)

val to_list : 'a t -> 'a list
(** Return normal-ordered list *)

val of_list : 'a list -> 'a t
(** Make reverse-ordered list from normal-ordered one *)

val append : 'a t -> 'a t -> 'a t
(** [append (of_list l1) (of_list l2)] is the same as [of_list (List.append l1 l2)] *)

val exists : 'a t -> f:('a -> bool) -> bool
(** Similar to [List.exists] *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** Similar to [List.map] *)

val rev_map : 'a t -> f:('a -> 'b) -> 'b list
(** Similar to [List.rev_map], so return normal-ordered list *)

val rev_map_append : 'a t -> 'b list -> f:('a -> 'b) -> 'b list
(** Similar to [List.rev_map_append] *)

val rev_partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b list * 'c list
(** Similar to [List.partition_map], but return normal-ordered lists *)
