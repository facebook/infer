(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** like map, but returns the original list if unchanged *)
val map_changed : ('a -> 'a) -> 'a list -> 'a list

(** like filter, but returns the original list if unchanged *)
val filter_changed : ('a -> bool) -> 'a list -> 'a list

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val rev : 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list

(** last element, if any *)
val last : 'a list -> 'a option

(* Drops the first n elements from a list. *)
val drop_first : int -> 'a list -> 'a list

(* Drops the last n elements from a list. *)
val drop_last : int -> 'a list -> 'a list

(** Remove consecutive equal elements from a list (according to the given comparison functions) *)
val remove_duplicates : ('a -> 'a -> int) -> 'a list -> 'a list

(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and relevance functions) *)
val remove_irrelevant_duplicates : ('a -> 'a -> int) -> ('a -> bool) -> 'a list -> 'a list

(** The function works on sorted lists without duplicates *)
val merge_sorted_nodup : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list -> 'a list

(** Returns whether there is an intersection in the elements of the two lists.
    The compare function is required to sort the lists. *)
val intersect : ('a -> 'a -> int) -> 'a list -> 'a list -> bool

(** [inter cmp xs ys] are the elements in both [xs] and [ys], sorted according to [cmp]. *)
val inter : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

(** Like List.mem_assoc but without builtin equality *)
val mem_assoc : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool

(** Like List.assoc but without builtin equality *)
val assoc : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

(** Return the first non-None result found when applying f to elements of l *)
val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option

val to_string : ('a -> string) -> 'a list -> string

(** Creates an list, inclusive. E.g. `range 2 4` -> [2, 3, 4].

    Not tail-recursive.*)
val range : int -> int -> int list
