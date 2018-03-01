(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val map_changed : ('a -> 'a) -> 'a list -> 'a list
(** like map, but returns the original list if unchanged *)

val filter_changed : ('a -> bool) -> 'a list -> 'a list
(** like filter, but returns the original list if unchanged *)

val remove_irrelevant_duplicates : ('a -> 'a -> int) -> ('a -> bool) -> 'a list -> 'a list
(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and relevance functions) *)

val merge_sorted_nodup : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list -> 'a list
(** The function works on sorted lists without duplicates *)

val inter : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [inter cmp xs ys] are the elements in both [xs] and [ys], sorted according to [cmp]. *)

val fold_last : 'a list -> init:'b -> f:('b -> 'a -> 'b) -> f_last:('b -> 'a -> 'b) -> 'b
(** like fold, but apply f_last to the last element *)

val to_string : ('a -> string) -> 'a list -> string

val uncons_exn : 'a list -> 'a * 'a list
(** deconstruct a list, like hd_exn and tl_exn *)

val append_no_duplicates : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
(** [append_no_duplicates list1 list2], assuming that list1 and list2 have no duplicates on their own,
   it computes list1 @ (filtered list2), so it keeps the order of both lists and has no duplicates.
   However, the complexity is O(n^2), don't use for big lists!  *)
