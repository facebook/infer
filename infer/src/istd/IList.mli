(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

[@@@warning "-unused-value-declaration"]

val map_changed : equal:('a -> 'a -> bool) -> f:('a -> 'a) -> 'a list -> 'a list
(** like map, but returns the original list if unchanged *)

val filter_changed : f:('a -> bool) -> 'a list -> 'a list
(** like filter, but returns the original list if unchanged *)

val remove_irrelevant_duplicates : equal:('a -> 'a -> bool) -> f:('a -> bool) -> 'a list -> 'a list
(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and
    relevance functions) *)

val merge_sorted_nodup : cmp:('a -> 'a -> int) -> res:'a list -> 'a list -> 'a list -> 'a list
(** The function works on sorted lists without duplicates, and keeps only one copy of elements that
    appear in both lists. *)

val inter : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [inter cmp xs ys] are the elements in both [xs] and [ys], sorted according to [cmp]. *)

val fold_last : 'a list -> init:'b -> f:('b -> 'a -> 'b) -> f_last:('b -> 'a -> 'b) -> 'b
(** like fold, but apply f_last to the last element *)

val split_last_rev : 'a list -> ('a * 'a list) option
(** [split_last_rev l] is [Some (last, rev_prefix)] where [l == List.rev (last :: rev_prefix)],
    [None] if [l] is empty *)

val append_no_duplicates : cmp:('a -> 'a -> int) -> ('a list -> 'a list -> 'a list) Staged.t
(** [append_no_duplicates list1 list2], assuming that list1 and list2 have no duplicates on their
    own, it computes [list1 @ (filtered list2)], so it keeps the order of both lists and has no
    duplicates. *)

val merge_dedup : 'a list -> 'a list -> compare:('a -> 'a -> int) -> 'a list

val drop : 'a list -> int -> 'a list
(** [drop l n] returns [l] without the first [n] elements, or the empty list if [n > length l]. *)

val opt_cons : 'a option -> 'a list -> 'a list
(** [opt_cons None l] returns [l]. [opt_cons (Some x) l] returns [x :: l]*)

val remove_first : 'a list -> f:('a -> bool) -> 'a list option

val eval_until_first_some : (unit -> 'a option) list -> 'a option
(** given a list of functions taking unit, evaluate and return the first one to return [Some x] *)

val product : 'a list list -> 'a list list
(** n-ary cartesian product *)

val pp_print_list :
     max:int
  -> ?pp_sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a list
  -> unit

val fold2_result :
     init:'acc
  -> f:('acc -> 'a -> 'b -> ('acc, 'err) result)
  -> 'a list
  -> 'b list
  -> ('acc, 'err) result Base.List.Or_unequal_lengths.t

val foldi_result :
  init:'acc -> f:(int -> 'acc -> 'a -> ('acc, 'err) result) -> 'a list -> ('acc, 'err) result

val move_last_to_first : 'a list -> 'a list

val traverse_opt : 'a list -> f:('a -> 'b option) -> 'b list option
(** Applies [f] to the elements of the list and returns [None] if any application results in [None]
    otherwise returns [Some list']. *)
