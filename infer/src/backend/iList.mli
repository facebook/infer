(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Generic comparison of lists given a compare function for the elements of the list *)
val compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(** Generic equality of lists given a compare function for the elements of the list *)
val equal : ('a -> 'b -> int) -> 'a list -> 'b list -> bool

(** tail-recursive variant of List.append *)
val append : 'a list -> 'a list -> 'a list

(** tail-recursive variant of List.combine *)
val combine : 'a list -> 'b list -> ('a * 'b) list

val exists : ('a -> bool) -> 'a list -> bool
val filter : ('a -> bool) -> 'a list -> 'a list

(** tail-recursive variant of List.flatten *)
val flatten : 'a list list -> 'a list

(** Remove all None elements from the list. *)
val flatten_options : ('a option) list -> 'a list

val find : ('a -> bool) -> 'a list -> 'a
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val for_all : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val hd : 'a list -> 'a
val iter : ('a -> unit) -> 'a list -> unit
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val length : 'a list -> int

(** tail-recursive variant of List.fold_right *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(** tail-recursive variant of List.map *)
val map : ('a -> 'b) -> 'a list -> 'b list

(** tail-recursive variant of List.mapi *)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** Like List.mem but without builtin equality *)
val mem : ('a -> 'b -> bool) -> 'a -> 'b list -> bool

val nth : 'a list -> int -> 'a
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val rev : 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list

(** tail-recursive variant of List.split *)
val split : ('a * 'b) list -> 'a list * 'b list

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val tl : 'a list -> 'a list

(* Drops the first n elements from a list. *)
val drop_first : int -> 'a list -> 'a list

(* Drops the last n elements from a list. *)
val drop_last : int -> 'a list -> 'a list

(** Returns (reverse input_list)[@]acc *)
val rev_with_acc : 'a list -> 'a list -> 'a list

(** Remove consecutive equal elements from a list (according to the given comparison functions) *)
val remove_duplicates : ('a -> 'a -> int) -> 'a list -> 'a list

(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and relevance functions) *)
val remove_irrelevant_duplicates : ('a -> 'a -> int) -> ('a -> bool) -> 'a list -> 'a list

(** The function works on sorted lists without duplicates *)
val merge_sorted_nodup : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list -> 'a list

(** Returns whether there is an intersection in the elements of the two lists.
    The compare function is required to sort the lists. *)
val intersect : ('a -> 'a -> int) -> 'a list -> 'a list -> bool

(** Like List.mem_assoc but without builtin equality *)
val mem_assoc : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool

(** Like List.assoc but without builtin equality *)
val assoc : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

exception Fail

(** Apply [f] to pairs of elements; raise [Fail] if the two lists have different lenghts. *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

val to_string : ('a -> string) -> 'a list -> string
