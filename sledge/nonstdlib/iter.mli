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

val mem : 'a -> 'a t -> eq:('a -> 'a -> bool) -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t
val flat_map : 'a t -> f:('a -> 'b t) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val sort : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val sort_uniq : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val sorted : 'a t -> cmp:('a -> 'a -> int) -> bool
val group_succ_by : 'a t -> eq:('a -> 'a -> bool) -> 'a list t

val group_by :
  'a t -> hash:('a -> int) -> eq:('a -> 'a -> bool) -> 'a list t

val count :
  'a t -> hash:('a -> int) -> eq:('a -> 'a -> bool) -> ('a * int) t

val uniq : 'a t -> eq:('a -> 'a -> bool) -> 'a t

val join_by :
     eq:'key equal
  -> hash:'key hash
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a -> 'b -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t

val join_all_by :
     eq:'key equal
  -> hash:'key hash
  -> ('a -> 'key)
  -> ('b -> 'key)
  -> merge:('key -> 'a list -> 'b list -> 'c option)
  -> 'a t
  -> 'b t
  -> 'c t

val group_join_by :
     eq:'a equal
  -> hash:'a hash
  -> ('b -> 'a)
  -> 'a t
  -> 'b t
  -> ('a * 'b list) t

val inter : eq:'a equal -> hash:'a hash -> 'a t -> 'a t -> 'a t
val union : eq:'a equal -> hash:'a hash -> 'a t -> 'a t -> 'a t
val diff : eq:'a equal -> hash:'a hash -> 'a t -> 'a t -> 'a t
val subset : eq:'a equal -> hash:'a hash -> 'a t -> 'a t -> bool
val max : 'a t -> lt:('a -> 'a -> bool) -> 'a option
val max_exn : 'a t -> lt:('a -> 'a -> bool) -> 'a
val min : 'a t -> lt:('a -> 'a -> bool) -> 'a option
val min_exn : 'a t -> lt:('a -> 'a -> bool) -> 'a
val pop : 'a iter -> ('a * 'a iter) option
val find : 'a t -> f:('a -> bool) -> 'a option
val find_exn : 'a t -> f:('a -> bool) -> 'a
val find_map : 'a iter -> f:('a -> 'b option) -> 'b option
val contains_dup : 'a iter -> cmp:('a -> 'a -> int) -> bool
val fold : 'a t -> 's -> f:('a -> 's -> 's) -> 's

val fold_opt : 'a t -> 's -> f:('a -> 's -> 's option) -> 's option
(** [fold_opt t s ~f] is a short-circuiting version of [fold] that runs in
    the [Option] monad. If [f] returns [None], [None] is returned without
    any additional invocations of [f]. *)

val fold_map : 'a t -> 's -> f:('a -> 's -> 'b * 's) -> 's * 'b t
val folding_map : 'a t -> 's -> f:('a -> 's -> 'b * 's) -> 'b t

val fold_until :
     'a t
  -> 's
  -> f:('a -> 's -> [`Continue of 's | `Stop of 'b])
  -> finish:('s -> 'b)
  -> 'b

val fold_result :
  'a t -> 's -> f:('a -> 's -> ('s, 'e) result) -> ('s, 'e) result
