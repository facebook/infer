(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type 'a singleton_or_more = Empty | Singleton of 'a | More

val singleton_or_more :
  fold:('t, 'a, 'a singleton_or_more) Container.fold -> 't -> 'a singleton_or_more

val mem_nth : fold:('t, _, int) Container.fold -> 't -> int -> bool

val forto : (int, int, 'accum) Container.fold

val to_rev_list : fold:('t, 'a, 'a list) Container.fold -> 't -> 'a list

val rev_filter_to_list : fold:('t, 'a, 'a list) Container.fold -> 't -> f:('a -> bool) -> 'a list

val rev_map_to_list : fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b) -> 'b list

val rev_filter_map_to_list :
  fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b option) -> 'b list

val iter_consecutive :
  fold:('t, 'a, 'a option) Container.fold -> 't -> f:('a -> 'a -> unit) -> unit

val pp_collection :
  fold:('t, 'a, 'a option) Container.fold -> pp_item:(F.formatter -> 'a -> unit) -> F.formatter
  -> 't -> unit

val pp_seq :
  fold:('t, 'a, bool) Container.fold -> sep:string -> (F.formatter -> 'a -> unit) -> F.formatter
  -> 't -> unit
  [@@warning "-32"]
