(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type 'a singleton_or_more = Empty | Singleton of 'a | More

val singleton_or_more :
  fold:('t, 'a, 'a singleton_or_more) Container.fold -> 't -> 'a singleton_or_more

val mem_nth : fold:('t, _, int) Container.fold -> 't -> int -> bool

val forto : (int, int, 'accum) Container.fold

val rev_map_to_list : fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b) -> 'b list

val rev_filter_map_to_list :
  fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b option) -> 'b list
