(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

[@@@warning "-unused-value-declaration"]

(** Extension of [Base.Container], i.e. generic definitions of container operations in terms of a
    [fold] function. *)

type 'a singleton_or_more = Empty | Singleton of 'a | More

val singleton_or_more :
  fold:('t, 'a, 'a singleton_or_more) Container.fold -> 't -> 'a singleton_or_more

val mem_nth : fold:('t, _, int) Container.fold -> 't -> int -> bool

val forto : (int, int, 'accum) Container.fold

val forto_right : (int, int, 'accum) Container.fold

val to_rev_list : fold:('t, 'a, 'a list) Container.fold -> 't -> 'a list

val rev_filter_to_list : fold:('t, 'a, 'a list) Container.fold -> 't -> f:('a -> bool) -> 'a list

val rev_map_to_list : fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b) -> 'b list

val rev_filter_map_to_list :
  fold:('t, 'a, 'b list) Container.fold -> 't -> f:('a -> 'b option) -> 'b list

val iter_consecutive : fold:('t, 'a, 'a option) Container.fold -> 't -> f:('a -> 'a -> unit) -> unit

val pp_collection :
     ?hov:bool
  -> fold:('t, 'a, 'a option) Container.fold
  -> pp_item:(F.formatter -> 'a -> unit)
  -> F.formatter
  -> 't
  -> unit

val filter :
  fold:('t, 'a, 'accum) Container.fold -> filter:('a -> bool) -> ('t, 'a, 'accum) Container.fold

val map : f:('a -> 'b) -> ('t, 'a, 'accum) Container.fold -> ('t, 'b, 'accum) Container.fold

val fold_of_pervasives_set_fold :
  (('elt -> 'accum -> 'accum) -> 't -> 'accum -> 'accum) -> ('t, 'elt, 'accum) Container.fold

val fold_of_pervasives_map_fold :
     (('key -> 'value -> 'accum -> 'accum) -> 't -> 'accum -> 'accum)
  -> ('t, 'key * 'value, 'accum) Container.fold

val iter_result :
  fold:('t, 'a, unit) Container.fold -> 't -> f:('a -> (unit, 'err) result) -> (unit, 'err) result

val fold_result_until :
     fold:('t, 'a, 'accum) Container.fold
  -> init:'accum
  -> f:('accum -> 'a -> (('accum, 'err) Result.t, 'final) Continue_or_stop.t)
  -> finish:('accum -> 'final)
  -> 't
  -> ('final, 'err) Result.t
