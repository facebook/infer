(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type none

type some

type (_, _) t = GNone : (none, _) t | GSome : 'a -> (some, 'a) t

val value : (some, 'a) t -> 'a

val value_map : (_, 'a) t -> default:'b -> f:('a -> 'b) -> 'b

val value_map_f : (_, 'a) t -> default:(unit -> 'b) -> f:('a -> 'b) -> 'b
(** Like [value_map] but the default value is evaluated lazily *)
