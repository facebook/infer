(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type none

type some

type (_, _) t = GNone : (none, _) t | GSome : 'a -> (some, 'a) t

let value : (some, 'a) t -> 'a = function GSome v -> v

let value_map (type h) (t : (h, _) t) ~default ~f = match t with GNone -> default | GSome v -> f v

let value_map_f (type h) (t : (h, _) t) ~default ~f =
  match t with GNone -> default () | GSome v -> f v
