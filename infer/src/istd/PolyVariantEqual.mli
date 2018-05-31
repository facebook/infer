(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

(** Open to bring equality [(=)] for polymorphic variants into scope *)

val ( = ) : ([> ] as 'a) -> 'a -> bool
(** Equality for polymorphic variants *)
