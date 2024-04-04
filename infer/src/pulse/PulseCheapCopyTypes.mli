(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_known_cheap_copy : Tenv.t -> Typ.name -> bool
(** Check if a struct name is known to be a cheap copy for unnecessary copy checker. *)
