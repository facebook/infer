(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This module models special c struct types from the Apple's Core Foundation libraries
    for which there are particular rules for memory management. *)

val is_core_lib_type : Typ.t -> bool

val is_malloc_model : Typ.t -> Typ.Procname.t -> bool
