(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** This module models special c struct types from the Apple's Core Foundation libraries
    for which there are particular rules for memory management. *)

val is_core_lib_type : Typ.t -> bool

val is_malloc_model : Typ.t -> Typ.Procname.t -> bool
