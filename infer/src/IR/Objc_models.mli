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

module Core_foundation_model : sig
  val is_core_lib_release : string -> string -> bool

  val is_core_lib_create : Typ.t -> string -> bool

  val is_core_lib_retain : string -> string -> bool

  val is_core_graphics_release : string -> string -> bool

  val is_objc_memory_model_controlled : string -> bool
end

val is_core_lib_type : Typ.t -> bool
