(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** This module models special c struct types from the Apple's Core Foundation libraries
for which there are particular rules for memory management. *)

open Utils

(** This module models special c struct types from the Apple's Core Foundation libraries
for which there are particular rules for memory management. *)

module Core_foundation_model :
sig

  val is_core_lib_release : string -> string -> bool

  val is_core_lib_create : Sil.typ -> string -> bool

  val is_core_lib_retain : string -> string -> bool

  val is_core_graphics_release : string -> string -> bool

  val is_objc_memory_model_controlled : string -> bool


end

val is_core_lib_type : Sil.typ -> bool
