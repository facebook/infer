(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** This module handles buckets of memory leaks in Objective-C *)

val objc_arc_flag : string

val init_buckets : string -> unit

(* Returns whether a memory leak should be raised.  *)
(* If cf is passed, then check leaks from Core Foundation. *)
(* If arc is passed, check leaks from code that compiles with arc*)
(* If no arc is passed check the leaks from code that compiles without arc *)
val should_raise_objc_leak : Sil.typ -> string option

(* Returns whether a memory leak should be raised for a C++ object.*)
(* If ml_buckets contains cpp, then check leaks from C++ objects. *)
val should_raise_cpp_leak : unit -> string option

val should_raise_leak_unknown_origin : unit -> bool

val ml_bucket_unknown_origin : unit -> string
