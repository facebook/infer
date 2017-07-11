(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** This module handles buckets of memory leaks in Objective-C *)

val objc_arc_flag : string

(* Returns whether a memory leak should be raised.  *)
(* If cf is passed, then check leaks from Core Foundation. *)
(* If arc is passed, check leaks from code that compiles with arc*)
(* If no arc is passed check the leaks from code that compiles without arc *)

val should_raise_objc_leak : Typ.t -> string option

(* Returns whether a memory leak should be raised for a C++ object.*)
(* If ml_buckets contains cpp, then check leaks from C++ objects. *)

val should_raise_cpp_leak : string option

val should_raise_leak_unknown_origin : bool

val ml_bucket_unknown_origin : string
