(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** This module handles buckets of memory leaks in Objective-C/C++ *)

let objc_arc_flag = "objc_arc"

let bucket_to_message bucket =
  match bucket with
  | `MLeak_cf -> "[CF]"
  | `MLeak_arc -> "[ARC]"
  | `MLeak_no_arc -> "[NO ARC]"
  | `MLeak_cpp -> "[CPP]"
  | `MLeak_unknown -> "[UNKNOWN ORIGIN]"

let contains_all =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_all

let contains_cf =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_cf

let contains_arc =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_arc

let contains_narc =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_no_arc

let contains_cpp =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_cpp

let contains_unknown_origin =
  List.mem ~equal:PVariant.(=) Config.ml_buckets `MLeak_unknown

let should_raise_leak_cf typ =
  if contains_cf then
    Objc_models.is_core_lib_type typ
  else false

let should_raise_leak_arc () =
  if contains_arc then
    !Config.arc_mode
  else false

let should_raise_leak_no_arc () =
  if contains_narc then
    not (!Config.arc_mode)
  else false

let should_raise_leak_unknown_origin =
  contains_unknown_origin

let ml_bucket_unknown_origin =
  bucket_to_message `MLeak_unknown

(* Returns whether a memory leak should be raised for a C++ object.*)
(* If ml_buckets contains cpp, then check leaks from C++ objects. *)
let should_raise_cpp_leak =
  if contains_cpp then
    Some (bucket_to_message `MLeak_cpp)
  else None

(* Returns whether a memory leak should be raised. *)
(* If cf is passed, then check leaks from Core Foundation. *)
(* If arc is passed, check leaks from code that compiles with arc*)
(* If no arc is passed check the leaks from code that compiles without arc *)
let should_raise_objc_leak typ =
  if List.is_empty Config.ml_buckets || contains_all then Some ""
  else if should_raise_leak_cf typ then Some (bucket_to_message `MLeak_cf)
  else if should_raise_leak_arc () then Some (bucket_to_message `MLeak_arc)
  else if should_raise_leak_no_arc () then Some (bucket_to_message `MLeak_no_arc)
  else None

(*
let bucket_to_string bucket =
  match bucket with
  | MLeak_cf -> "Core Foundation"
  | MLeak_arc -> "Arc"
  | MLeak_no_arc -> "No arc"
  | MLeak_cpp -> "Cpp"
  | MLeak_unknown -> "Unknown origin"
*)
