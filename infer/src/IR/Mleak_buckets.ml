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
open PolyVariantEqual

(** This module handles buckets of memory leaks in Objective-C/C++ *)

let bucket_to_message bucket =
  match bucket with `MLeak_cpp -> "[CPP]" | `MLeak_unknown -> "[UNKNOWN ORIGIN]"


let contains_cpp = List.mem ~equal:( = ) Config.ml_buckets `MLeak_cpp

let contains_unknown_origin = List.mem ~equal:( = ) Config.ml_buckets `MLeak_unknown

let should_raise_leak_unknown_origin = contains_unknown_origin

let ml_bucket_unknown_origin = bucket_to_message `MLeak_unknown

(* Returns whether a memory leak should be raised for a C++ object.*)
(* If ml_buckets contains cpp, then check leaks from C++ objects. *)
let should_raise_cpp_leak = if contains_cpp then Some (bucket_to_message `MLeak_cpp) else None
