(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This module handles buckets of memory leaks in Objective-C/C++ *)

let bucket_to_message bucket =
  match bucket with `MLeak_cpp -> "[CPP]" | `MLeak_unknown -> "[UNKNOWN ORIGIN]"


let ml_bucket_unknown_origin = bucket_to_message `MLeak_unknown
