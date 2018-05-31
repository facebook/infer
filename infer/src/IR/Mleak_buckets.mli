(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This module handles buckets of memory leaks *)

(* Returns whether a memory leak should be raised for a C++ object.*)
(* If ml_buckets contains cpp, then check leaks from C++ objects. *)

val should_raise_cpp_leak : string option

val should_raise_leak_unknown_origin : bool

val ml_bucket_unknown_origin : string
