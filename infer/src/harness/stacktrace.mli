(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for parsing stack traces and using them to guide Infer analysis *)

type stack_trace

(** create an Infer-readable representation of a stack trace given its raw text *)
val parse_stack_trace : string -> Exe_env.t -> stack_trace
