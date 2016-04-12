(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Main module of InferClang. Take as input AST files produced by clang during compilation *)
(** and their corresponding C/C++/ObjectiveC source files. *)
(** Parse the arguments, parse and validate the input AST into a data structure *)
(** and translates it into a cfg. *)


val do_run : string -> string option -> unit
