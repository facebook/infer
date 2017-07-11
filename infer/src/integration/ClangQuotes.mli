(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* module for escaping clang arguments on the command line and put them into files *)

(** quoting style of the arguments *)

type style =
  | EscapedDoubleQuotes
      (** the arguments should be enclosed in "double quotes" and are already escaped *)
  | SingleQuotes  (** the arguments should be enclosed in 'single quotes' and have to be escaped *)
  | EscapedNoQuotes  (** the arguments should not be enclosed in quotes and are already escaped *)

val quote : style -> string -> string

val mk_arg_file : string -> style -> string list -> string
