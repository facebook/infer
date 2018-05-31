(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** module for escaping clang arguments on the command line and put them into files *)

open! IStd

(** quoting style of the arguments *)
type style =
  | EscapedDoubleQuotes
      (** the arguments should be enclosed in "double quotes" and are already escaped *)
  | SingleQuotes  (** the arguments should be enclosed in 'single quotes' and have to be escaped *)
  | EscapedNoQuotes  (** the arguments should not be enclosed in quotes and are already escaped *)

val quote : style -> string -> string

val mk_arg_file : string -> style -> string list -> string
