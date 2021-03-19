(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Escape a string, eg replace reserved characters with escape sequences *)

val escape_dotty : string -> string
(** escape a string to be used in a dotty file *)

val escape_path : string -> string
(** escape a path replacing the directory separator with an underscore *)

val escape_xml : string -> string
(** escape a string to be used in an xml file *)

val escape_url : string -> string

val escape_json : string -> string
(** escape characters in the string so it becomes a valid JSON string *)

val escape_double_quotes : string -> string
(** replaces double-quote with backslash double-quote *)

val escape_in_single_quotes : string -> string
(** put the string inside single quotes and escape the single quotes within that string *)

val escape_shell : string -> string
(** escape the string so it can be passed to the shell without remorse *)

val escape_filename : string -> string
(** escape the characters that cannot be used in filenames *)
