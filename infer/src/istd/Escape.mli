(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Escape a string for use in a CSV or XML file: replace reserved
    characters with escape sequences *)

val escape_dotty : string -> string
(** escape a string to be used in a dotty file *)

val escape_csv : string -> string
(** escape a string to be used in a csv file *)

val escape_path : string -> string
(** escape a path replacing the directory separator with an underscore *)

val escape_xml : string -> string
(** escape a string to be used in an xml file *)

val escape_url : string -> string

val escape_filename : string -> string
(** escape a string to be used as a file name *)

val escape_json : string -> string
(** escape characters in the string so it becomes a valid JSON string *)

val escape_double_quotes : string -> string
(** replaces double-quote with backslash double-quote *)

val escape_in_single_quotes : string -> string
(** put the string inside single quotes and escape the single quotes within that string *)

val escape_shell : string -> string
(** escape the string so it can be passed to the shell without remorse *)
