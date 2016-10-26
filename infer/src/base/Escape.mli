(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Escape a string for use in a CSV or XML file: replace reserved
    characters with escape sequences *)

(** escape a string specifying the per character escaping function *)
val escape_map : (char -> string option) -> string -> string

(** escape a string to be used in a dotty file *)
val escape_dotty : string -> string

(** escape a string to be used in a csv file *)
val escape_csv : string -> string

(** escape a path replacing the directory separator with an underscore *)
val escape_path : string -> string

(** escape a string to be used in an xml file *)
val escape_xml : string -> string

(** escape a string to be used as a file name *)
val escape_filename : string -> string
