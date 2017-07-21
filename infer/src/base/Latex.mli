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

type style = Boldface | Roman | Italics

val convert_string : string -> string
(** Convert a string to a latex-friendly format *)

val pp_string : style -> Format.formatter -> string -> unit
(** Print a string in the given style, after converting it into latex-friendly format *)

val pp_color : Format.formatter -> Pp.color -> unit
(** Print color command *)

val pp_begin : Format.formatter -> string * string * bool -> unit
(** Prelude for a latex file with the given author and title and table of contents flag *)

val pp_end : Format.formatter -> unit -> unit
(** Epilogue for a latex file *)

val pp_section : Format.formatter -> string -> unit
(** Section with the given title *)
