(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Location in the original source file *)
type t =
  { line: int  (** The line number. -1 means "do not know" *)
  ; col: int  (** The column number. -1 means "do not know" *)
  ; file: SourceFile.t  (** The name of the source file *) }
  [@@deriving compare]

val equal : t -> t -> bool

val none : SourceFile.t -> t
(** Dummy source location for the given file *)

val dummy : t
(** Dummy location with no source file *)

val pp : Format.formatter -> t -> unit
(** Pretty print a location. *)

val to_string : t -> string
(** String representation of a location. *)

val pp_file_pos : Format.formatter -> t -> unit
(** Pretty print a file-position of a location *)
