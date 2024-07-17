(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Location in the original source file *)
type t =
  { file: SourceFile.t  (** The name of the source file *)
  ; line: int  (** The line number. -1 means "do not know" *)
  ; col: int  (** The column number. -1 means "do not know" *)
  ; macro_file_opt: SourceFile.t option
        (** If the location is coming from macro expansion, the name of the file macro is defined in *)
  ; macro_line: int  (** If the location is coming from macro expansion, the line number *) }
[@@deriving compare, sexp_of, sexp, hash, normalize]

val get_macro_file_line_opt : t -> (SourceFile.t * int) option

val equal : t -> t -> bool

val none : SourceFile.t -> t
(** Dummy source location for the given file *)

val dummy : t
(** Dummy location with no source file *)

val pp : Format.formatter -> t -> unit
(** Pretty print a location. *)

val pp_line : Format.formatter -> t -> unit
(** print just the line information *)

val pp_file_pos : Format.formatter -> t -> unit
(** Pretty print a file-position of a location *)

val pp_range : Format.formatter -> t * t -> unit

module Map : PrettyPrintable.PPMap with type key = t
