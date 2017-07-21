(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t [@@deriving compare]

(** Maps from source_file *)
module Map : Caml.Map.S with type key = t

(** Set of source files *)
module Set : Caml.Set.S with type elt = t

module UNSAFE : sig
  val from_string : string -> t
  (** Create a SourceFile from any path. This is unchecked and should not be
      used when the existence of source files is a requirement. Furthermore,
      absolute paths won't be made relative to project root.*)
end

val is_invalid : t -> bool
(** Is the source file the invalid source file? *)

val changed_sources_from_changed_files : string list -> Set.t
(** Set of files read from --changed-files-index file, None if option not specified
    NOTE: it may include extra source_files if --changed-files-index contains paths to
          header files *)

val invalid : string -> t
(** Invalid source file *)

val equal : t -> t -> bool
(** equality of source files *)

val from_abs_path : ?warn_on_error:bool -> string -> t
(** create source file from absolute path.
    WARNING: If warn_on_error is false, no warning will be shown whenever an error occurs for
    the given path (e.g. if it does not exist). *)

(* Create a SourceFile from a given path. If relative, it assumes it is w.r.t. project root.
   WARNING: If warn_on_error is false, no warning will be shown whenever an error occurs for
   the given path (e.g. if it does not exist). *)

val create : ?warn_on_error:bool -> string -> t

val is_cpp_model : t -> bool
(** Returns true if the file is a C++ model *)

val is_infer_model : t -> bool

val is_under_project_root : t -> bool
(** Returns true if the file is in project root *)

val line_count : t -> int
(** compute line count of a source file *)

val of_header : ?warn_on_error:bool -> t -> t option
(** Return approximate source file corresponding to the parameter if it's header file and
    file exists. returns None otherwise.
    WARNING: If warn_on_error is false, no warning will be shown whenever an error occurs for
    the given SourceFile (e.g. if it does not exist).*)

val pp : Format.formatter -> t -> unit
(** pretty print t *)

val to_abs_path : t -> string
(** get the full path of a source file *)

val to_rel_path : t -> string
(** get the relative path of a source file *)

val to_string : t -> string
(** convert a source file to a string
    WARNING: result may not be valid file path, do not use this function to perform operations
             on filenames *)
