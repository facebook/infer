(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type t [@@deriving compare]

(** Maps from source_file *)
module Map : Map.S with type key = t

(** Set of source files *)
module Set : Set.S with type elt = t

module UNSAFE : sig
  (** Create a SourceFile from any path. This is unchecked and should not be
      used when the existence of source files is a requirement. Furthermore,
      absolute paths won't be made relative to project root.*)
  val from_string : string -> t
end

(** Is the source file the invalid source file? *)
val is_invalid : t -> bool

(** Set of files read from --changed-files-index file, None if option not specified
    NOTE: it may include extra source_files if --changed-files-index contains paths to
          header files *)
val changed_files_set : Set.t option

(** Invalid source file *)
val invalid : string -> t

(** equality of source files *)
val equal : t -> t -> bool

(** create source file from absolute path *)
val from_abs_path : string -> t

(* Create a SourceFile from a given path. If relative, it assumes it is w.r.t.
   project root *)
val create : string -> t

(** Returns true if the file is a C++ model *)
val is_cpp_model : t -> bool

val is_infer_model : t -> bool

(** Returns true if the file is in project root *)
val is_under_project_root : t -> bool

(** compute line count of a source file *)
val line_count : t -> int

(** Return approximate source file corresponding to the parameter if it's header file and
    file exists. returns None otherwise *)
val of_header : t -> t option

(** pretty print t *)
val pp : Format.formatter -> t -> unit

(** get the full path of a source file *)
val to_abs_path : t -> string

(** get the relative path of a source file *)
val to_rel_path : t -> string

(** convert a source file to a string
    WARNING: result may not be valid file path, do not use this function to perform operations
             on filenames *)
val to_string : t -> string
