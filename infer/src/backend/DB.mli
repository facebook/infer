(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Database of analysis results *)

(** {2 Filename} *)

(** generic file name *)
type filename

module FilenameSet : Set.S with type elt = filename
module FilenameMap : Map.S with type key = filename

val filename_from_string : string -> filename
val filename_to_string : filename -> string
val filename_compare : filename -> filename -> int
val chop_extension : filename -> filename
val filename_concat : filename -> string -> filename
val filename_add_suffix : filename -> string -> filename
val file_exists : filename -> bool
val file_remove : filename -> unit

(** Return the time when a file was last modified. The file must exist. *)
val file_modified_time : ?symlink:bool -> filename -> float

(** Return whether filename was updated after analysis started. File doesn't have to exist *)
val file_was_updated_after_start : filename -> bool

(** {2 Results Directory} *)

module Results_dir : sig
  (** path expressed as a list of strings *)
  type path = string list

  (** kind of path: specifies how to interpret a path *)
  type path_kind =
    | Abs_root (** absolute path implicitly rooted at the root of the results dir *)
    | Abs_source_dir (** absolute path implicitly rooted at the source directory for the current file *)
    | Rel (** relative path *)

  (** convert a path to a filename *)
  val path_to_filename : path_kind -> path -> filename

  (** directory of spec files *)
  val specs_dir : filename

  (** Initialize the results directory *)
  val init : unit -> unit

  (** Clean up specs directory *)
  val clean_specs_dir : unit -> unit

  (** create a file at the given path, creating any missing directories *)
  val create_file : path_kind -> path -> Unix.file_descr
end

(** origin of a analysis artifact: current results dir, a spec library, or models *)
type origin =
  | Res_dir
  | Spec_lib
  | Models

(** {2 Source Files} *)

type source_file

(** Maps from source_file *)
module SourceFileMap : Map.S with type key = source_file

(** Set of source files *)
module SourceFileSet : Set.S with type elt = source_file

(** current source file *)
val current_source : source_file ref

(** comparison of source files *)
val source_file_compare : source_file -> source_file -> int

(** equality of source files *)
val source_file_equal : source_file -> source_file -> bool

(** empty source file *)
val source_file_empty : source_file

(** convert a path to a source file, turning it into an absolute path if necessary *)
val abs_source_file_from_path : string -> source_file

(** convert a project root directory and an absolute path to a source file *)
val rel_source_file_from_abs_path : string -> string -> source_file

(** string encoding of a source file (including path) as a single filename *)
val source_file_encoding : source_file -> string

(** convert a source file to a string *)
val source_file_to_string : source_file -> string

(** convert a string obtained by source_file_to_string to a source file *)
val source_file_from_string : string -> source_file

(** get the full path of a source file, raise No_project_root exception when used with a relative source file and no project root specified *)
val source_file_to_abs_path : source_file -> string

(** get the relative path of a source file *)
val source_file_to_rel_path : source_file -> string

(** {2 Source Dirs} *)

(** source directory: the directory inside the results dir corresponding to a source file *)
type source_dir

val source_dir_compare : source_dir -> source_dir -> int

(** get the absolute path to the sources dir *)
val sources_dir : string

(** expose the source dir as a string *)
val source_dir_to_string : source_dir -> string

(** get the path to an internal file with the given extention (.cfg, .cg, .tenv) *)
val source_dir_get_internal_file : source_dir -> string -> filename

(** get the source directory corresponding to a source file *)
val source_dir_from_source_file : source_file -> source_dir

(** get the path to the copy of the source file to be stored in the results directory *)
val source_file_in_resdir : source_file -> filename

(** directory where the results of the capture phase are stored *)
val captured_dir : filename

(** create the directory containing the file bane *)
val filename_create_dir : filename -> unit

(** Find the source directories in the current results dir *)
val find_source_dirs : unit -> source_dir list

(** create a directory if it does not exist already *)
val create_dir : string -> unit

(** [make_directory path] will create a directory [path] creating all the
    sub directories if non-existing *)
val create_path : string -> unit

(** Read a file using a lock to allow write attempts in parallel. *)
val read_file_with_lock : string -> string -> bytes option

(** Update the file contents with the update function provided.
    If the directory does not exist, it is created.
    If the file does not exist, it is created, and update is given the empty string.
    A lock is used to allow write attempts in parallel. *)
val update_file_with_lock : string -> string -> (bytes -> bytes) -> unit

(** get the path of the global type environment (only used in Java) *)
val global_tenv_fname : filename

(** Check if a path is a Java, C, C++ or Objectve C source file according to the file extention *)
val is_source_file: string -> bool

(** Returns true if the file is a C++ model *)
val file_is_in_cpp_model : string -> bool

(** Fold over all file paths recursively under [dir] which match [p]. *)
val fold_paths_matching :
  dir:filename -> p:(filename -> bool) -> init:'a -> f:(filename -> 'a -> 'a) -> 'a

(** Return all file paths recursively under the given directory which match the given predicate *)
val paths_matching : string -> (string -> bool) -> string list

val read_changed_files_index : string list option
