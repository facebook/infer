(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Database of analysis results *)

(** {2 Filename} *)

(** generic file name *)
type filename [@@deriving compare]

val filename_from_string : string -> filename

val filename_to_string : filename -> string

val filename_add_suffix : filename -> string -> filename

val file_exists : filename -> bool

val file_modified_time : ?symlink:bool -> filename -> float
(** Return the time when a file was last modified. The file must exist. *)

(** {2 Results Directory} *)

module Results_dir : sig
  (** path expressed as a list of strings *)
  type path = string list

  (** kind of path: specifies how to interpret a path *)
  type path_kind =
    | Abs_root  (** absolute path implicitly rooted at the root of the results dir *)
    | Abs_source_dir of SourceFile.t
        (** absolute path implicitly rooted at the source directory for the file *)
    | Rel  (** relative path *)

  val path_to_filename : path_kind -> path -> filename
  (** convert a path to a filename *)

  val specs_dir : filename
  (** directory of spec files *)

  val init : SourceFile.t -> unit
  (** Initialize the results directory *)

  val clean_specs_dir : unit -> unit
  (** Clean up specs directory *)

  val create_file : path_kind -> path -> Unix.File_descr.t
  (** create a file at the given path, creating any missing directories *)
end

val append_crc_cutoff : ?key:string -> ?crc_only:bool -> string -> string
(** Append a crc to the string, using string_crc_hex32.
    Cut the string if it exceeds the cutoff limit.
    Use an optional key to compute the crc.
    Return only the crc if [crc_only] is true.  *)

val source_file_encoding : SourceFile.t -> string
(** string encoding of a source file (including path) as a single filename *)

(** {2 Source Dirs} *)

(** source directory: the directory inside the results dir corresponding to a source file *)
type source_dir [@@deriving compare]

val source_dir_get_internal_file : source_dir -> string -> filename
(** get the path to an internal file with the given extention (.tenv, ...) *)

val source_dir_from_source_file : SourceFile.t -> source_dir
(** get the source directory corresponding to a source file *)

val is_source_file : string -> bool
(** Check if a path is a Java, C, C++ or Objectve C source file according to the file extention *)

val fold_paths_matching :
  dir:filename -> p:(filename -> bool) -> init:'a -> f:(filename -> 'a -> 'a) -> 'a
(** Fold over all file paths recursively under [dir] which match [p]. *)

val paths_matching : string -> (string -> bool) -> string list
(** Return all file paths recursively under the given directory which match the given predicate *)
