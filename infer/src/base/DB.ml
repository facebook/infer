(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Database of analysis results *)

module L = Logging

let append_crc_cutoff ?(key = "") name =
  let cutoff_length = 50 - 16 (* crc *) - 1 (* separator *) in
  let name_up_to_cutoff =
    if String.length name <= cutoff_length then name else String.sub name ~pos:0 ~len:cutoff_length
  in
  let crc_str =
    let name_for_crc = name ^ key in
    String.sub (Utils.string_crc_hex32 name_for_crc) ~pos:0 ~len:16
  in
  Printf.sprintf "%s.%s" name_up_to_cutoff crc_str


let curr_source_file_encoding = `Enc_crc

(** string encoding of a source file (including path) as a single filename *)
let source_file_encoding source_file =
  let source_file_s = SourceFile.to_string source_file in
  match curr_source_file_encoding with
  | `Enc_base ->
      Filename.basename source_file_s
  | `Enc_path_with_underscores ->
      Escape.escape_path source_file_s
  | `Enc_crc ->
      let base = Filename.basename source_file_s in
      let dir = Filename.dirname source_file_s in
      append_crc_cutoff ~key:dir base


(** {2 Source Dirs} *)

(** source directory: the directory inside the results dir corresponding to a source file *)
type source_dir = string [@@deriving compare]

(** expose the source dir as a string *)
let source_dir_to_string source_dir = source_dir

(** get the path to an internal file with the given extention (.tenv, ...) *)
let source_dir_get_internal_file source_dir extension =
  let source_dir_name =
    append_crc_cutoff (Caml.Filename.remove_extension (Filename.basename source_dir))
  in
  let fname = source_dir_name ^ extension in
  Filename.concat source_dir fname


(** get the source directory corresponding to a source file *)
let source_dir_from_source_file source_file =
  ResultsDir.get_path Debug ^/ source_file_encoding source_file


(** {2 Filename} *)

type filename = string [@@deriving compare]

let filename_to_string s = s

let filename_from_string s = s

let filename_add_suffix fn s = fn ^ s

let file_exists path = ISys.file_exists path

(** Return the time when a file was last modified. The file must exist. *)
let file_modified_time ?(symlink = false) fname =
  try
    let stat = (if symlink then Unix.lstat else Unix.stat) fname in
    stat.Unix.st_mtime
  with Unix.Unix_error _ -> L.(die InternalError) "File %s does not exist." fname


(** {2 Results Directory} *)

module Results_dir = struct
  (** path expressed as a list of strings *)
  type path = string list

  (** kind of path: specifies how to interpret a path *)
  type path_kind =
    | Abs_root  (** absolute path implicitly rooted at the root of the results dir *)
    | Abs_source_dir of SourceFile.t
        (** absolute path implicitly rooted at the source directory for the file *)
    | Rel  (** relative path *)

  let filename_from_base base path =
    let rec f = function
      | [] ->
          base
      | name :: names ->
          Filename.concat (f names)
            (if String.equal name ".." then Filename.parent_dir_name else name)
    in
    f (List.rev path)


  (** convert a path to a filename *)
  let path_to_filename pk path =
    let base =
      match pk with
      | Abs_root ->
          Config.results_dir
      | Abs_source_dir source ->
          let dir = source_dir_from_source_file source in
          source_dir_to_string dir
      | Rel ->
          Filename.current_dir_name
    in
    filename_from_base base path


  (** initialize the results directory *)
  let init ?(debug = false) source =
    if SourceFile.is_invalid source then L.(die InternalError) "Invalid source file passed" ;
    if debug || Config.html || Config.debug_mode || Config.frontend_tests then (
      Utils.create_dir (ResultsDir.get_path Debug) ;
      Utils.create_dir (path_to_filename (Abs_source_dir source) []) )


  (** create a file at the given path, creating any missing directories *)
  let create_file pk path =
    let rec create = function
      | [] ->
          let fname = path_to_filename pk [] in
          Utils.create_dir fname ;
          fname
      | name :: names ->
          let new_path = Filename.concat (create names) name in
          Utils.create_dir new_path ;
          new_path
    in
    let filename, dir_path =
      match List.rev path with
      | filename :: dir_path ->
          (filename, dir_path)
      | [] ->
          L.(die InternalError) "create_path"
    in
    let full_fname = Filename.concat (create dir_path) filename in
    Unix.openfile full_fname ~mode:Unix.[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o777
end
