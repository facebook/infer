(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Database of analysis results *)

module F = Format
module L = Logging

(** {2 Source Files} *)

type source_file =
  | Absolute of string
  | Relative of string

let source_file_compare sf1 sf2 =
  match sf1, sf2 with
  | Absolute p1, Absolute p2 -> string_compare p1 p2
  | Absolute _, _ -> -1
  | _, Absolute _ -> 1
  | Relative p1, Relative p2 -> string_compare p1 p2

let source_file_equal sf1 sf2 =
  (source_file_compare sf1 sf2) = 0

module OrderedSourceFile =
struct
  type t = source_file
  let compare = source_file_compare
end

module SourceFileMap = Map.Make(OrderedSourceFile)

module SourceFileSet = Set.Make(OrderedSourceFile)

let source_file_from_string path =
  if Filename.is_relative path then
    Relative path
  else
    Absolute path

(** convert a path to a source file, turning it into an absolute path if necessary *)
let abs_source_file_from_path fname =
  Absolute (filename_to_absolute fname)

(** convert a project root directory and a full path to a rooted source file *)
let rel_source_file_from_abs_path root fname =
  let relative_complemented_fname = filename_to_relative root fname in
  if string_is_prefix root fname &&
     Filename.is_relative relative_complemented_fname then
    Relative relative_complemented_fname
  else
    (* The project root is not a prefix of the file name *)
    abs_source_file_from_path fname

let curr_encoding = `Enc_crc

let source_file_to_string fname =
  match fname with
  | Relative path
  | Absolute path -> path

exception No_project_root

let project_root () =
  match !Config.project_root with
  | None -> L.err "No -project_root option passed@."; raise No_project_root
  | Some path -> path

(* Checking if the path exists may be needed only in some cases, hence the flag check_exists *)
let source_file_to_abs_path fname =
  match fname with
  | Relative path -> Filename.concat (project_root()) path
  | Absolute path -> path

let source_file_to_rel_path fname =
  match fname with
  | Relative path -> path
  | Absolute path -> filename_to_relative (project_root ()) path

(** string encoding of a source file (including path) as a single filename *)
let source_file_encoding source_file =
  let source_file_s = source_file_to_string source_file in
  match curr_encoding with
  | `Enc_base ->
      Filename.basename source_file_s
  | `Enc_path_with_underscores ->
      Escape.escape_path source_file_s
  | `Enc_crc ->
      let base = Filename.basename source_file_s in
      let dir = Filename.dirname source_file_s in
      string_append_crc_cutoff ~key:dir base

let source_file_empty = Absolute ""

(** current source file *)
let current_source = ref source_file_empty

(** {2 Source Dirs} *)

(** source directory: the directory inside the results dir corresponding to a source file *)
type source_dir = string

let source_dir_compare = string_compare

(** expose the source dir as a string *)
let source_dir_to_string source_dir = source_dir

(** get the path to an internal file with the given extention (.cfg, .cg, .tenv) *)
let source_dir_get_internal_file source_dir extension =
  let source_dir_name =
    string_append_crc_cutoff (Filename.chop_extension (Filename.basename source_dir)) in
  let fname = source_dir_name ^ extension in
  Filename.concat source_dir fname

let captured_dir () =
  Filename.concat !Config.results_dir Config.captured_dir_name

let sources_dir () =
  Filename.concat !Config.results_dir Config.sources_dir_name

(** get the source directory corresponding to a source file *)
let source_dir_from_source_file source_file =
  Filename.concat (captured_dir ()) (source_file_encoding source_file)

(** get the path to the copy of the source file to be stored in the results directory *)
let source_file_in_resdir source_file =
  Filename.concat (sources_dir ()) (source_file_encoding source_file)

(** Find the source directories in the results dir *)
let find_source_dirs () =
  let source_dirs = ref [] in
  let capt_dir = captured_dir () in
  let files_in_results_dir = Array.to_list (Sys.readdir capt_dir) in
  let add_cg_files_from_dir dir =
    let files = Array.to_list (Sys.readdir dir) in
    IList.iter (fun fname ->
        let path = Filename.concat dir fname in
        if Filename.check_suffix path ".cg" then source_dirs := dir :: !source_dirs)
      files in
  IList.iter (fun fname ->
      let dir = Filename.concat capt_dir fname in
      if Sys.is_directory dir then add_cg_files_from_dir dir)
    files_in_results_dir;
  IList.rev !source_dirs

(** {2 Filename} *)

type filename = string

let filename_concat = Filename.concat

let filename_to_string s = s

let filename_from_string s = s

let filename_compare = Pervasives.compare

let filename_add_suffix fn s = fn ^ s

let chop_extension = Filename.chop_extension

let file_exists = Sys.file_exists

let file_remove = Sys.remove

module FilenameSet = Set.Make(
  struct
    type t = filename
    let compare = filename_compare
  end)

module FilenameMap = Map.Make(
  struct
    type t = filename
    let compare = filename_compare
  end)

(** Return the time when a file was last modified. The file must exist. *)
let file_modified_time ?(symlink=false) fname =
  let stat = (if symlink then Unix.lstat else Unix.stat) fname in
  stat.Unix.st_mtime

(** Create a directory if it does not exist already. *)
let create_dir dir =
  try
    if (Unix.stat dir).Unix.st_kind != Unix.S_DIR then
      (L.err "@.ERROR: file %s exists and is not a directory@." dir;
       assert false)
  with Unix.Unix_error _ ->
    (try Unix.mkdir dir 0o700 with
       Unix.Unix_error _ ->
         let created_concurrently = (* check if another process created it meanwhile *)
           (Unix.stat dir).Unix.st_kind = Unix.S_DIR in
         if not created_concurrently then
           (L.err "@.ERROR: cannot create directory %s@." dir;
            assert false))

let filename_create_dir fname =
  let dirname = Filename.dirname fname in
  if not (Sys.file_exists dirname)
  then create_dir dirname

let read_whole_file fd =
  let stats = Unix.fstat fd in
  let size = stats.Unix.st_size in
  let buf = Bytes.create size in
  let nread = Unix.read fd buf 0 size in
  if nread != size then
    begin
      L.stderr "Error nread:%d size:%d@." nread size;
      assert false
    end;
  buf

(** Update the file contents with the update function provided.
    If the directory does not exist, it is created.
    If the file does not exist, it is created, and update is given the empty string.
    A lock is used to allow write attempts in parallel. *)
let update_file_with_lock dir fname update =
  let reset_file fd =
    let n = Unix.lseek fd 0 Unix.SEEK_SET in
    if n <> 0 then
      begin
        L.stderr "reset_file: lseek fail@.";
        assert false
      end in
  create_dir dir;
  let path = Filename.concat dir fname in
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_SYNC; Unix.O_RDWR] 0o640 in
  Unix.lockf fd Unix.F_LOCK 0;
  let buf = read_whole_file fd in
  reset_file fd;
  let str = update buf in
  let i = Unix.write fd str 0 (Bytes.length str) in
  if (i = (Bytes.length str))
  then (Unix.lockf fd Unix.F_ULOCK 0; Unix.close fd)
  else (L.err "@.save_with_lock: fail on path: %s@." path;
        assert false)

(** Read a file using a lock to allow write attempts in parallel. *)
let read_file_with_lock dir fname =
  let path = Filename.concat dir fname in
  try
    let fd = Unix.openfile path [Unix.O_RSYNC; Unix.O_RDONLY] 0o646 in
    try
      Unix.lockf fd Unix.F_RLOCK 0;
      let buf = read_whole_file fd in
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd;
      Some buf
    with Unix.Unix_error _ ->
      L.stderr "read_file_with_lock: Unix error";
      assert false
  with Unix.Unix_error _ -> None

(** {2 Results Directory} *)

module Results_dir = struct
  (** path expressed as a list of strings *)
  type path = string list

  (** kind of path: specifies how to interpret a path *)
  type path_kind =
    | Abs_root (** absolute path implicitly rooted at the root of the results dir *)
    | Abs_source_dir (** absolute path implicitly rooted at the source directory for the current file *)
    | Rel (** relative path *)

  let filename_from_base base path =
    let rec f = function
      | [] -> base
      | name:: names ->
          Filename.concat (f names) (if name ==".." then Filename.parent_dir_name else name) in
    f (IList.rev path)

  (** convert a path to a filename *)
  let path_to_filename pk path =
    let base = match pk with
      | Abs_root -> !Config.results_dir
      | Abs_source_dir ->
          let dir = source_dir_from_source_file !current_source in
          source_dir_to_string dir
      | Rel -> Filename.current_dir_name in
    filename_from_base base path

  (** directory of spec files *)
  let specs_dir () = path_to_filename Abs_root [Config.specs_dir_name]

  (** initialize the results directory *)
  let init () =
    create_dir !Config.results_dir;
    create_dir (specs_dir ());
    create_dir (path_to_filename Abs_root [Config.attributes_dir_name]);
    create_dir (path_to_filename Abs_root [Config.sources_dir_name]);
    create_dir (path_to_filename Abs_root [Config.captured_dir_name]);
    if not (source_file_equal !current_source source_file_empty) then
      create_dir (path_to_filename Abs_source_dir [])

  let clean_specs_dir () =
    let specs_dir_path = specs_dir () in
    create_dir specs_dir_path; (* create dir just in case it doesn't exist to avoid errors *)
    let files_to_remove = Array.map (Filename.concat specs_dir_path) (Sys.readdir specs_dir_path) in
    Array.iter Sys.remove files_to_remove

  (** create a file at the given path, creating any missing directories *)
  let create_file pk path =
    let rec create = function
      | [] ->
          let fname = path_to_filename pk [] in
          create_dir fname;
          fname
      | name:: names ->
          let new_path = Filename.concat (create names) name in
          create_dir new_path;
          new_path in
    let filename, dir_path = match IList.rev path with
      | filename:: dir_path -> filename, dir_path
      | [] -> raise (Failure "create_path") in
    let full_fname = Filename.concat (create dir_path) filename in
    Unix.openfile full_fname [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o777
end

let global_tenv_fname () =
  let basename = Config.global_tenv_filename in
  filename_concat (captured_dir ()) basename

let is_source_file path =
  IList.exists
    (fun ext -> Filename.check_suffix path ext)
    Config.source_file_extentions

let infer_start_time = lazy
  (file_modified_time (Results_dir.path_to_filename Results_dir.Abs_root [Config.start_filename]))

(** Return whether filename was updated after analysis started. File doesn't have to exist *)
let file_was_updated_after_start fname =
  if file_exists fname then
    let file_mtime = file_modified_time fname in
    file_mtime > Lazy.force infer_start_time
  else
    (* since file doesn't exist, it wasn't modified *)
    false
