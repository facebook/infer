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

module F = Format
module L = Logging

(** {2 Source Files} *)

let count_newlines (path: string): int =
  let open Core.Std in
  let f file = In_channel.fold_lines file ~init:0 ~f:(fun i _ -> i + 1) in
  In_channel.with_file path ~f

type source_file =
  | Absolute of string
  | RelativeProjectRoot of string (* relative to project root *)
  | RelativeInferModel of string (* relative to infer models *)
[@@deriving compare]

let equal_source_file sf1 sf2 =
  compare_source_file sf1 sf2 = 0

module OrderedSourceFile =
struct
  type t = source_file [@@deriving compare]
end

module SourceFileMap = Map.Make(OrderedSourceFile)

module SourceFileSet = Set.Make(OrderedSourceFile)

let rel_path_from_abs_path root fname =
  let relative_complemented_fname = filename_to_relative root fname in
  if string_is_prefix root fname &&
     Filename.is_relative relative_complemented_fname then
    Some relative_complemented_fname
  else None (* The project root is not a prefix of the file name *)

let source_file_from_abs_path fname =
  if Filename.is_relative fname then
    (failwithf
       "ERROR: Path %s is relative, when absolute path was expected .@."
       fname);
  (* try to get realpath of source file. Use original if it fails *)
  let fname_real = try realpath fname with Unix.Unix_error _ -> fname in
  let project_root_real = realpath Config.project_root in
  let models_dir_real = Config.models_src_dir in
  match rel_path_from_abs_path project_root_real fname_real with
  | Some path -> RelativeProjectRoot path
  | None -> (
      match rel_path_from_abs_path models_dir_real fname_real with
      | Some path -> RelativeInferModel path
      | None -> Absolute fname (* fname is absolute already *)
    )

let curr_encoding = `Enc_crc

let source_file_to_string fname =
  match fname with
  | RelativeInferModel path -> "INFER_MODEL/" ^ path
  | RelativeProjectRoot path
  | Absolute path -> path

let source_file_pp fmt fname =
  Format.fprintf fmt "%s" (source_file_to_string fname)

(* Checking if the path exists may be needed only in some cases, hence the flag check_exists *)
let source_file_to_abs_path fname =
  match fname with
  | RelativeProjectRoot path -> Filename.concat Config.project_root path
  | RelativeInferModel path -> Filename.concat Config.models_src_dir path
  | Absolute path -> path

let source_file_line_count source_file =
  let abs_path = source_file_to_abs_path source_file in
  count_newlines abs_path

let source_file_to_rel_path fname =
  match fname with
  | RelativeProjectRoot path -> path
  | _ -> source_file_to_abs_path fname

(** string encoding of a source file (including path) as a single filename *)
let source_file_encoding source_file =
  let prefix = match source_file with
    | RelativeProjectRoot _ -> "P"
    | RelativeInferModel _ -> "MOD"
    | Absolute _ -> "ABS" in
  let source_file_s = source_file_to_string source_file in
  match curr_encoding with
  | `Enc_base ->
      Filename.basename source_file_s
  | `Enc_path_with_underscores ->
      prefix ^ Escape.escape_path source_file_s
  | `Enc_crc ->
      let base = Filename.basename source_file_s in
      let dir = prefix ^ Filename.dirname source_file_s in
      string_append_crc_cutoff ~key:dir base

let source_file_empty = Absolute ""

let source_file_is_infer_model source_file = match source_file with
  | RelativeProjectRoot _ | Absolute _ -> false
  | RelativeInferModel _ -> true

(** Returns true if the file is a C++ model *)
let source_file_is_cpp_model file =
  match file with
  | RelativeInferModel path ->
      string_is_prefix Config.relative_cpp_models_dir path
  | _ -> false

let source_file_is_under_project_root = function
  | RelativeProjectRoot _ -> true
  | Absolute _ | RelativeInferModel _ -> false

let source_file_exists_cache = Hashtbl.create 256

let source_file_path_exists abs_path =
  try Hashtbl.find source_file_exists_cache abs_path
  with Not_found ->
    let result = Sys.file_exists  abs_path in
    Hashtbl.add source_file_exists_cache abs_path result;
    result

let source_file_of_header header_file =
  let abs_path = source_file_to_abs_path header_file in
  let source_file_exts = ["c"; "cc"; "cpp"; "cxx"; "m"; "mm"] in
  let header_file_exts = ["h"; "hh"; "hpp"; "hxx"] in
  let file_no_ext, ext_opt = Core.Std.Filename.split_extension abs_path in
  let file_opt = match ext_opt with
    | Some ext when IList.mem Core.Std.String.equal ext header_file_exts -> (
        let possible_files = IList.map (fun ext -> file_no_ext ^ "." ^ ext) source_file_exts in
        try Some (IList.find source_file_path_exists possible_files)
        with Not_found -> None
      )
    | _ -> None in
  Option.map source_file_from_abs_path file_opt

let changed_source_files_set =
  let create_source_file path =
    if Filename.is_relative path then
      (* sources in changed-files-index may be specified relative to project root *)
      RelativeProjectRoot path
    else
      source_file_from_abs_path path in
  Option.map_default read_file None Config.changed_files_index |>
  Option.map (
    IList.fold_left
      (fun changed_files line ->
         let source_file = create_source_file line in
         let changed_files' = SourceFileSet.add source_file changed_files in
         (* Add source corresponding to changed header if it exists *)
         match source_file_of_header source_file with
         | Some src -> SourceFileSet.add src changed_files'
         | None -> changed_files'
      )
      SourceFileSet.empty
  )

(** {2 Source Dirs} *)

(** source directory: the directory inside the results dir corresponding to a source file *)
type source_dir = string [@@deriving compare]

(** expose the source dir as a string *)
let source_dir_to_string source_dir = source_dir

(** get the path to an internal file with the given extention (.cfg, .cg, .tenv) *)
let source_dir_get_internal_file source_dir extension =
  let source_dir_name =
    string_append_crc_cutoff (Filename.chop_extension (Filename.basename source_dir)) in
  let fname = source_dir_name ^ extension in
  Filename.concat source_dir fname

let captured_dir =
  Filename.concat Config.results_dir Config.captured_dir_name

(** get the source directory corresponding to a source file *)
let source_dir_from_source_file source_file =
  Filename.concat captured_dir (source_file_encoding source_file)

(** Find the source directories in the results dir *)
let find_source_dirs () =
  let source_dirs = ref [] in
  let files_in_results_dir = Array.to_list (Sys.readdir captured_dir) in
  let add_cg_files_from_dir dir =
    let files = Array.to_list (Sys.readdir dir) in
    IList.iter (fun fname ->
        let path = Filename.concat dir fname in
        if Filename.check_suffix path ".cg" then source_dirs := dir :: !source_dirs)
      files in
  IList.iter (fun fname ->
      let dir = Filename.concat captured_dir fname in
      if Sys.is_directory dir then add_cg_files_from_dir dir)
    files_in_results_dir;
  IList.rev !source_dirs

(** {2 Filename} *)

type filename = string [@@deriving compare]

let filename_concat = Filename.concat

let filename_to_string s = s

let filename_from_string s = s

let filename_add_suffix fn s = fn ^ s

let chop_extension = Filename.chop_extension

let file_exists = Sys.file_exists

let file_remove = Sys.remove

module FilenameSet = Set.Make(
  struct
    type t = filename [@@deriving compare]
  end)

module FilenameMap = Map.Make(
  struct
    type t = filename [@@deriving compare]
  end)

(** Return the time when a file was last modified. The file must exist. *)
let file_modified_time ?(symlink=false) fname =
  try
    let stat = (if symlink then Unix.lstat else Unix.stat) fname in
    stat.Unix.st_mtime
  with Unix.Unix_error _ ->
    Logging.do_err "File %s does not exist." fname;
    exit 1

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
    | Abs_root
    (** absolute path implicitly rooted at the root of the results dir *)
    | Abs_source_dir of source_file
    (** absolute path implicitly rooted at the source directory for the file *)
    | Rel
    (** relative path *)

  let filename_from_base base path =
    let rec f = function
      | [] -> base
      | name:: names ->
          Filename.concat (f names) (if name ==".." then Filename.parent_dir_name else name) in
    f (IList.rev path)

  (** convert a path to a filename *)
  let path_to_filename pk path =
    let base = match pk with
      | Abs_root -> Config.results_dir
      | Abs_source_dir source ->
          let dir = source_dir_from_source_file source in
          source_dir_to_string dir
      | Rel -> Filename.current_dir_name in
    filename_from_base base path

  (** directory of spec files *)
  let specs_dir = path_to_filename Abs_root [Config.specs_dir_name]

  (** initialize the results directory *)
  let init source =
    create_dir Config.results_dir;
    create_dir specs_dir;
    create_dir (path_to_filename Abs_root [Config.attributes_dir_name]);
    create_dir (path_to_filename Abs_root [Config.captured_dir_name]);
    if not (equal_source_file source source_file_empty) then
      create_dir (path_to_filename (Abs_source_dir source) [])

  let clean_specs_dir () =
    create_dir specs_dir; (* create dir just in case it doesn't exist to avoid errors *)
    let files_to_remove = Array.map (Filename.concat specs_dir) (Sys.readdir specs_dir) in
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

(** origin of a analysis artifact: current results dir, a spec library, or models *)
type origin =
  | Res_dir
  | Spec_lib
  | Models

let global_tenv_fname =
  let basename = Config.global_tenv_filename in
  filename_concat captured_dir basename

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

(** Mark a file as updated by changing its timestamps to be one second in the future.
    This guarantees that it appears updated after start. *)
let mark_file_updated fname =
  let near_future = Unix.gettimeofday () +. 1. in
  Unix.utimes fname near_future near_future

(** Fold over all file paths recursively under [dir] which match [p]. *)
let fold_paths_matching ~dir ~p ~init ~f =
  let rec paths path_list dir =
    Array.fold_left
      (fun acc file ->
         let path = dir // file in
         if Sys.is_directory path then (paths acc path)
         else if p path then f path acc
         else acc)
      path_list
      (Sys.readdir dir) in
  paths init dir

(** Return all absolute paths recursively under root_dir, matching the given
    matcher function p *)
let paths_matching dir p =
  fold_paths_matching ~dir ~p ~init:[] ~f:(fun x xs -> x :: xs)
