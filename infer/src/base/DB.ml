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
open! PVariant

(** Database of analysis results *)

module F = Format
module L = Logging



let cutoff_length = 100
let crc_token = '.'

let append_crc_cutoff ?(key="") name =
  let name_up_to_cutoff =
    if String.length name <= cutoff_length
    then name
    else String.sub name ~pos:0 ~len:cutoff_length in
  let crc_str =
    let name_for_crc = name ^ key in
    Utils.string_crc_hex32 name_for_crc in
  name_up_to_cutoff ^ Char.to_string crc_token ^ crc_str

(* Lengh of .crc part: 32 characters of digest, plus 1 character of crc_token *)
let dot_crc_len = 1 + 32

let strip_crc str =
  Core.Std.String.slice str 0 (- dot_crc_len)

let string_crc_has_extension ~ext name_crc =
  let name = strip_crc name_crc in
  match Filename.split_extension name with
  | (_, Some ext') -> String.equal ext ext'
  | (_, None) -> false

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

(** get the path to an internal file with the given extention (.cfg, .cg, .tenv) *)
let source_dir_get_internal_file source_dir extension =
  let source_dir_name =
    append_crc_cutoff (Caml.Filename.remove_extension (Filename.basename source_dir)) in
  let fname = source_dir_name ^ extension in
  Filename.concat source_dir fname

(** get the source directory corresponding to a source file *)
let source_dir_from_source_file source_file =
  Filename.concat Config.captured_dir (source_file_encoding source_file)

(** Find the source directories in the results dir *)
let find_source_dirs () =
  let source_dirs = ref [] in
  let files_in_results_dir = Array.to_list (Sys.readdir Config.captured_dir) in
  let add_cg_files_from_dir dir =
    let files = Array.to_list (Sys.readdir dir) in
    List.iter ~f:(fun fname ->
        let path = Filename.concat dir fname in
        if Filename.check_suffix path ".cg" then source_dirs := dir :: !source_dirs)
      files in
  List.iter ~f:(fun fname ->
      let dir = Filename.concat Config.captured_dir fname in
      if Sys.is_directory dir = `Yes then add_cg_files_from_dir dir)
    files_in_results_dir;
  List.rev !source_dirs

(** {2 Filename} *)

type filename = string [@@deriving compare]

let equal_filename = [%compare.equal : filename]

let filename_concat = Filename.concat

let filename_to_string s = s

let filename_from_string s = s

let filename_add_suffix fn s = fn ^ s

let chop_extension = Filename.chop_extension

let file_exists path = Sys.file_exists path = `Yes

let file_remove = Sys.remove

module FilenameSet = Caml.Set.Make(
  struct
    type t = filename [@@deriving compare]
  end)

module FilenameMap = Caml.Map.Make(
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
  if (Sys.file_exists dirname) <> `Yes
  then Utils.create_dir dirname

let read_whole_file fd =
  In_channel.input_all (Unix.in_channel_of_descr fd)

(** Update the file contents with the update function provided.
    If the directory does not exist, it is created.
    If the file does not exist, it is created, and update is given the empty string.
    A lock is used to allow write attempts in parallel. *)
let update_file_with_lock dir fname update =
  let reset_file fd =
    let n = Unix.lseek fd 0L ~mode:Unix.SEEK_SET in
    if n <> 0L then
      begin
        L.stderr "reset_file: lseek fail@.";
        assert false
      end in
  Utils.create_dir dir;
  let path = Filename.concat dir fname in
  let fd = Unix.openfile path ~mode:Unix.[O_CREAT; O_SYNC; O_RDWR] ~perm:0o640 in
  Unix.lockf fd ~mode:Unix.F_LOCK ~len:0L;
  let buf = read_whole_file fd in
  reset_file fd;
  let str = update buf in
  let i = Unix.write fd ~buf:str ~pos:0 ~len:(String.length str) in
  if Int.equal i (String.length str) then (
    Unix.lockf fd ~mode:Unix.F_ULOCK ~len:0L;
    Unix.close fd
  ) else (
    L.err "@.save_with_lock: fail on path: %s@." path;
    assert false
  )

(** Read a file using a lock to allow write attempts in parallel. *)
let read_file_with_lock dir fname =
  let path = Filename.concat dir fname in
  try
    let fd = Unix.openfile path ~mode:Unix.[O_RSYNC; O_RDONLY] ~perm:0o646 in
    try
      Unix.lockf fd ~mode:Unix.F_RLOCK ~len:0L;
      let buf = read_whole_file fd in
      Unix.lockf fd ~mode:Unix.F_ULOCK ~len:0L;
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
    | Abs_source_dir of SourceFile.t
    (** absolute path implicitly rooted at the source directory for the file *)
    | Rel
    (** relative path *)

  let filename_from_base base path =
    let rec f = function
      | [] -> base
      | name:: names ->
          Filename.concat (f names) (if String.equal name ".." then Filename.parent_dir_name else name) in
    f (List.rev path)

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
    if SourceFile.is_invalid source then
      invalid_arg "Invalid source file passed";
    Utils.create_dir Config.results_dir;
    Utils.create_dir specs_dir;
    Utils.create_dir (path_to_filename Abs_root [Config.attributes_dir_name]);
    Utils.create_dir (path_to_filename Abs_root [Config.captured_dir_name]);
    Utils.create_dir (path_to_filename (Abs_source_dir source) [])

  let clean_specs_dir () =
    Utils.create_dir specs_dir; (* create dir just in case it doesn't exist to avoid errors *)
    let files_to_remove = Array.map ~f:(Filename.concat specs_dir) (Sys.readdir specs_dir) in
    Array.iter ~f:Sys.remove files_to_remove

  (** create a file at the given path, creating any missing directories *)
  let create_file pk path =
    let rec create = function
      | [] ->
          let fname = path_to_filename pk [] in
          Utils.create_dir fname;
          fname
      | name:: names ->
          let new_path = Filename.concat (create names) name in
          Utils.create_dir new_path;
          new_path in
    let filename, dir_path = match List.rev path with
      | filename:: dir_path -> filename, dir_path
      | [] -> raise (Failure "create_path") in
    let full_fname = Filename.concat (create dir_path) filename in
    Unix.openfile full_fname ~mode:Unix.[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o777
end

let global_tenv_fname =
  let basename = Config.global_tenv_filename in
  filename_concat Config.captured_dir basename

let is_source_file path =
  List.exists
    ~f:(fun ext -> Filename.check_suffix path ext)
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
  Unix.utimes fname ~access:near_future ~modif:near_future

(** Fold over all file paths recursively under [dir] which match [p]. *)
let fold_paths_matching ~dir ~p ~init ~f =
  let rec paths path_list dir =
    Array.fold
      ~f:(fun acc file ->
          let path = dir ^/ file in
          if Sys.is_directory path = `Yes then (paths acc path)
          else if p path then f path acc
          else acc)
      ~init:path_list
      (Sys.readdir dir) in
  paths init dir

(** Return all absolute paths recursively under root_dir, matching the given
    matcher function p *)
let paths_matching dir p =
  fold_paths_matching ~dir ~p ~init:[] ~f:(fun x xs -> x :: xs)
