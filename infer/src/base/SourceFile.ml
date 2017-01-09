(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let count_newlines (path: string): int =
  let f file = In_channel.fold_lines file ~init:0 ~f:(fun i _ -> i + 1) in
  In_channel.with_file path ~f

type t =
  | Absolute of string
  | RelativeProjectRoot of string (* relative to project root *)
  | RelativeInferModel of string (* relative to infer models *)
[@@deriving compare]

let equal sf1 sf2 =
  compare sf1 sf2 = 0

module OrderedSourceFile =
struct
  (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
  type _t = t [@@deriving compare]
  type t = _t [@@deriving compare]
end

module Map = Caml.Map.Make (OrderedSourceFile)

module Set = Caml.Set.Make (OrderedSourceFile)

let from_abs_path fname =
  if Filename.is_relative fname then
    (failwithf
       "ERROR: Path %s is relative, when absolute path was expected .@."
       fname);
  (* try to get realpath of source file. Use original if it fails *)
  let fname_real = try Utils.realpath fname with Unix.Unix_error _ -> fname in
  let project_root_real = Utils.realpath Config.project_root in
  let models_dir_real = Config.models_src_dir in
  match Utils.filename_to_relative ~root:project_root_real fname_real with
  | Some path -> RelativeProjectRoot path
  | None -> (
      match Utils.filename_to_relative ~root:models_dir_real fname_real with
      | Some path -> RelativeInferModel path
      | None -> Absolute fname (* fname is absolute already *)
    )

let curr_encoding = `Enc_crc

let to_string fname =
  match fname with
  | RelativeInferModel path -> "INFER_MODEL/" ^ path
  | RelativeProjectRoot path
  | Absolute path -> path

let pp fmt fname =
  Format.fprintf fmt "%s" (to_string fname)

(* Checking if the path exists may be needed only in some cases, hence the flag check_exists *)
let to_abs_path fname =
  match fname with
  | RelativeProjectRoot path -> Filename.concat Config.project_root path
  | RelativeInferModel path -> Filename.concat Config.models_src_dir path
  | Absolute path -> path

let line_count source_file =
  let abs_path = to_abs_path source_file in
  count_newlines abs_path

let to_rel_path fname =
  match fname with
  | RelativeProjectRoot path -> path
  | _ -> to_abs_path fname

(** string encoding of a source file (including path) as a single filename *)
let encoding source_file =
  let prefix = match source_file with
    | RelativeProjectRoot _ -> "P"
    | RelativeInferModel _ -> "MOD"
    | Absolute _ -> "ABS" in
  let source_file_s = to_string source_file in
  match curr_encoding with
  | `Enc_base ->
      Filename.basename source_file_s
  | `Enc_path_with_underscores ->
      prefix ^ Escape.escape_path source_file_s
  | `Enc_crc ->
      let base = Filename.basename source_file_s in
      let dir = prefix ^ Filename.dirname source_file_s in
      Utils.string_append_crc_cutoff ~key:dir base

let empty = Absolute ""

let is_infer_model source_file = match source_file with
  | RelativeProjectRoot _ | Absolute _ -> false
  | RelativeInferModel _ -> true

(** Returns true if the file is a C++ model *)
let is_cpp_model file =
  match file with
  | RelativeInferModel path ->
      String.is_prefix ~prefix:Config.relative_cpp_models_dir path
  | _ -> false

let is_under_project_root = function
  | RelativeProjectRoot _ -> true
  | Absolute _ | RelativeInferModel _ -> false

let exists_cache = String.Table.create ~size:256 ()

let path_exists abs_path =
  try String.Table.find_exn exists_cache abs_path
  with Not_found ->
    let result = Sys.file_exists abs_path = `Yes in
    String.Table.set exists_cache ~key:abs_path ~data:result;
    result

let of_header header_file =
  let abs_path = to_abs_path header_file in
  let source_exts = ["c"; "cc"; "cpp"; "cxx"; "m"; "mm"] in
  let header_exts = ["h"; "hh"; "hpp"; "hxx"] in
  let file_no_ext, ext_opt = Filename.split_extension abs_path in
  let file_opt = match ext_opt with
    | Some ext when IList.mem String.equal ext header_exts -> (
        let possible_files = IList.map (fun ext -> file_no_ext ^ "." ^ ext) source_exts in
        try Some (IList.find path_exists possible_files)
        with Not_found -> None
      )
    | _ -> None in
  Option.map ~f:from_abs_path file_opt

let changed_files_set =
  let create_source_file path =
    if Filename.is_relative path then
      (* sources in changed-files-index may be specified relative to project root *)
      RelativeProjectRoot path
    else
      from_abs_path path in
  Option.bind Config.changed_files_index Utils.read_file |>
  Option.map ~f:(
    IList.fold_left
      (fun changed_files line ->
         let source_file = create_source_file line in
         let changed_files' = Set.add source_file changed_files in
         (* Add source corresponding to changed header if it exists *)
         match of_header source_file with
         | Some src -> Set.add src changed_files'
         | None -> changed_files'
      )
      Set.empty
  )
