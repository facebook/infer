(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
module L = Logging

let count_newlines (path : string) : int =
  let f file = In_channel.fold_lines file ~init:0 ~f:(fun i _ -> i + 1) in
  In_channel.with_file path ~f


type t =
  | Invalid of string
  (* ML function of origin *)
  | Absolute of string
  | RelativeProjectRoot of string
  (* relative to project root *)
  | RelativeInferModel of string
  (* relative to infer models *)
[@@deriving compare]

let equal = [%compare.equal: t]

module OrderedSourceFile = struct
  type nonrec t = t

  let compare = compare
end

module Map = Caml.Map.Make (OrderedSourceFile)
module Set = Caml.Set.Make (OrderedSourceFile)

module Hash = Caml.Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = Caml.Hashtbl.hash
end)

let from_abs_path ?(warn_on_error = true) fname =
  if Filename.is_relative fname then
    L.(die InternalError) "Path '%s' is relative, when absolute path was expected." fname ;
  (* try to get realpath of source file. Use original if it fails *)
  let fname_real = try Utils.realpath ~warn_on_error fname with Unix.Unix_error _ -> fname in
  let project_root_real = Utils.realpath ~warn_on_error Config.project_root in
  let models_dir_real = Config.models_src_dir in
  match
    Utils.filename_to_relative ~backtrack:Config.relative_path_backtrack ~root:project_root_real
      fname_real
  with
  | Some path ->
      RelativeProjectRoot path
  | None when Config.buck_cache_mode && Filename.check_suffix fname_real "java" ->
      L.(die InternalError) "%s is not relative to %s" fname_real project_root_real
  | None -> (
    match Utils.filename_to_relative ~root:models_dir_real fname_real with
    | Some path ->
        RelativeInferModel path
    | None ->
        (* fname_real is absolute already *)
        Absolute fname_real )


let to_string =
  let root = Utils.realpath Config.project_root in
  fun ?(force_relative = false) fname ->
    match fname with
    | Invalid origin ->
        "DUMMY from " ^ origin
    | RelativeInferModel path ->
        "INFER_MODEL/" ^ path
    | RelativeProjectRoot path ->
        path
    | Absolute path ->
        if force_relative then
          Option.value_exn (Utils.filename_to_relative ~force_full_backtrack:true ~root path)
        else path


let pp fmt fname = Format.pp_print_string fmt (to_string fname)

let to_abs_path fname =
  match fname with
  | Invalid origin ->
      L.(die InternalError) "cannot be called with Invalid source file originating in %s" origin
  | RelativeProjectRoot path ->
      Filename.concat Config.project_root path
  | RelativeInferModel path ->
      Filename.concat Config.models_src_dir path
  | Absolute path ->
      path


let line_count source_file =
  let abs_path = to_abs_path source_file in
  count_newlines abs_path


let to_rel_path fname =
  match fname with RelativeProjectRoot path -> path | _ -> to_abs_path fname


let invalid origin = Invalid origin

let is_invalid = function Invalid _ -> true | _ -> false

let is_infer_model source_file =
  match source_file with
  | Invalid origin ->
      L.(die InternalError) "cannot be called with Invalid source file from %s" origin
  | RelativeProjectRoot _ | Absolute _ ->
      false
  | RelativeInferModel _ ->
      true


(** Returns true if the file is a C++ model *)
let is_cpp_model file =
  match file with
  | RelativeInferModel path ->
      String.is_prefix ~prefix:Config.relative_cpp_models_dir path
  | _ ->
      false


let is_under_project_root = function
  | Invalid origin ->
      L.(die InternalError) "cannot be called with Invalid source file from %s" origin
  | RelativeProjectRoot _ ->
      true
  | Absolute _ | RelativeInferModel _ ->
      false


let exists_cache = String.Table.create ~size:256 ()

let path_exists abs_path =
  try String.Table.find_exn exists_cache abs_path with
  | Not_found_s _ | Caml.Not_found ->
      let result = Sys.file_exists abs_path = `Yes in
      String.Table.set exists_cache ~key:abs_path ~data:result ;
      result


let of_header ?(warn_on_error = true) header_file =
  let abs_path = to_abs_path header_file in
  let source_exts = ["c"; "cc"; "cpp"; "cxx"; "m"; "mm"] in
  let header_exts = ["h"; "hh"; "hpp"; "hxx"] in
  match Filename.split_extension abs_path with
  | file_no_ext, Some ext when List.mem ~equal:String.equal header_exts ext ->
      List.find_map source_exts ~f:(fun ext ->
          let possible_file = file_no_ext ^ "." ^ ext in
          if path_exists possible_file then Some (from_abs_path ~warn_on_error possible_file)
          else None )
  | _ ->
      None


let create ?(warn_on_error = true) path =
  if Filename.is_relative path then
    (* sources in changed-files-index may be specified relative to project root *)
    RelativeProjectRoot path
  else from_abs_path ~warn_on_error path


let changed_sources_from_changed_files changed_files =
  List.fold changed_files ~init:Set.empty ~f:(fun changed_files_set line ->
      let source_file = create line in
      let changed_files' = Set.add source_file changed_files_set in
      (* Add source corresponding to changed header if it exists *)
      match of_header source_file with
      | Some src ->
          Set.add src changed_files'
      | None ->
          changed_files' )


module SQLite = struct
  type nonrec t = t

  let serialize = function
    | RelativeProjectRoot path ->
        (* show the most common paths as text (for debugging, possibly perf) *)
        Sqlite3.Data.TEXT path
    | _ as x ->
        Sqlite3.Data.BLOB (Marshal.to_string x [])


  let deserialize = function[@warning "-8"]
    | Sqlite3.Data.TEXT rel_path ->
        RelativeProjectRoot rel_path
    | Sqlite3.Data.BLOB b ->
        Marshal.from_string b 0
end
