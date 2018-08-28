(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual

(** Module for function to retrieve the location (file, line, etc) of instructions *)

let clang_to_sil_location default_source_file clang_loc =
  let line = Option.value ~default:(-1) clang_loc.Clang_ast_t.sl_line in
  let col = Option.value ~default:(-1) clang_loc.Clang_ast_t.sl_column in
  let file =
    Option.value_map ~default:default_source_file ~f:SourceFile.from_abs_path
      clang_loc.Clang_ast_t.sl_file
  in
  Location.{line; col; file}


let source_file_in_project source_file =
  let file_in_project = SourceFile.is_under_project_root source_file in
  let rel_source_file = SourceFile.to_string source_file in
  let file_should_be_skipped =
    List.exists
      ~f:(fun path -> String.is_prefix ~prefix:path rel_source_file)
      Config.skip_translation_headers
  in
  file_in_project && not file_should_be_skipped


let should_do_frontend_check translation_unit (loc_start, _) =
  match Option.map ~f:SourceFile.from_abs_path loc_start.Clang_ast_t.sl_file with
  | Some source_file ->
      SourceFile.equal translation_unit source_file
      || (source_file_in_project source_file && not Config.testing_mode)
  | None ->
      false


(** We translate by default the instructions in the current file.  In C++ development, we also
    translate the headers that are part of the project. However, in testing mode, we don't want to
    translate the headers because the dot files in the frontend tests should contain nothing else
    than the source file to avoid conflicts between different versions of the libraries. *)
let should_translate translation_unit (loc_start, loc_end) decl_trans_context ~translate_when_used
    =
  let map_file_of pred loc =
    match Option.map ~f:SourceFile.from_abs_path loc.Clang_ast_t.sl_file with
    | Some f ->
        pred f
    | None ->
        false
  in
  (* it's not necessary to compare inodes here because both files come from
     the same context - they are produced by the same invocation of ASTExporter
     which uses same logic to produce both files *)
  let equal_current_source = SourceFile.equal translation_unit in
  let equal_header_of_current_source maybe_header =
    (* SourceFile.of_header will cache calls to filesystem *)
    let source_of_header_opt = SourceFile.of_header maybe_header in
    Option.value_map ~f:equal_current_source ~default:false source_of_header_opt
  in
  let file_in_project =
    map_file_of source_file_in_project loc_end || map_file_of source_file_in_project loc_start
  in
  let translate_on_demand = translate_when_used || file_in_project || Config.models_mode in
  let file_in_models =
    map_file_of SourceFile.is_cpp_model loc_end || map_file_of SourceFile.is_cpp_model loc_start
  in
  map_file_of equal_current_source loc_end
  || map_file_of equal_current_source loc_start
  || file_in_models
  || (Config.cxx && map_file_of equal_header_of_current_source loc_start)
  || Config.cxx
     && decl_trans_context = `Translation
     && translate_on_demand && not Config.testing_mode


let should_translate_lib translation_unit source_range decl_trans_context ~translate_when_used =
  (not Config.no_translate_libs)
  || should_translate translation_unit source_range decl_trans_context ~translate_when_used


let is_file_blacklisted file =
  let paths = Config.skip_analysis_in_path in
  let is_file_blacklisted =
    List.exists ~f:(fun path -> Str.string_match (Str.regexp ("^.*/" ^ path)) file 0) paths
  in
  is_file_blacklisted


let location_of_source_range ?(pick_location = `Start) default_source_file source_range =
  source_range
  |> (match pick_location with `Start -> fst | `End -> snd)
  |> clang_to_sil_location default_source_file


let location_of_stmt_info default_source_file stmt_info =
  location_of_source_range default_source_file stmt_info.Clang_ast_t.si_source_range
