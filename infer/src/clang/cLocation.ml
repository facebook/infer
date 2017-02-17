(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Module for function to retrieve the location (file, line, etc) of instructions *)

let clang_to_sil_location trans_unit_ctx clang_loc =
  let line = Option.value ~default:(-1) clang_loc.Clang_ast_t.sl_line in
  let col = Option.value ~default:(-1) clang_loc.Clang_ast_t.sl_column in
  let file =
    Option.value ~default:trans_unit_ctx.CFrontend_config.source_file
      clang_loc.Clang_ast_t.sl_file in
  Location.{line; col; file}

let source_file_in_project source_file =
  let file_in_project = SourceFile.is_under_project_root source_file in
  let rel_source_file = SourceFile.to_string source_file in
  let file_should_be_skipped =
    List.exists
      ~f:(fun path -> String.is_prefix ~prefix:path rel_source_file)
      Config.skip_translation_headers in
  file_in_project && not (file_should_be_skipped)

let should_do_frontend_check trans_unit_ctx (loc_start, _) =
  match loc_start.Clang_ast_t.sl_file with
  | Some source_file ->
      SourceFile.equal source_file trans_unit_ctx.CFrontend_config.source_file ||
      (source_file_in_project source_file && not Config.testing_mode)
  | None -> false

(** We translate by default the instructions in the current file.  In C++ development, we also
    translate the headers that are part of the project. However, in testing mode, we don't want to
    translate the headers because the dot files in the frontend tests should contain nothing else
    than the source file to avoid conflicts between different versions of the libraries. *)
let should_translate trans_unit_ctx (loc_start, loc_end) decl_trans_context ~translate_when_used =
  let map_file_of pred loc =
    match loc.Clang_ast_t.sl_file with
    | Some f -> pred f
    | None -> false
  in
  (* it's not necessary to compare inodes here because both files come from
     the same context - they are produced by the same invocation of ASTExporter
     which uses same logic to produce both files *)
  let equal_current_source = SourceFile.equal trans_unit_ctx.CFrontend_config.source_file
  in
  let equal_header_of_current_source maybe_header =
    (* SourceFile.of_header will cache calls to filesystem *)
    let source_of_header_opt = SourceFile.of_header maybe_header in
    Option.value_map ~f:equal_current_source ~default:false source_of_header_opt
  in
  let file_in_project = map_file_of source_file_in_project loc_end
                        || map_file_of source_file_in_project loc_start in
  let translate_on_demand = translate_when_used || file_in_project || Config.models_mode in
  let file_in_models = map_file_of SourceFile.is_cpp_model loc_end
                       || map_file_of SourceFile.is_cpp_model loc_start in
  map_file_of equal_current_source loc_end
  || map_file_of equal_current_source loc_start
  || file_in_models
  || (Config.cxx && map_file_of equal_header_of_current_source loc_start)
  || (Config.cxx && decl_trans_context = `Translation && translate_on_demand
      && not Config.testing_mode)

let should_translate_lib trans_unit_ctx source_range decl_trans_context ~translate_when_used =
  not Config.no_translate_libs
  || should_translate trans_unit_ctx source_range decl_trans_context ~translate_when_used

let is_file_blacklisted file =
  let paths = Config.skip_analysis_in_path in
  let is_file_blacklisted =
    List.exists
      ~f:(fun path -> Str.string_match (Str.regexp ("^.*/" ^ path)) file 0)
      paths in
  is_file_blacklisted

let get_sil_location_from_range trans_unit_ctx source_range prefer_first =
  let sloc1, sloc2 = source_range in
  let sloc = if not prefer_first then sloc2 else sloc1 in
  clang_to_sil_location trans_unit_ctx sloc

let get_sil_location stmt_info context =
  let sloc1, _ = stmt_info.Clang_ast_t.si_source_range in
  clang_to_sil_location context.CContext.translation_unit_context sloc1
