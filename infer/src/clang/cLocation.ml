(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  let macro_file_opt = Option.map ~f:SourceFile.from_abs_path clang_loc.Clang_ast_t.sl_macro_file in
  let macro_line = Option.value ~default:(-1) clang_loc.Clang_ast_t.sl_macro_line in
  Location.{line; col; file; macro_file_opt; macro_line}


let matches_skip_translation_headers =
  match Config.skip_translation_headers with
  | [] ->
      fun _ -> false
  | reg_list ->
      let regex = Str.regexp (String.concat ~sep:"\\|" reg_list) in
      fun file -> Str.string_match regex file 0


let source_file_in_project source_file =
  let file_in_project = SourceFile.is_under_project_root source_file in
  let source_file_path = SourceFile.to_string source_file in
  let file_should_be_skipped = matches_skip_translation_headers source_file_path in
  file_in_project && not file_should_be_skipped


(** We translate by default the instructions in the current file. In C++ development, we also
    translate the headers that are part of the project. However, in testing mode, we don't want to
    translate the headers because the dot files in the frontend tests should contain nothing else
    than the source file to avoid conflicts between different versions of the libraries. *)
let should_translate translation_unit (loc_start, loc_end) decl_trans_context ~translate_when_used =
  let map_file_of pred loc =
    match Option.map ~f:SourceFile.create loc.Clang_ast_t.sl_file with
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
    Option.exists ~f:equal_current_source source_of_header_opt
  in
  let file_in_project =
    map_file_of source_file_in_project loc_end || map_file_of source_file_in_project loc_start
  in
  let translate_on_demand =
    translate_when_used || file_in_project || Config.biabduction_models_mode
  in
  map_file_of equal_current_source loc_end
  || map_file_of equal_current_source loc_start
  || (Config.cxx && map_file_of equal_header_of_current_source loc_start)
  || Config.cxx
     && (decl_trans_context = `Translation || decl_trans_context = `CppLambdaExprTranslation)
     && translate_on_demand && not Config.testing_mode


let should_translate_lib translation_unit source_range decl_trans_context ~translate_when_used =
  (not Config.no_translate_libs)
  || should_translate translation_unit source_range decl_trans_context ~translate_when_used


let is_file_block_listed file =
  Option.exists ~f:(fun re -> Str.string_match re file 0) Config.skip_analysis_in_path
  || Inferconfig.capture_block_list_file_matcher (SourceFile.create file)


let location_of_source_range ?(pick_location = `Start) default_source_file source_range =
  source_range
  |> (match pick_location with `Start -> fst | `End -> snd)
  |> clang_to_sil_location default_source_file


let location_of_stmt_info default_source_file stmt_info =
  location_of_source_range default_source_file stmt_info.Clang_ast_t.si_source_range
