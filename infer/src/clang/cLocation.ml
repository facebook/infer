(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for function to retrieve the location (file, line, etc) of instructions *)

(** Inside the AST there may be code or type definitions from other files than the one passed as an
    argument. That current file in the translation is saved in this variable. *)
let curr_file = ref DB.source_file_empty

let source_file_from_path path =
  if Filename.is_relative path then
    (failwithf
       "ERROR: Path %s is relative. Please pass an absolute path in the -c argument.@."
       path);
  match Config.project_root with
  | Some root ->
      (try
         DB.rel_source_file_from_abs_path root path
       with Failure _ ->
         Logging.err_debug "ERROR: %s should be a prefix of %s.@." root path;
         DB.source_file_from_string path)
  | None -> DB.source_file_from_string path

let choose_sloc_to_update_curr_file trans_unit_ctx sloc1 sloc2 =
  match sloc2.Clang_ast_t.sl_file with
  | Some f when DB.inode_equal (source_file_from_path f)
        trans_unit_ctx.CFrontend_config.source_file ->
      sloc2
  | _ -> sloc1

let update_curr_file trans_unit_ctx di =
  let loc_start, loc_end = di.Clang_ast_t.di_source_range in
  let loc = choose_sloc_to_update_curr_file trans_unit_ctx loc_start loc_end in
  match loc.Clang_ast_t.sl_file with
  | Some f -> curr_file := source_file_from_path f
  | None -> ()

let clang_to_sil_location trans_unit_ctx clang_loc procdesc_opt =
  let line = match clang_loc.Clang_ast_t.sl_line with
    | Some l -> l
    | None -> -1 in
  let col = match clang_loc.Clang_ast_t.sl_column with
    | Some c -> c
    | None -> -1 in
  let file, nLOC =
    match procdesc_opt with
    | Some procdesc ->
        let proc_loc = Cfg.Procdesc.get_loc procdesc in
        if (DB.source_file_equal proc_loc.Location.file DB.source_file_empty) then
          !curr_file, !Config.nLOC
        else proc_loc.Location.file, proc_loc.Location.nLOC
    | None ->
        match clang_loc.Clang_ast_t.sl_file with
        | Some f ->
            let file_db = source_file_from_path f in
            let nloc =
              if (DB.inode_equal file_db trans_unit_ctx.CFrontend_config.source_file) then
                !Config.nLOC
              else -1 in
            file_db, nloc
        | None -> !curr_file, !Config.nLOC in
  Location.{line; col; file; nLOC}

let file_in_project file =
  match Config.project_root with
  | Some root ->
      let real_root = real_path root in
      let real_file = real_path file in
      let file_in_project = string_is_prefix real_root real_file in
      let paths = Config.skip_translation_headers in
      let file_should_be_skipped =
        IList.exists
          (fun path -> string_is_prefix (Filename.concat real_root path) real_file)
          paths in
      file_in_project && not (file_should_be_skipped)
  | None -> false

let should_do_frontend_check trans_unit_ctx (loc_start, _) =
  match loc_start.Clang_ast_t.sl_file with
  | Some file ->
      let equal_current_source file =
        DB.inode_equal (source_file_from_path file)
          trans_unit_ctx.CFrontend_config.source_file in
      equal_current_source file ||
      (file_in_project file &&  not Config.testing_mode)
  | None -> false

(** We translate by default the instructions in the current file.  In C++ development, we also
    translate the headers that are part of the project. However, in testing mode, we don't want to
    translate the headers because the dot files in the frontend tests should contain nothing else
    than the source file to avoid conflicts between different versions of the libraries. *)
let should_translate trans_unit_ctx (loc_start, loc_end) decl_trans_context ~translate_when_used =
  let map_path_of pred loc =
    match loc.Clang_ast_t.sl_file with
    | Some f -> pred f
    | None -> false
  in
  let map_file_of pred loc =
    let path_pred path = pred (source_file_from_path path) in
    map_path_of path_pred loc
  in
  let equal_current_source = DB.inode_equal trans_unit_ctx.CFrontend_config.source_file
  in
  let file_in_project = map_path_of file_in_project loc_end
                        || map_path_of file_in_project loc_start in
  let translate_on_demand = translate_when_used || file_in_project || Config.models_mode in
  let file_in_models = map_path_of DB.file_is_in_cpp_model loc_end
                       || map_path_of DB.file_is_in_cpp_model loc_start in
  equal_current_source !curr_file
  || map_file_of equal_current_source loc_end
  || map_file_of equal_current_source loc_start
  || file_in_models
  || (Config.cxx_experimental && decl_trans_context = `Translation && translate_on_demand
      && not Config.testing_mode)

let should_translate_lib trans_unit_ctx source_range decl_trans_context ~translate_when_used =
  not Config.no_translate_libs
  || should_translate trans_unit_ctx source_range decl_trans_context ~translate_when_used

let is_file_blacklisted file =
  let paths = Config.skip_clang_analysis_in_path in
  let is_file_blacklisted =
    IList.exists
      (fun path -> Str.string_match (Str.regexp ("^.*/" ^ path)) file 0)
      paths in
  is_file_blacklisted

let get_sil_location_from_range trans_unit_ctx source_range prefer_first =
  let sloc1, sloc2 = source_range in
  let sloc = if not prefer_first then sloc2 else sloc1 in
  clang_to_sil_location trans_unit_ctx sloc None

let get_sil_location stmt_info context =
  let sloc1, _ = stmt_info.Clang_ast_t.si_source_range in
  clang_to_sil_location context.CContext.translation_unit_context sloc1
    (Some (CContext.get_procdesc context))
