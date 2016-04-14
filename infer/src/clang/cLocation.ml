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

open CFrontend_utils

(* Inside the json there may be code or type definitions from other files *)
(* than the one passed as an argument. That current file in the translation is saved*)
(* in this variable. *)
let curr_file = ref DB.source_file_empty

let source_file_from_path path =
  if Filename.is_relative path then
    (Printing.log_err
       "ERROR: Path %s is relative. Please pass an absolute path in the -c argument.@."
       path;
     exit 1);
  match !Config.project_root with
  | Some root ->
      (try
         DB.rel_source_file_from_abs_path root path
       with Failure _ ->
         Printing.log_err "ERROR: %s should be a prefix of %s.@." root path;
         DB.source_file_from_string path)
  | None -> DB.source_file_from_string path

let choose_sloc sloc1 sloc2 =
  match sloc1.Clang_ast_t.sl_file with
  | Some f when not (DB.source_file_equal (source_file_from_path f) !curr_file) -> sloc2
  | _ -> sloc1

let choose_sloc_to_update_curr_file sloc1 sloc2 =
  match sloc2.Clang_ast_t.sl_file with
  | Some f when DB.source_file_equal (source_file_from_path f) !DB.current_source -> sloc2
  | _ -> sloc1

let update_curr_file di =
  let loc_start, loc_end = di.Clang_ast_t.di_source_range in
  let loc = choose_sloc_to_update_curr_file loc_start loc_end in
  match loc.Clang_ast_t.sl_file with
  | Some f -> curr_file := source_file_from_path f
  | None -> ()

let clang_to_sil_location clang_loc procdesc_opt =
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
              if (DB.source_file_equal file_db !DB.current_source) then
                !Config.nLOC
              else -1 in
            file_db, nloc
        | None -> !curr_file, !Config.nLOC in
  Location.{line; col; file; nLOC}

let file_in_project file = match !Config.project_root with
  | Some root -> string_is_prefix root file
  | None -> false

let should_do_frontend_check (loc_start, _) =
  let file =
    match loc_start.Clang_ast_t.sl_file with
    | Some f -> f
    | None -> assert false in
  let equal_current_source file =
    DB.source_file_equal (source_file_from_path file) !DB.current_source in
  equal_current_source file ||
  (file_in_project file &&  not !CFrontend_config.testing_mode)

(* We translate by default the instructions in the current file.*)
(* In C++ development, we also translate the headers that are part *)
(* of the project. However, in testing mode, we don't want to translate *)
(* the headers because the dot files in the frontend tests should contain nothing *)
(* else than the source file to avoid conflicts between different versions of the *)
(* libraries in the CI *)
let should_translate (loc_start, loc_end) decl_trans_context =
  let map_path_of pred loc =
    match loc.Clang_ast_t.sl_file with
    | Some f -> pred f
    | None -> false
  in
  let map_file_of pred loc =
    let path_pred path = pred (source_file_from_path path) in
    map_path_of path_pred loc
  in
  let equal_current_source file =
    DB.source_file_equal file !DB.current_source
  in
  let file_in_project = map_path_of file_in_project loc_end
                        || map_path_of file_in_project loc_start in
  let file_in_models file = Str.string_match (Str.regexp "^.*/infer/models/cpp/include/") file 0 in
  let file_in_models = map_path_of file_in_models loc_end
                       || map_path_of file_in_models loc_start in
  equal_current_source !curr_file
  || map_file_of equal_current_source loc_end
  || map_file_of equal_current_source loc_start
  || file_in_models
  || (!CFrontend_config.cxx_experimental && decl_trans_context = `Translation && file_in_project
      && not (!CFrontend_config.testing_mode))

let should_translate_lib source_range decl_trans_context =
  not !CFrontend_config.no_translate_libs
  || should_translate source_range decl_trans_context

let get_sil_location_from_range source_range prefer_first =
  let sloc1, sloc2 = source_range in
  let sloc = if not prefer_first then sloc2 else choose_sloc sloc1 sloc2 in
  clang_to_sil_location sloc None

let get_sil_location stmt_info context =
  let sloc1, sloc2 = stmt_info.Clang_ast_t.si_source_range in
  let sloc = choose_sloc sloc1 sloc2 in
  clang_to_sil_location sloc (Some (CContext.get_procdesc context))

let check_source_file source_file =
  let extensions_allowed = [".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"] in
  let allowed = IList.exists (fun ext -> Filename.check_suffix source_file ext) extensions_allowed in
  if not allowed then
    (Printing.log_stats "%s"
       ("\nThe source file "^source_file^
        " should end with "^(IList.to_string (fun x -> x) extensions_allowed)^"\n\n");
     assert false)
