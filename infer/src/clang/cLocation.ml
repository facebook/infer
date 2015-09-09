(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for function to retrieve the location (file, line, etc) of instructions *)

open CFrontend_utils
open Utils

(* The file passed as an argument to InferClang *)
let current_source_file = ref DB.source_file_empty

(* Inside the json there may be code or type definitions from other files *)
(* than the one passed as an argument. That current file in the translation is saved*)
(* in this variable. *)
let curr_file = ref DB.source_file_empty

let init_curr_source_file source_file =
  current_source_file := source_file

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

let choose_sloc sloc1 sloc2 prefer_first =
  let sloc_bad sloc =
    match sloc.Clang_ast_t.sl_file with
    | Some f when not (DB.source_file_equal (source_file_from_path f) !curr_file) ->
        true
    | _ -> false in
  if sloc_bad sloc1 then sloc2
  else if prefer_first then sloc1 else sloc2

let choose_sloc_to_update_curr_file sloc1 sloc2 =
  let sloc_curr_file sloc =
    match sloc.Clang_ast_t.sl_file with
    | Some f when DB.source_file_equal (source_file_from_path f) !current_source_file ->
        true
    | _ -> false in
  if sloc_curr_file sloc2 then sloc2
  else sloc1

let update_curr_file di =
  match di.Clang_ast_t.di_source_range with (loc_start, loc_end) ->
    let loc = choose_sloc_to_update_curr_file loc_start loc_end in
    (match loc.Clang_ast_t.sl_file with
     | Some f -> curr_file := source_file_from_path f
     | None -> ())

let clang_to_sil_location clang_loc parent_line_number procdesc_opt =
  let line = match clang_loc.Clang_ast_t.sl_line with
    | Some l -> l
    | None -> parent_line_number in
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
              if (DB.source_file_equal file_db !current_source_file) then
                !Config.nLOC
              else -1 in
            file_db, nloc
        | None -> !curr_file, !Config.nLOC in
  { Location.line = line; Location.col = col; Location.file = file; Location.nLOC = nLOC }

let should_translate_lib source_range =
  if !CFrontend_config.no_translate_libs then
    match source_range with (loc_start, loc_end) ->
      let loc_start = choose_sloc_to_update_curr_file loc_start loc_end in
      let loc = clang_to_sil_location loc_start (-1) None in
      DB.source_file_equal loc.Location.file !DB.current_source
  else true

let should_translate_enum source_range =
  if !CFrontend_config.testing_mode then
    match source_range with (loc_start, loc_end) ->
      let loc_start = choose_sloc_to_update_curr_file loc_start loc_end in
      let loc = clang_to_sil_location loc_start (-1) None in
      DB.source_file_equal loc.Location.file !DB.current_source
  else true

let get_sil_location_from_range source_range prefer_first =
  match source_range with (sloc1, sloc2) ->
    let sloc = choose_sloc sloc1 sloc2 prefer_first in
    clang_to_sil_location sloc (-1) None

let get_sil_location stmt_info parent_line_number context =
  match stmt_info.Clang_ast_t.si_source_range with (sloc1, sloc2) ->
    let sloc = choose_sloc sloc1 sloc2 true in
    clang_to_sil_location sloc parent_line_number (Some (CContext.get_procdesc context))

let get_line stmt_info line_number =
  match stmt_info.Clang_ast_t.si_source_range with
  | (sloc1, sloc2) ->
      let sloc = choose_sloc sloc1 sloc2 true in
      (match sloc.Clang_ast_t.sl_line with
       | Some l -> l
       | None -> line_number)

let check_source_file source_file =
  let extensions_allowed = [".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"] in
  let allowed = list_exists (fun ext -> Filename.check_suffix source_file ext) extensions_allowed in
  if not allowed then
    (Printing.log_stats "%s"
       ("\nThe source file "^source_file^
        " should end with "^(Utils.list_to_string (fun x -> x) extensions_allowed)^"\n\n");
     assert false)
