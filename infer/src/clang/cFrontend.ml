(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils


module L = Logging

open CFrontend_utils

module rec CTransImpl : CModule_type.CTranslation =
  CTrans.CTrans_funct(CFrontend_declImpl)
and CFrontend_declImpl : CModule_type.CFrontend =
  CFrontend_decl.CFrontend_decl_funct(CTransImpl)

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg trans_unit_ctx tenv ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list;
      Logging.out_debug "@\n Start creating icfg@\n";
      let cg = Cg.create (Some trans_unit_ctx.CFrontend_config.source_file) in
      let cfg = Cfg.Node.create_cfg () in
      IList.iter
        (CFrontend_declImpl.translate_one_declaration trans_unit_ctx tenv cg cfg `DeclTraversal)
        decl_list;
      Logging.out_debug "\n Finished creating icfg\n";
      (cg, cfg)
  | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)

let init_global_state_capture () =
  Ident.NameGenerator.reset ();
  CFrontend_config.global_translation_unit_decls := [];
  CFrontend_utils.General_utils.reset_block_counter ()

let do_source_file translation_unit_context ast =
  let tenv = Tenv.create () in
  CTypes_decl.add_predefined_types tenv;
  init_global_state_capture ();
  let source_file = translation_unit_context.CFrontend_config.source_file in
  Config.nLOC := FileLOC.file_get_loc (DB.source_file_to_string source_file);
  Logging.out_debug "@\n Start building call/cfg graph for '%s'....@\n"
    (DB.source_file_to_string source_file);
  let call_graph, cfg = compute_icfg translation_unit_context tenv ast in
  Logging.out_debug "@\n End building call/cfg graph for '%s'.@\n"
    (DB.source_file_to_string source_file);
  (* This part below is a boilerplate in every frontends. *)
  (* This could be moved in the cfg_infer module *)
  let source_dir = DB.source_dir_from_source_file source_file in
  let tenv_file = DB.source_dir_get_internal_file source_dir ".tenv" in
  let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
  Cg.store_to_file cg_file call_graph;
  Cfg.store_cfg_to_file ~source_file cfg_file cfg;
  (*Logging.out "Tenv %a@." Sil.pp_tenv tenv;*)
  (* Printing.print_tenv tenv; *)
  (*Printing.print_procedures cfg; *)
  General_utils.sort_fields_tenv tenv;
  Tenv.store_to_file tenv_file tenv;
  if Config.stats_mode then Cfg.check_cfg_connectedness cfg;
  if Config.stats_mode
  || Config.debug_mode
  || Config.testing_mode
  || Config.frontend_tests then
    (Dotty.print_icfg_dotty source_file cfg;
     Cg.save_call_graph_dotty source_file Specs.get_specs call_graph);
  (* NOTE: nothing should be written to source_dir after this *)
  DB.mark_file_updated (DB.source_dir_to_string source_dir)
