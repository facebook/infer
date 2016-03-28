(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


module L = Logging

open CFrontend_utils

module rec CTransImpl : CTrans.CTrans =
  CTrans.CTrans_funct(CFrontend_declImpl)
and CFrontend_declImpl : CFrontend_decl.CFrontend_decl =
  CFrontend_decl.CFrontend_decl_funct(CTransImpl)

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg tenv ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list;
      Printing.log_out "\n Start creating icfg\n";
      let cg = Cg.create () in
      let cfg = Cfg.Node.create_cfg () in
      IList.iter (CFrontend_declImpl.translate_one_declaration tenv cg cfg ast) decl_list;
      Printing.log_out "\n Finished creating icfg\n";
      (cg, cfg)
  | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)

let init_global_state source_file =
  Config.curr_language := Config.C_CPP;
  DB.current_source := source_file;
  DB.Results_dir.init ();
  Ident.NameGenerator.reset ();
  CFrontend_config.global_translation_unit_decls := [];
  CFrontend_utils.General_utils.reset_block_counter ()

let do_source_file source_file ast =
  let tenv = Tenv.create () in
  CTypes_decl.add_predefined_types tenv;
  init_global_state source_file;
  Config.nLOC := FileLOC.file_get_loc (DB.source_file_to_string source_file);
  Printing.log_out "\n Start building call/cfg graph for '%s'....\n"
    (DB.source_file_to_string source_file);
  let call_graph, cfg = compute_icfg tenv ast in
  Printing.log_out "\n End building call/cfg graph for '%s'.\n"
    (DB.source_file_to_string source_file);
  (* This part below is a boilerplate in every frontends. *)
  (* This could be moved in the cfg_infer module *)
  let source_dir = DB.source_dir_from_source_file !DB.current_source in
  let tenv_file = DB.source_dir_get_internal_file source_dir ".tenv" in
  let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
  Preanal.doit cfg call_graph tenv;
  Cg.store_to_file cg_file call_graph;
  Cfg.store_cfg_to_file cfg_file true cfg;
  (*Logging.out "Tenv %a@." Sil.pp_tenv tenv;*)
  (* Printing.print_tenv tenv; *)
  (*Printing.print_procedures cfg; *)
  General_utils.sort_fields_tenv tenv;
  Tenv.store_to_file tenv_file tenv;
  if !CFrontend_config.stats_mode then Cfg.check_cfg_connectedness cfg;
  if !CFrontend_config.stats_mode
  || !CFrontend_config.debug_mode || !CFrontend_config.testing_mode then
    (Dotty.print_icfg_dotty cfg [];
     Cg.save_call_graph_dotty None Specs.get_specs call_graph)
