(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging

module rec CTransImpl : CModule_type.CTranslation = CTrans.CTrans_funct (CFrontend_declImpl)

and CFrontend_declImpl : CModule_type.CFrontend = CFrontend_decl.CFrontend_decl_funct (CTransImpl)

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg trans_unit_ctx tenv ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list ;
      L.(debug Capture Verbose) "@\n Start creating icfg@\n" ;
      let cfg = Cfg.create () in
      List.iter
        ~f:(CFrontend_declImpl.translate_one_declaration trans_unit_ctx tenv cfg `DeclTraversal)
        decl_list ;
      L.(debug Capture Verbose) "@\n Finished creating icfg@\n" ;
      cfg
  | _ ->
      assert false


(* NOTE: Assumes that an AST always starts with a TranslationUnitDecl *)

let init_global_state_capture () =
  Ident.NameGenerator.reset () ;
  CFrontend_config.global_translation_unit_decls := [] ;
  CProcname.reset_block_counter ()


let do_source_file (translation_unit_context: CFrontend_config.translation_unit_context) ast =
  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = translation_unit_context.CFrontend_config.source_file in
  L.(debug Capture Verbose)
    "@\n Start building call/cfg graph for '%a'....@\n" SourceFile.pp source_file ;
  let cfg = compute_icfg translation_unit_context tenv ast in
  L.(debug Capture Verbose)
    "@\n End building call/cfg graph for '%a'.@\n" SourceFile.pp source_file ;
  (* This part below is a boilerplate in every frontends. *)
  (* This could be moved in the cfg_infer module *)
  NullabilityPreanalysis.analysis cfg tenv ;
  SourceFiles.add source_file cfg (FileLocal tenv) ;
  if Config.debug_mode then Tenv.store_debug_file_for_source source_file tenv ;
  if
    Config.debug_mode || Config.testing_mode || Config.frontend_tests
    || Option.is_some Config.icfg_dotty_outfile
  then Dotty.print_icfg_dotty source_file cfg ;
  L.(debug Capture Verbose) "%a" Cfg.pp_proc_signatures cfg ;
  let procedures_translated_summary =
    EventLogger.ProceduresTranslatedSummary
      { procedures_translated_total= !CFrontend_config.procedures_attempted
      ; procedures_translated_failed= !CFrontend_config.procedures_failed
      ; lang= CFrontend_config.string_of_clang_lang translation_unit_context.lang
      ; source_file= translation_unit_context.source_file }
  in
  EventLogger.log procedures_translated_summary ;
  ()
