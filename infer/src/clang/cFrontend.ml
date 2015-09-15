(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Translate one file into a cfg. Create a tenv, cg and cfg file for a source file    *)
(** given its ast in json format. Translate the json file into a cfg by adding all     *)
(** the type and class declarations to the tenv, adding all the functions and methods  *)
(** declarations as procdescs to the cfg, and adding the control flow graph of all the *)
(** code of those functions and methods to the cfg   *)

module L = Logging

open Utils
open CFrontend_utils
open CGen_trans

(* Translate one global declaration *)
let rec translate_one_declaration tenv cg cfg namespace parent_dec dec =
  (* each procedure has different scope: start names from id 0 *)
  Ident.reset_name_generator ();

  let ns_suffix = Ast_utils.namespace_to_string namespace in
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.update_curr_file info;
  let source_range = info.Clang_ast_t.di_source_range in
  let should_translate_enum = CLocation.should_translate_enum source_range in
  let open Clang_ast_t in
  match dec with
  | FunctionDecl(di, name_info, qt, fdecl_info) ->
      CMethod_declImpl.function_decl tenv cfg cg namespace dec None
  | TypedefDecl (decl_info, name_info, opt_type, _, typedef_decl_info) ->
      Printing.log_out "%s" "Skipping typedef declaration. Will expand the type in its occurrences."
  (* Currently C/C++ record decl treated in the same way *)
  | CXXRecordDecl (_, _, _, _, decl_list, _, _, _)
  | RecordDecl (_, _, _, _, decl_list, _, _) ->
      ignore (CTypes_decl.add_types_from_decl_to_tenv tenv namespace dec);
      let method_decls = CTypes_decl.get_method_decls dec decl_list in
      let tranlate_method (parent, decl) =
        translate_one_declaration tenv cg cfg namespace parent decl in
      list_iter tranlate_method method_decls

  | VarDecl(decl_info, name_info, t, _) ->
      let name = name_info.Clang_ast_t.ni_name in
      CVar_decl.global_var_decl tenv namespace decl_info name t

  | ObjCInterfaceDecl(decl_info, name_info, decl_list, decl_context_info, oi_decl_info) ->
      let name = name_info.Clang_ast_t.ni_name in
      let curr_class =
        ObjcInterface_decl.interface_declaration tenv decl_info name decl_list oi_decl_info in
      CMethod_declImpl.process_methods tenv cg cfg curr_class namespace decl_list

  | ObjCProtocolDecl(decl_info, name_info, decl_list, decl_context_info, obj_c_protocol_decl_info) ->
      let name = name_info.Clang_ast_t.ni_name in
      let curr_class = ObjcProtocol_decl.protocol_decl tenv name decl_list in
      CMethod_declImpl.process_methods tenv cg cfg curr_class namespace decl_list

  | ObjCCategoryDecl(decl_info, name_info, decl_list, decl_context_info, category_decl_info) ->
      let name = name_info.Clang_ast_t.ni_name in
      let curr_class =
        ObjcCategory_decl.category_decl tenv name category_decl_info decl_list in
      CMethod_declImpl.process_methods tenv cg cfg curr_class namespace decl_list

  | ObjCCategoryImplDecl(decl_info, name_info, decl_list, decl_context_info, category_impl_info) ->
      let name = name_info.Clang_ast_t.ni_name in
      let curr_class =
        ObjcCategory_decl.category_impl_decl tenv name decl_info category_impl_info decl_list in
      CMethod_declImpl.process_methods tenv cg cfg curr_class namespace decl_list

  | ObjCImplementationDecl(decl_info, name_info, decl_list, decl_context_info, idi) ->
      let name = name_info.Clang_ast_t.ni_name in
      let curr_class =
        ObjcInterface_decl.interface_impl_declaration tenv name decl_list idi in
      CMethod_declImpl.process_methods tenv cg cfg curr_class namespace decl_list
  | CXXMethodDecl(decl_info, name_info, qual_type, function_decl_info) ->
      (* di_parent_pointer has pointer to lexical context such as class.*)
      (* If it's not defined, then it's the same as parent in AST *)
      let class_decl = match decl_info.Clang_ast_t.di_parent_pointer with
        | Some ptr -> Ast_utils.get_decl ptr
        | None -> Some parent_dec in
      (match class_decl with
       | Some CXXRecordDecl(_, name_info, opt_type, _, _, _, _, _) ->
           let class_name = CTypes_decl.get_record_name opt_type name_info in
           let curr_class = CContext.ContextCls(class_name, None, []) in
           if !CFrontend_config.testing_mode then
             CMethod_declImpl.process_methods tenv cg cfg curr_class namespace [dec]
       | Some dec -> Printing.log_stats "Methods of %s skipped\n" (Ast_utils.string_of_decl dec)
       | None -> ())

  | EnumDecl(decl_info, name_info, opt_type, pointer, decl_list, decl_context_info, enum_decl_info)
    when should_translate_enum ->
      let name = name_info.Clang_ast_t.ni_name in
      CEnum_decl.enum_decl name tenv cfg cg namespace pointer decl_list opt_type

  | LinkageSpecDecl(decl_info, decl_list, decl_context_info) ->
      Printing.log_out "ADDING: LinkageSpecDecl decl list\n";
      list_iter (translate_one_declaration tenv cg cfg namespace dec) decl_list
  | NamespaceDecl(decl_info, name_info, decl_list, decl_context_info, _) ->
      let name = ns_suffix^name_info.Clang_ast_t.ni_name in
      list_iter (translate_one_declaration tenv cg cfg (Some name) dec) decl_list
  | EmptyDecl _ ->
      Printing.log_out "Passing from EmptyDecl. Treated as skip\n";
  | dec ->
      Printing.log_stats "\nWARNING: found Declaration %s skipped\n" (Ast_utils.string_of_decl dec)

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg tenv source_file ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list;
      Printing.log_out "\n Start creating icfg\n";
      let cg = Cg.create () in
      let cfg = Cfg.Node.create_cfg () in
      list_iter (translate_one_declaration tenv cg cfg None ast) decl_list;
      Printing.log_out "\n Finished creating icfg\n";
      (cg, cfg)
  | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)

let init_global_state source_file =
  Config.curr_language := Config.C_CPP;
  DB.current_source := source_file;
  DB.Results_dir.init ();
  Ident.reset_name_generator ();
  CGlobal_vars.reset_map ();
  CFrontend_config.global_translation_unit_decls := [];
  ObjcProperty_decl.reset_property_table ();
  CFrontend_utils.General_utils.reset_block_counter ()

let do_source_file source_file ast =
  let tenv = Sil.create_tenv () in
  CTypes_decl.add_predefined_types tenv;
  init_global_state source_file;
  CLocation.init_curr_source_file source_file;
  Config.nLOC := FileLOC.file_get_loc (DB.source_file_to_string source_file);
  Printing.log_out "\n Start building call/cfg graph for '%s'....\n"
    (DB.source_file_to_string source_file);
  let call_graph, cfg = compute_icfg tenv (DB.source_file_to_string source_file) ast in
  Printing.log_out "\n End building call/cfg graph for '%s'.\n"
    (DB.source_file_to_string source_file);
  (* This part below is a boilerplate in every frontends. *)
  (* This could be moved in the cfg_infer module *)
  let source_dir = DB.source_dir_from_source_file !DB.current_source in
  let tenv_file = DB.source_dir_get_internal_file source_dir ".tenv" in
  let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
  Cfg.add_removetemps_instructions cfg;
  Preanal.doit cfg tenv;
  Cfg.add_abstraction_instructions cfg;
  Cg.store_to_file cg_file call_graph;
  Cfg.store_cfg_to_file cfg_file true cfg;
  (*Logging.out "Tenv %a@." Sil.pp_tenv tenv;*)
  (*Printing.print_tenv tenv;*)
  (*Printing.print_procedures cfg; *)
  Sil.store_tenv_to_file tenv_file tenv;
  if !CFrontend_config.stats_mode then Cfg.check_cfg_connectedness cfg;
  if !CFrontend_config.stats_mode || !CFrontend_config.debug_mode || !CFrontend_config.testing_mode then
    (Dotty.print_icfg_dotty cfg [];
     Cg.save_call_graph_dotty None Specs.get_specs call_graph)

