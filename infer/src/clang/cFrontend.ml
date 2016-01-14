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
let rec translate_one_declaration tenv cg cfg parent_dec dec =
  let open Clang_ast_t in
  (* each procedure has different scope: start names from id 0 *)
  Ident.NameGenerator.reset ();
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.update_curr_file info;
  let source_range = info.Clang_ast_t.di_source_range in
  let should_translate_decl = CLocation.should_translate_lib source_range in
  (if should_translate_decl then
     match dec with
     | FunctionDecl(di, name_info, tp, fdecl_info) ->
         CMethod_declImpl.function_decl tenv cfg cg dec None

     | ObjCInterfaceDecl(decl_info, name_info, decl_list, decl_context_info, oi_decl_info) ->
         let name = Ast_utils.get_qualified_name name_info in
         let curr_class = ObjcInterface_decl.get_curr_class name oi_decl_info in
         ignore
           (ObjcInterface_decl.interface_declaration CTypes_decl.type_ptr_to_sil_type tenv dec);
         CMethod_declImpl.process_methods tenv cg cfg curr_class decl_list

     | ObjCProtocolDecl(decl_info, name_info, decl_list, decl_context_info, _) ->
         let name = Ast_utils.get_qualified_name name_info in
         let curr_class = CContext.ContextProtocol name in
         ignore (ObjcProtocol_decl.protocol_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
         CMethod_declImpl.process_methods tenv cg cfg curr_class decl_list

     | ObjCCategoryDecl(decl_info, name_info, decl_list, decl_context_info, ocdi) ->
         let name = Ast_utils.get_qualified_name name_info in
         let curr_class = ObjcCategory_decl.get_curr_class_from_category_decl name ocdi in
         ignore (ObjcCategory_decl.category_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
         CMethod_declImpl.process_methods tenv cg cfg curr_class decl_list

     | ObjCCategoryImplDecl(decl_info, name_info, decl_list, decl_context_info, ocidi) ->
         let name = Ast_utils.get_qualified_name name_info in
         let curr_class = ObjcCategory_decl.get_curr_class_from_category_impl name ocidi in
         ignore (ObjcCategory_decl.category_impl_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
         CMethod_declImpl.process_methods tenv cg cfg curr_class decl_list;
         (match Ast_utils.get_decl_opt_with_decl_ref ocidi.Clang_ast_t.ocidi_category_decl with
          | Some ObjCCategoryDecl(_, _, cat_decl_list, _, _) ->
              let name = CContext.get_curr_class_name curr_class in
              let decls = cat_decl_list @ decl_list in
              CFrontend_errors.check_for_property_errors cfg cg tenv name decls
          | _ -> ())

     | ObjCImplementationDecl(decl_info, name_info, decl_list, decl_context_info, idi) ->
         let curr_class = ObjcInterface_decl.get_curr_class_impl idi in
         let type_ptr_to_sil_type = CTypes_decl.type_ptr_to_sil_type in
         ignore (ObjcInterface_decl.interface_impl_declaration type_ptr_to_sil_type tenv dec);
         CMethod_declImpl.process_methods tenv cg cfg curr_class decl_list;
         (match Ast_utils.get_decl_opt_with_decl_ref idi.Clang_ast_t.oidi_class_interface with
          | Some ObjCInterfaceDecl(_, _, cl_decl_list, _, _) ->
              let name = CContext.get_curr_class_name curr_class in
              let decls = cl_decl_list @ decl_list in
              CFrontend_errors.check_for_property_errors cfg cg tenv name decls
          | _ -> ())

     | CXXMethodDecl (decl_info, name_info, type_ptr, function_decl_info, _)
     | CXXConstructorDecl (decl_info, name_info, type_ptr, function_decl_info, _)
     | CXXDestructorDecl (decl_info, name_info, type_ptr, function_decl_info, _) ->
         (* di_parent_pointer has pointer to lexical context such as class.*)
         (* If it's not defined, then it's the same as parent in AST *)
         let class_decl = match decl_info.Clang_ast_t.di_parent_pointer with
           | Some ptr -> Ast_utils.get_decl ptr
           | None -> Some parent_dec in
         (match class_decl with
          | Some (CXXRecordDecl _ as d)
          | Some (ClassTemplateSpecializationDecl _ as d) ->
              let class_name = CTypes_decl.get_record_name d in
              let curr_class = CContext.ContextCls(class_name, None, []) in
              if !CFrontend_config.testing_mode then
                CMethod_declImpl.process_methods tenv cg cfg curr_class [dec]
          | Some dec -> Printing.log_stats "Methods of %s skipped\n" (Ast_utils.string_of_decl dec)
          | None -> ())
     | dec -> ());
  match dec with
  (* Currently C/C++ record decl treated in the same way *)
  | ClassTemplateSpecializationDecl (decl_info, _, _, _, decl_list, _, _, _)
  | CXXRecordDecl (decl_info, _, _, _, decl_list, _, _, _)
  | RecordDecl (decl_info, _, _, _, decl_list, _, _) when not decl_info.di_is_implicit ->
      let is_method_decl decl = match decl with
        | CXXMethodDecl _ | CXXConstructorDecl _ | CXXDestructorDecl _ | FunctionTemplateDecl _ ->
            true
        | _ -> false in
      let method_decls, no_method_decls = IList.partition is_method_decl decl_list in
      IList.iter (translate_one_declaration tenv cg cfg dec) no_method_decls;
      ignore (CTypes_decl.add_types_from_decl_to_tenv tenv dec);
      IList.iter (translate_one_declaration tenv cg cfg dec) method_decls
  | EnumDecl _ -> ignore (CEnum_decl.enum_decl dec)
  | LinkageSpecDecl (decl_info, decl_list, decl_context_info) ->
      Printing.log_out "ADDING: LinkageSpecDecl decl list\n";
      IList.iter (translate_one_declaration tenv cg cfg dec) decl_list
  | NamespaceDecl (decl_info, name_info, decl_list, decl_context_info, _) ->
      IList.iter (translate_one_declaration tenv cg cfg dec) decl_list
  | ClassTemplateDecl (decl_info, named_decl_info, template_decl_info)
  | FunctionTemplateDecl (decl_info, named_decl_info, template_decl_info) ->
      let decl_list = template_decl_info.Clang_ast_t.tdi_specializations in
      IList.iter (translate_one_declaration tenv cg cfg dec) decl_list
  | dec -> ()

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg tenv source_file ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list;
      Printing.log_out "\n Start creating icfg\n";
      let cg = Cg.create () in
      let cfg = Cfg.Node.create_cfg () in
      IList.iter (translate_one_declaration tenv cg cfg ast) decl_list;
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
  let tenv = Sil.create_tenv () in
  CTypes_decl.add_predefined_types tenv;
  init_global_state source_file;
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
  (* Printing.print_tenv tenv; *)
  (*Printing.print_procedures cfg; *)
  Sil.store_tenv_to_file tenv_file tenv;
  if !CFrontend_config.stats_mode then Cfg.check_cfg_connectedness cfg;
  if !CFrontend_config.stats_mode || !CFrontend_config.debug_mode || !CFrontend_config.testing_mode then
    (Dotty.print_icfg_dotty cfg [];
     Cg.save_call_graph_dotty None Specs.get_specs call_graph)

