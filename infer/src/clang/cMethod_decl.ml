(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Process methods or functions declarations by adding them to the cfg. *)

open Utils
open CFrontend_utils

module L = Logging

module type CMethod_decl = sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class ->
    Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit

  val process_getter_setter : CContext.t -> Procname.t -> bool
end

module CMethod_decl_funct(T: CModule_type.CTranslation) : CMethod_decl =
struct

  let model_exists procname =
    Specs.summary_exists_in_models procname && not !CFrontend_config.models_mode

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method tenv cg cfg class_decl_opt procname instrs is_objc_method
      captured_vars outer_context_opt extra_instrs =

    Printing.log_out
      "\n\n>>---------- ADDING METHOD: '%s' ---------<<\n@." (Procname.to_string procname);
    try
      (match Cfg.Procdesc.find_from_name cfg procname with
       | Some procdesc ->
           if (Cfg.Procdesc.is_defined procdesc && not (model_exists procname)) then
             (let context =
                CContext.create_context tenv cg cfg procdesc class_decl_opt
                  is_objc_method outer_context_opt in
              let start_node = Cfg.Procdesc.get_start_node procdesc in
              let exit_node = Cfg.Procdesc.get_exit_node procdesc in
              Printing.log_out
                "\n\n>>---------- Start translating body of function: '%s' ---------<<\n@."
                (Procname.to_string procname);
              let meth_body_nodes = T.instructions_trans context instrs extra_instrs exit_node in
              Cfg.Node.add_locals_ret_declaration start_node (Cfg.Procdesc.get_locals procdesc);
              Cfg.Node.set_succs_exn start_node meth_body_nodes [];
              Cg.add_defined_node (CContext.get_cg context) (Cfg.Procdesc.get_proc_name procdesc))
       | None -> ())
    with
    | Not_found -> ()
    | CTrans_utils.Self.SelfClassException _ ->
        assert false (* this shouldn't happen, because self or [a class] should always be arguments of functions. This is to make sure I'm not wrong. *)
    | Assert_failure (file, line, column) ->
        print_endline ("Fatal error: exception Assert_failure("^
                       file^", "^(string_of_int line)^", "^(string_of_int column)^")");
        Cfg.Procdesc.remove cfg procname true;
        CMethod_trans.create_external_procdesc cfg procname is_objc_method None;
        ()

  let function_decl tenv cfg cg func_decl block_data_opt =
    Printing.log_out "\nResetting the goto_labels hashmap...\n";
    CTrans_utils.GotoLabel.reset_all_labels (); (* C Language Std 6.8.6.1-1 *)
    let captured_vars, outer_context_opt =
      match block_data_opt with
      | Some (outer_context, _, _, captured_vars) -> captured_vars, Some outer_context
      | None -> [], None in
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl func_decl block_data_opt in
    match body_opt with
    | Some body -> (* Only in the case the function declaration has a defined body we create a procdesc *)
        let procname = CMethod_signature.ms_get_name ms in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] captured_vars false then
          add_method tenv cg cfg CContext.ContextNoCls procname [body] false
            captured_vars outer_context_opt extra_instrs
    | None -> ()

  let process_method_decl tenv cg cfg curr_class meth_decl ~is_objc =
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl meth_decl None in
    match body_opt with
    | Some body ->
        let is_instance = CMethod_signature.ms_is_instance ms in
        let procname = CMethod_signature.ms_get_name ms in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] [] is_instance then
          add_method tenv cg cfg curr_class procname [body] is_objc [] None extra_instrs
    | None -> ()

  let rec process_one_method_decl tenv cg cfg curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ ->
        process_method_decl tenv cg cfg curr_class dec ~is_objc:false
    | ObjCMethodDecl _ ->
        process_method_decl tenv cg cfg curr_class dec ~is_objc:true
    | ObjCPropertyImplDecl (decl_info, property_impl_decl_info) ->
        let pname = Ast_utils.property_name property_impl_decl_info in
        Printing.log_out "ADDING: ObjCPropertyImplDecl for property '%s' "
          pname.Clang_ast_t.ni_name;
        let setter = ObjcProperty_decl.make_setter' curr_class decl_info pname in
        IList.iter (process_one_method_decl tenv cg cfg curr_class) setter
    | EmptyDecl _ | ObjCIvarDecl _ | ObjCPropertyDecl _ -> ()
    | _ ->
        Printing.log_stats
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n" (Ast_utils.string_of_decl dec);
        ()

  let process_methods tenv cg cfg curr_class decl_list =
    IList.iter (process_one_method_decl tenv cg cfg curr_class) decl_list

  let process_getter_setter context procname =
    (*If there is already a spec for the method we want to generate (in incremental analysis) *)
    (* we don't need to generate it again. *)
    if Specs.summary_exists procname then false
    else
      let class_name = Procname.c_get_class procname in
      let tenv = context.CContext.tenv in
      let cg = context.CContext.cg in
      let cfg = context.CContext.cfg in
      let cls = CContext.create_curr_class tenv class_name in
      let method_name = Procname.c_get_method procname in
      match ObjcProperty_decl.method_is_property_accesor cls method_name with
      | Some (property_name, property_type, is_getter) when
          CMethod_trans.should_create_procdesc cfg procname true true ->
          (match property_type with tp, atts, decl_info, _, _, ivar_opt ->
            let ivar_name = ObjcProperty_decl.get_ivar_name property_name ivar_opt in
            let field =
              CField_decl.build_sil_field_property CTypes_decl.type_ptr_to_sil_type cls tenv
                ivar_name tp (Some atts) in
            ignore (CField_decl.add_missing_fields tenv class_name [field]);
            let accessor_opt =
              if is_getter then []
              else ObjcProperty_decl.make_setter cls property_name property_type in
            match accessor_opt with
            | [accessor] ->
                let decl_info = Clang_ast_proj.get_decl_tuple accessor in
                if CLocation.should_translate_lib decl_info.Clang_ast_t.di_source_range then
                  (process_one_method_decl tenv cg cfg cls accessor;
                   true)
                else false
            | _ -> false)
      | _ -> false

end
