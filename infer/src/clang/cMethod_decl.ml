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
open Clang_ast_t
open CContext

module L = Logging

module type CMethod_decl = sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class -> string option ->
    Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> Clang_ast_t.decl ->
    (Clang_ast_t.qual_type * bool * Procname.t * (Mangled.t * Sil.typ * bool) list) option ->
    CContext.curr_class -> unit

  val process_getter_setter : CContext.t -> Procname.t -> bool
end

module CMethod_decl_funct(T: CModule_type.CTranslation) : CMethod_decl =
struct

  let method_body_to_translate ms body =
    match body with
    | Some body ->
        if not (CLocation.should_translate_lib (CMethod_signature.ms_get_loc ms))
        then None else Some body
    | None -> body

  let model_exists procname =
    Specs.summary_exists_in_models procname && not !CFrontend_config.models_mode

  let rec add_assume_not_null_calls param_decls attributes =
    match param_decls with
    | [] -> []
    | decl:: rest ->
        let rest_assume_calls = add_assume_not_null_calls rest attributes in
        (match decl with
         | ParmVarDecl(decl_info, name_info, qtype, var_decl_info)
           when CFrontend_utils.Ast_utils.is_type_nonnull qtype attributes ->
             let name = name_info.Clang_ast_t.ni_name in
             let assume_call = Ast_expressions.create_assume_not_null_call decl_info name qtype in
             assume_call:: rest_assume_calls
         | _ -> rest_assume_calls)

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method tenv cg cfg class_decl_opt procname namespace instrs is_objc_method is_instance
      captured_vars is_anonym_block param_decls attributes =
    Printing.log_out
      "\n\n>>---------- ADDING METHOD: '%s' ---------<<\n@." (Procname.to_string procname);
    try
      (match Cfg.Procdesc.find_from_name cfg procname with
       | Some procdesc ->
           if (Cfg.Procdesc.is_defined procdesc && not (model_exists procname)) then
             (let context =
                CContext.create_context tenv cg cfg procdesc namespace class_decl_opt
                  is_objc_method captured_vars is_instance in
              CVar_decl.get_fun_locals context instrs;
              let local_vars = list_map (fun (n, t, _) -> n, t) context.CContext.local_vars in
              let start_node = Cfg.Procdesc.get_start_node procdesc in
              let exit_node = Cfg.Procdesc.get_exit_node procdesc in
              Cfg.Procdesc.append_locals procdesc local_vars;
              Cfg.Node.add_locals_ret_declaration start_node local_vars;
              Printing.log_out
                "\n\n>>---------- Start translating body of function: '%s' ---------<<\n@."
                (Procname.to_string procname);
              let nonnull_assume_calls = add_assume_not_null_calls param_decls in
              let instrs' = instrs@nonnull_assume_calls attributes in
              let meth_body_nodes = T.instructions_trans context instrs' exit_node in
              if (not is_anonym_block) then CContext.LocalVars.reset_block ();
              Cfg.Node.set_succs_exn start_node meth_body_nodes [];
              Cg.add_node (CContext.get_cg context) (Cfg.Procdesc.get_proc_name procdesc))
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

  let function_decl tenv cfg cg namespace func_decl block_data_opt curr_class =
    Printing.log_out "\nResetting the goto_labels hashmap...\n";
    CTrans_utils.GotoLabel.reset_all_labels (); (* C Language Std 6.8.6.1-1 *)
    let ms, body_opt, param_decls =
      CMethod_trans.method_signature_of_decl curr_class func_decl block_data_opt in
    match method_body_to_translate ms body_opt with
    | Some body -> (* Only in the case the function declaration has a defined body we create a procdesc *)
        let procname = CMethod_signature.ms_get_name ms in
        let is_anonym_block, captured_vars =
          match block_data_opt with
          | Some (_, _, _, captured_vars) -> true, captured_vars
          | None -> false, [] in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] captured_vars false then
          let is_instance = CMethod_signature.ms_is_instance ms in
          let is_objc_method = is_anonym_block in
          let curr_class = if is_anonym_block then curr_class else CContext.ContextNoCls in
          let attributes = CMethod_signature.ms_get_attributes ms in
          CMethod_signature.add ms;
          add_method tenv cg cfg curr_class procname namespace [body] is_objc_method is_instance
            captured_vars is_anonym_block param_decls attributes
    | None -> CMethod_signature.add ms

  let process_method_decl tenv cg cfg namespace curr_class meth_decl ~is_objc =
    let ms, body_opt, param_decls =
      CMethod_trans.method_signature_of_decl curr_class meth_decl None in
    CMethod_signature.add ms;
    match method_body_to_translate ms body_opt with
    | Some body ->
        let is_instance = CMethod_signature.ms_is_instance ms in
        let attributes = CMethod_signature.ms_get_attributes ms in
        let procname = CMethod_signature.ms_get_name ms in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] [] is_instance then
          add_method tenv cg cfg curr_class procname namespace [body] is_objc is_instance [] false
            param_decls attributes
    | None -> ()

  let rec process_one_method_decl tenv cg cfg curr_class namespace dec =
    match dec with
    | CXXMethodDecl _ ->
        process_method_decl tenv cg cfg namespace curr_class dec ~is_objc:false
    | ObjCMethodDecl _ ->
        process_method_decl tenv cg cfg namespace curr_class dec ~is_objc:true
    | ObjCPropertyImplDecl (decl_info, property_impl_decl_info) ->
        let pname = Ast_utils.property_name property_impl_decl_info in
        Printing.log_out "ADDING: ObjCPropertyImplDecl for property '%s' " pname;
        let getter_setter = ObjcProperty_decl.make_getter_setter curr_class decl_info pname in
        list_iter (process_one_method_decl tenv cg cfg curr_class namespace) getter_setter
    | EmptyDecl _ | ObjCIvarDecl _ | ObjCPropertyDecl _ -> ()
    | _ ->
        Printing.log_stats
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n" (Ast_utils.string_of_decl dec);
        ()

  let process_methods tenv cg cfg curr_class namespace decl_list =
    list_iter (process_one_method_decl tenv cg cfg curr_class namespace) decl_list

  let process_getter_setter context procname =
    let class_name = Procname.c_get_class procname in
    let cls = CContext.create_curr_class context.tenv class_name in
    let method_name = Procname.c_get_method procname in
    match ObjcProperty_decl.method_is_property_accesor cls method_name with
    | Some (property_name, property_type, is_getter) when
        CMethod_trans.should_create_procdesc context.cfg procname true true ->
        (match property_type with qt, atts, decl_info, _, _, ivar_opt ->
          let ivar_name = ObjcProperty_decl.get_ivar_name property_name ivar_opt in
          let field = CField_decl.build_sil_field_property cls context.tenv ivar_name qt (Some atts) in
          ignore (CField_decl.add_missing_fields context.tenv class_name [field]);
          let accessor =
            if is_getter then
              ObjcProperty_decl.make_getter cls property_name property_type
            else ObjcProperty_decl.make_setter cls property_name property_type in
          list_iter (process_one_method_decl context.tenv context.cg context.cfg cls context.namespace) accessor;
          true)
    | _ -> false

end
