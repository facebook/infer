(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Process methods or functions declarations by adding them to the cfg. *)

open Utils
open CFrontend_utils
open Clang_ast_t

module L = Logging

module type CMethod_decl = sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class -> string option ->
  Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> bool -> Clang_ast_t.decl_info ->
  string -> Clang_ast_t.qual_type -> Clang_ast_t.function_decl_info -> (Mangled.t * Sil.typ * bool) list -> Procname.t option -> CContext.curr_class -> unit

  val create_function_signature : Clang_ast_t.decl_info -> Clang_ast_t.function_decl_info -> string ->
  Clang_ast_t.qual_type -> bool -> Procname.t option -> Clang_ast_t.stmt option * CMethod_signature.method_signature
end

module CMethod_decl_funct(T: CModule_type.CTranslation) : CMethod_decl =
struct

  let method_body_to_translate di ms body =
    match body with
    | Some body ->
        if not (CLocation.should_translate_lib (CMethod_signature.ms_get_loc ms))
        then None else Some body
    | None -> body

  type function_method_decl_info =
    | Func_decl_info of Clang_ast_t.function_decl_info * string
    | Meth_decl_info of Clang_ast_t.obj_c_method_decl_info * string

  let is_instance_method function_method_decl_info is_instance is_anonym_block =
    if is_anonym_block then is_instance
    else (
      match function_method_decl_info with
      | Func_decl_info (function_decl_info, _) -> false
      | Meth_decl_info (method_decl_info, _) ->
          method_decl_info.Clang_ast_t.omdi_is_instance_method)

  let get_parameters function_method_decl_info =
    let par_to_ms_par par =
      match par with
      | ParmVarDecl(decl_info, name, qtype, var_decl_info) ->
          Printing.log_out "Adding  param '%s' " name;
          Printing.log_out "with pointer %s@." decl_info.Clang_ast_t.di_pointer;
          (name, CTypes.get_type qtype)
      | _ -> assert false in
    match function_method_decl_info with
    | Func_decl_info (function_decl_info, _) ->
        list_map par_to_ms_par function_decl_info.Clang_ast_t.fdi_parameters
    | Meth_decl_info (method_decl_info, class_name) ->
        let pars = list_map par_to_ms_par method_decl_info.Clang_ast_t.omdi_parameters in
        if (is_instance_method function_method_decl_info false false) then
          ("self", class_name):: pars
        else pars

  let get_return_type function_method_decl_info =
    match function_method_decl_info with
    | Func_decl_info (_, qt) -> qt
    | Meth_decl_info (method_decl_info, _) ->
        let qt = method_decl_info.Clang_ast_t.omdi_result_type in
        CTypes.get_type qt

  let build_method_signature decl_info procname function_method_decl_info is_instance is_anonym_block =
    let source_range = decl_info.Clang_ast_t.di_source_range in
    let qt = get_return_type function_method_decl_info in
    let is_instance_method = is_instance_method function_method_decl_info is_instance is_anonym_block in
    let parameters = get_parameters function_method_decl_info in
    let attributes = decl_info.Clang_ast_t.di_attributes in
    CMethod_signature.make_ms procname parameters qt attributes source_range is_instance_method

  let create_function_signature di fdecl_info name qt is_instance anonym_block_opt =
    let procname, is_anonym_block =
      match anonym_block_opt with
      | Some block -> block, true
      | None -> CMethod_trans.mk_procname_from_function name (CTypes.get_type qt), false in
    let ms = build_method_signature di procname
        (Func_decl_info (fdecl_info, CTypes.get_type qt)) is_instance is_anonym_block in
    (match method_body_to_translate di ms fdecl_info.Clang_ast_t.fdi_body with
      | Some body -> Some body, ms
      | None -> None, ms)

  let model_exists procname =
    Specs.summary_exists_in_models procname && not !CFrontend_config.models_mode

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method tenv cg cfg class_decl_opt procname namespace instrs is_objc_method is_instance
      captured_vars is_anonym_block =
    Printing.log_out
      "\n\n>>---------- ADDING METHOD: '%s' ---------<<\n" (Procname.to_string procname);
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
                  "\n\n>>---------- Start translating the function: '%s' ---------<<"
                  (Procname.to_string procname);
                let meth_body_nodes = T.instructions_trans context instrs exit_node in
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

  let function_decl tenv cfg cg namespace is_instance di name qt fdecl_info captured_vars anonym_block_opt curr_class =
    Printing.log_out "\nFound FunctionDecl '%s'. Processing...\n" name;
    Printing.log_out "\nResetting the goto_labels hashmap...\n";
    CTrans_utils.GotoLabel.reset_all_labels (); (* C Language Std 6.8.6.1-1 *)
    match create_function_signature di fdecl_info name qt is_instance anonym_block_opt with
    | Some body, ms -> (* Only in the case the function declaration has a defined body we create a procdesc *)
        let procname = CMethod_signature.ms_get_name ms in
        CMethod_trans.create_local_procdesc cfg tenv ms [body] captured_vars false;
        let is_instance = CMethod_signature.ms_is_instance ms in
        let is_anonym_block = Option.is_some anonym_block_opt in
        let is_objc_method = is_anonym_block in
        let curr_class = if is_anonym_block then curr_class else CContext.ContextNoCls in
        add_method tenv cg cfg curr_class procname namespace [body] is_objc_method is_instance
          captured_vars is_anonym_block
    | None, ms ->
      CMethod_trans.create_local_procdesc cfg tenv ms [] captured_vars false;
      CMethod_signature.add ms

  let process_objc_method_decl tenv cg cfg namespace curr_class decl_info method_name method_decl_info =
    let class_name = CContext.get_curr_class_name curr_class in
    let procname = CMethod_trans.mk_procname_from_method class_name method_name in
    let method_decl = Meth_decl_info (method_decl_info, class_name) in
    let ms = build_method_signature decl_info procname method_decl false false in
    Printing.log_out " ....Processing implementation for method '%s'\n" (Procname.to_string procname);
    (match method_body_to_translate decl_info ms method_decl_info.Clang_ast_t.omdi_body with
      | Some body ->
          let is_instance = CMethod_signature.ms_is_instance ms in
          CMethod_trans.create_local_procdesc cfg tenv ms [body] [] is_instance;
          add_method tenv cg cfg curr_class procname namespace [body] true is_instance [] false
      | None ->
          CMethod_signature.add ms)

  let rec process_one_method_decl tenv cg cfg curr_class namespace dec =
    match dec with
    | ObjCMethodDecl(decl_info, method_name, method_decl_info) ->
        process_objc_method_decl tenv cg cfg namespace curr_class decl_info method_name method_decl_info

    | ObjCPropertyImplDecl(decl_info, property_impl_decl_info) ->
        let prop_methods = ObjcProperty_decl.make_getter_setter cfg curr_class decl_info property_impl_decl_info in
        list_iter (process_one_method_decl tenv cg cfg curr_class namespace) prop_methods

    | EmptyDecl _ | ObjCIvarDecl _ -> ()
    | d -> Printing.log_err
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n" (Ast_utils.string_of_decl d);
        ()

  let process_methods tenv cg cfg curr_class namespace decl_list =
    list_iter (process_one_method_decl tenv cg cfg curr_class namespace) decl_list

end
