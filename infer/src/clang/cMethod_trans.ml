(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Methods for creating a procdesc from a method or function declaration
    and for resolving a method call and finding the right callee *)

open CFrontend_utils

module L = Logging

exception Invalid_declaration

(** When the methoc call is MCStatic, means that it is a class method.  When it is MCVirtual, it
    means that it is an instance method and that the method to be called will be determined at
    runtime. If it is MCNoVirtual it means that it is an instance method but that the method to be
    called will be determined at compile time *)
type method_call_type =
  | MCVirtual
  | MCNoVirtual
  | MCStatic

type function_method_decl_info =
  | Func_decl_info of Clang_ast_t.function_decl_info * Clang_ast_t.type_ptr
  | Cpp_Meth_decl_info of Clang_ast_t.function_decl_info * Clang_ast_t.cxx_method_decl_info * Clang_ast_t.pointer * Clang_ast_t.type_ptr
  | ObjC_Meth_decl_info of Clang_ast_t.obj_c_method_decl_info * Clang_ast_t.pointer
  | Block_decl_info of Clang_ast_t.block_decl_info * Clang_ast_t.type_ptr * CContext.t

let is_instance_method function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info _  | Block_decl_info _ -> false
  | Cpp_Meth_decl_info (_, method_decl_info, _, _) ->
      not method_decl_info.Clang_ast_t.xmdi_is_static
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      method_decl_info.Clang_ast_t.omdi_is_instance_method

let get_original_return_type function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, typ)
  | Cpp_Meth_decl_info (_, _, _, typ)
  | Block_decl_info (_, typ, _) -> CTypes.return_type_of_function_type typ
  | ObjC_Meth_decl_info (method_decl_info, _) -> method_decl_info.Clang_ast_t.omdi_result_type

let get_class_param function_method_decl_info =
  if (is_instance_method function_method_decl_info) then
    match function_method_decl_info with
    | Cpp_Meth_decl_info (_, _, class_decl_ptr, _) ->
        let class_type = Ast_expressions.create_qual_type (`DeclPtr class_decl_ptr) in
        [(Mangled.from_string CFrontend_config.this, class_type)]
    | ObjC_Meth_decl_info (_, class_decl_ptr) ->
        let class_type = Ast_expressions.create_qual_type (`DeclPtr class_decl_ptr) in
        [(Mangled.from_string CFrontend_config.self, class_type)]
    | _ -> []
  else []


let should_add_return_param return_type ~is_objc_method =
  match return_type with
  | Typ.Tstruct _ -> not is_objc_method
  | _ -> false

let is_objc_method function_method_decl_info =
  match function_method_decl_info with
  | ObjC_Meth_decl_info _ -> true
  | _ -> false

let get_return_param tenv function_method_decl_info =
  let is_objc_method = is_objc_method function_method_decl_info in
  let return_type_ptr = get_original_return_type function_method_decl_info in
  let return_typ = CTypes_decl.type_ptr_to_sil_type tenv return_type_ptr in
  if should_add_return_param return_typ ~is_objc_method then
    [(Mangled.from_string CFrontend_config.return_param,
      Ast_expressions.create_pointer_qual_type ~is_const:false return_type_ptr)]
  else
    []

let get_param_decls function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (function_decl_info, _)
  | Cpp_Meth_decl_info (function_decl_info, _, _, _) ->
      function_decl_info.Clang_ast_t.fdi_parameters
  | ObjC_Meth_decl_info (method_decl_info, _) -> method_decl_info.Clang_ast_t.omdi_parameters
  | Block_decl_info (block_decl_info, _, _) -> block_decl_info.Clang_ast_t.bdi_parameters

let get_language trans_unit_ctx function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, _) -> trans_unit_ctx.CFrontend_config.lang
  | Cpp_Meth_decl_info _ -> CFrontend_config.CPP
  | ObjC_Meth_decl_info _ -> CFrontend_config.ObjC
  | Block_decl_info _ -> CFrontend_config.ObjC

let is_cpp_virtual function_method_decl_info =
  match function_method_decl_info with
  | Cpp_Meth_decl_info (_, mdi, _, _) -> mdi.Clang_ast_t.xmdi_is_virtual
  | _ -> false

(** Returns parameters of a function/method. They will have following order:
    1. self/this parameter (optional, only for methods)
    2. normal parameters
    3. return parameter (optional) *)
let get_parameters trans_unit_ctx tenv function_method_decl_info =
  let par_to_ms_par par =
    match par with
    | Clang_ast_t.ParmVarDecl (_, name_info, qt, var_decl_info) ->
        let _, mangled = General_utils.get_var_name_mangled name_info var_decl_info in
        let param_typ = CTypes_decl.type_ptr_to_sil_type tenv qt.Clang_ast_t.qt_type_ptr in
        let qt_type_ptr =
          match param_typ with
          | Typ.Tstruct _ when General_utils.is_cpp_translation trans_unit_ctx ->
              Ast_expressions.create_reference_type qt.Clang_ast_t.qt_type_ptr
          | _ -> qt.Clang_ast_t.qt_type_ptr in
        (mangled, {qt with qt_type_ptr})
    | _ -> assert false in
  let pars = IList.map par_to_ms_par (get_param_decls function_method_decl_info) in
  get_class_param function_method_decl_info @ pars @ get_return_param tenv function_method_decl_info

(** get return type of the function and optionally type of function's return parameter *)
let get_return_val_and_param_types tenv function_method_decl_info =
  let return_type_ptr = get_original_return_type function_method_decl_info in
  let return_typ = CTypes_decl.type_ptr_to_sil_type tenv return_type_ptr in
  let is_objc_method = is_objc_method function_method_decl_info in
  if should_add_return_param return_typ ~is_objc_method then
    Ast_expressions.create_void_type, Some (Typ.Tptr (return_typ, Typ.Pk_pointer))
  else return_type_ptr, None

let build_method_signature trans_unit_ctx tenv decl_info procname function_method_decl_info
    parent_pointer pointer_to_property_opt =
  let source_range = decl_info.Clang_ast_t.di_source_range in
  let tp, return_param_type_opt = get_return_val_and_param_types tenv function_method_decl_info in
  let is_instance_method = is_instance_method function_method_decl_info in
  let parameters = get_parameters trans_unit_ctx tenv function_method_decl_info in
  let attributes = decl_info.Clang_ast_t.di_attributes in
  let lang = get_language trans_unit_ctx function_method_decl_info in
  let is_cpp_virtual = is_cpp_virtual function_method_decl_info in
  CMethod_signature.make_ms
    procname parameters tp attributes source_range is_instance_method ~is_cpp_virtual:is_cpp_virtual
    lang parent_pointer pointer_to_property_opt return_param_type_opt

let get_assume_not_null_calls param_decls =
  let do_one_param decl = match decl with
    | Clang_ast_t.ParmVarDecl (decl_info, name, qt, _)
      when CFrontend_utils.Ast_utils.is_type_nonnull qt.Clang_ast_t.qt_type_ptr ->
        let assume_call = Ast_expressions.create_assume_not_null_call
            decl_info name qt.Clang_ast_t.qt_type_ptr in
        [(`ClangStmt assume_call)]
    | _ -> [] in
  IList.flatten (IList.map do_one_param param_decls)

let get_init_list_instrs method_decl_info =
  let create_custom_instr construct_instr = `CXXConstructorInit construct_instr in
  IList.map create_custom_instr method_decl_info.Clang_ast_t.xmdi_cxx_ctor_initializers

let method_signature_of_decl trans_unit_ctx tenv meth_decl block_data_opt =
  let open Clang_ast_t in
  match meth_decl, block_data_opt with
  | FunctionDecl (decl_info, _, qt, fdi), _ ->
      let func_decl = Func_decl_info (fdi, qt.Clang_ast_t.qt_type_ptr) in
      let procname = General_utils.procname_of_decl trans_unit_ctx meth_decl in
      let ms = build_method_signature trans_unit_ctx tenv decl_info procname func_decl None None in
      let extra_instrs = get_assume_not_null_calls fdi.Clang_ast_t.fdi_parameters in
      ms, fdi.Clang_ast_t.fdi_body, extra_instrs
  | CXXMethodDecl (decl_info, _, qt, fdi, mdi), _
  | CXXConstructorDecl (decl_info, _, qt, fdi, mdi), _
  | CXXConversionDecl (decl_info, _, qt, fdi, mdi), _
  | CXXDestructorDecl (decl_info, _, qt, fdi, mdi), _ ->
      let procname = General_utils.procname_of_decl trans_unit_ctx meth_decl in
      let parent_ptr = Option.get decl_info.di_parent_pointer in
      let method_decl = Cpp_Meth_decl_info (fdi, mdi, parent_ptr, qt.Clang_ast_t.qt_type_ptr)  in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let ms = build_method_signature
          trans_unit_ctx tenv decl_info procname method_decl parent_pointer None in
      let non_null_instrs = get_assume_not_null_calls fdi.Clang_ast_t.fdi_parameters in
      let init_list_instrs = get_init_list_instrs mdi in (* it will be empty for methods *)
      ms, fdi.Clang_ast_t.fdi_body, (init_list_instrs @ non_null_instrs)
  | ObjCMethodDecl (decl_info, _, mdi), _ ->
      let procname = General_utils.procname_of_decl trans_unit_ctx meth_decl in
      let parent_ptr = Option.get decl_info.di_parent_pointer in
      let method_decl = ObjC_Meth_decl_info (mdi, parent_ptr) in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let pointer_to_property_opt =
        match mdi.Clang_ast_t.omdi_property_decl with
        | Some decl_ref -> Some decl_ref.Clang_ast_t.dr_decl_pointer
        | None -> None in
      let ms = build_method_signature trans_unit_ctx tenv decl_info procname method_decl
          parent_pointer pointer_to_property_opt in
      let extra_instrs = get_assume_not_null_calls mdi.omdi_parameters in
      ms, mdi.omdi_body, extra_instrs
  | BlockDecl (decl_info, bdi), Some (outer_context, tp, procname, _) ->
      let func_decl = Block_decl_info (bdi, tp, outer_context) in
      let ms = build_method_signature trans_unit_ctx tenv decl_info procname func_decl None None in
      let extra_instrs = get_assume_not_null_calls bdi.bdi_parameters in
      ms, bdi.bdi_body, extra_instrs
  | _ -> raise Invalid_declaration

let method_signature_of_pointer trans_unit_ctx tenv pointer =
  try
    match Ast_utils.get_decl pointer with
    | Some meth_decl ->
        let ms, _, _ = method_signature_of_decl trans_unit_ctx tenv meth_decl None in
        Some ms
    | None -> None
  with Invalid_declaration -> None

let get_method_name_from_clang tenv ms_opt =
  match ms_opt with
  | Some ms ->
      (match Ast_utils.get_decl_opt (CMethod_signature.ms_get_pointer_to_parent ms) with
       | Some decl ->
           if ObjcProtocol_decl.is_protocol decl then None
           else
             (ignore (CTypes_decl.add_types_from_decl_to_tenv tenv decl);
              match ObjcCategory_decl.get_base_class_name_from_category decl with
              | Some class_name ->
                  let procname = CMethod_signature.ms_get_name ms in
                  let new_procname = Procname.replace_class procname class_name in
                  CMethod_signature.ms_set_name ms new_procname;
                  Some ms
              | None -> Some ms)
       | None -> Some ms)
  | None -> None

let get_superclass_curr_class_objc context =
  let retrive_super cname super_opt =
    let iname = Typename.TN_csu (Csu.Class Csu.Objc, Mangled.from_string cname) in
    Logging.out_debug "Checking for superclass = '%s'\n\n%!" (Typename.to_string iname);
    match Tenv.lookup (CContext.get_tenv context) iname with
    | Some { supers = super_name :: _ } ->
        Typename.name super_name
    | _ ->
        Logging.err_debug "NOT FOUND superclass = '%s'\n\n%!" (Typename.to_string iname);
        (match super_opt with
         | Some super -> super
         | _ -> assert false) in
  match CContext.get_curr_class context with
  | CContext.ContextCls (cname, super_opt, _) ->
      retrive_super cname super_opt
  | CContext.ContextCategory (_, cls) ->
      retrive_super cls None
  | CContext.ContextNoCls
  | CContext.ContextClsDeclPtr _
  | CContext.ContextProtocol _ -> assert false

(* Gets the class name from a method signature found by clang, if search is successful *)
let get_class_name_method_call_from_clang trans_unit_ctx tenv obj_c_message_expr_info =
  match obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
  | Some pointer ->
      (match method_signature_of_pointer trans_unit_ctx tenv pointer with
       | Some ms ->
           begin
             match CMethod_signature.ms_get_name ms with
             | Procname.ObjC_Cpp objc_cpp ->
                 Some (Procname.objc_cpp_get_class_name objc_cpp)
             | _ ->
                 None
           end
       | None -> None)
  | None -> None

(* Get class name from a method call accorsing to the info given by the receiver kind  *)
let get_class_name_method_call_from_receiver_kind context obj_c_message_expr_info act_params =
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Class tp ->
      let sil_type = CTypes_decl.type_ptr_to_sil_type context.CContext.tenv tp in
      (CTypes.classname_of_type sil_type)
  | `Instance ->
      (match act_params with
       | (_, Typ.Tptr(t, _)):: _
       | (_, t):: _ -> CTypes.classname_of_type t
       | _ -> assert false)
  | `SuperInstance ->get_superclass_curr_class_objc context
  | `SuperClass -> get_superclass_curr_class_objc context

let get_objc_method_data obj_c_message_expr_info =
  let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
  let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Instance -> (selector, pointer, MCVirtual)
  | `SuperInstance -> (selector, pointer, MCNoVirtual)
  | `Class _
  | `SuperClass -> (selector, pointer, MCStatic)

let skip_property_accessor ms =
  let open Clang_ast_t in
  let pointer_to_property_opt = CMethod_signature.ms_get_pointer_to_property_opt ms in
  match Ast_utils.get_decl_opt pointer_to_property_opt with
  | Some (ObjCPropertyDecl _) -> true
  | _ -> false

let get_formal_parameters tenv ms =
  let rec defined_parameters pl =
    match pl with
    | [] -> []
    | (mangled, {Clang_ast_t.qt_type_ptr}):: pl' ->
        let should_add_pointer name ms =
          let is_objc_self = name = CFrontend_config.self &&
                             CMethod_signature.ms_get_lang ms = CFrontend_config.ObjC in
          let is_cxx_this = name = CFrontend_config.this &&
                            CMethod_signature.ms_get_lang ms = CFrontend_config.CPP in
          (is_objc_self && CMethod_signature.ms_is_instance ms) || is_cxx_this in
        let tp = if should_add_pointer (Mangled.to_string mangled) ms then
            (Ast_expressions.create_pointer_type qt_type_ptr)
          else qt_type_ptr in
        let typ = CTypes_decl.type_ptr_to_sil_type tenv tp in
        (mangled, typ):: defined_parameters pl' in
  defined_parameters (CMethod_signature.ms_get_args ms)

let get_return_type tenv ms =
  let return_type = CMethod_signature.ms_get_ret_type ms in
  CTypes_decl.type_ptr_to_sil_type tenv return_type

let sil_func_attributes_of_attributes attrs =
  let rec do_translation acc al = match al with
    | [] -> IList.rev acc
    | Clang_ast_t.SentinelAttr attribute_info:: tl ->
        let (sentinel, null_pos) = match attribute_info.Clang_ast_t.ai_parameters with
          | a:: b::[] -> (int_of_string a, int_of_string b)
          | _ -> assert false
        in
        do_translation (PredSymb.FA_sentinel(sentinel, null_pos):: acc) tl
    | _:: tl -> do_translation acc tl in
  do_translation [] attrs

let should_create_procdesc cfg procname defined =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some previous_procdesc ->
      let is_defined_previous = Cfg.Procdesc.is_defined previous_procdesc in
      if defined && (not is_defined_previous) then
        (Cfg.Procdesc.remove cfg (Cfg.Procdesc.get_proc_name previous_procdesc) true;
         true)
      else false
  | None -> true

let sil_method_annotation_of_args args : Annot.Method.t =
  let default_visibility = true in
  let mk_annot param_name annot_name =
    let annot = { Annot.class_name = annot_name; parameters = [param_name]; } in
    annot, default_visibility in
  let arg_to_sil_annot (arg_mangled, {Clang_ast_t.qt_type_ptr}) acc  =
    let arg_name = Mangled.to_string arg_mangled in
    if CFrontend_utils.Ast_utils.is_type_nullable qt_type_ptr then
      [mk_annot arg_name Annotations.nullable] :: acc
    else Annot.Item.empty::acc in
  let param_annots = IList.fold_right arg_to_sil_annot args []  in
  (* TODO: parse annotations on return value *)
  let retval_annot = [] in
  retval_annot, param_annots


let is_pointer_to_const type_ptr = match Ast_utils.get_type type_ptr with
  | Some PointerType (_, {Clang_ast_t.qt_is_const})
  | Some ObjCObjectPointerType (_, {Clang_ast_t.qt_is_const})
  | Some RValueReferenceType (_, {Clang_ast_t.qt_is_const})
  | Some LValueReferenceType (_, {Clang_ast_t.qt_is_const}) ->
      qt_is_const
  | _ ->
      false

(** Returns a list of the indices of expressions in [args] which point to const-typed values. Each
    index is offset by [shift]. *)
let get_const_args_indices ~shift args =
  let i = ref shift in
  let rec aux result = function
    | [] ->
        IList.rev result
    | (_, {Clang_ast_t.qt_type_ptr})::tl ->
        incr i;
        if is_pointer_to_const qt_type_ptr then
          aux (!i - 1::result) tl
        else
          aux result tl in
  aux [] args

(** Creates a procedure description. *)
let create_local_procdesc trans_unit_ctx cfg tenv ms fbody captured is_objc_inst_method =
  let defined = not ((IList.length fbody) == 0) in
  let proc_name = CMethod_signature.ms_get_name ms in
  let pname = Procname.to_string proc_name in
  let attributes = sil_func_attributes_of_attributes (CMethod_signature.ms_get_attributes ms) in
  let method_annotation =
    sil_method_annotation_of_args (CMethod_signature.ms_get_args ms) in
  let is_cpp_inst_method = CMethod_signature.ms_is_instance ms
                           && CMethod_signature.ms_get_lang ms = CFrontend_config.CPP in
  let create_new_procdesc () =
    let formals = get_formal_parameters tenv ms in
    let captured_mangled = IList.map (fun (var, t) -> (Pvar.get_name var), t) captured in
    (* Captured variables for blocks are treated as parameters *)
    let formals = captured_mangled @ formals in
    let const_formals = get_const_args_indices
        ~shift:(IList.length captured_mangled)
        (CMethod_signature.ms_get_args ms) in
    let source_range = CMethod_signature.ms_get_loc ms in
    Logging.out_debug "\nCreating a new procdesc for function: '%s'\n@." pname;
    Logging.out_debug "\nms = %s\n@." (CMethod_signature.ms_to_string ms);
    let loc_start = CLocation.get_sil_location_from_range trans_unit_ctx source_range true in
    let loc_exit = CLocation.get_sil_location_from_range trans_unit_ctx source_range false in
    let ret_type = get_return_type tenv ms in
    if skip_property_accessor ms then ()
    else
      let procdesc =
        let proc_attributes =
          { (ProcAttributes.default proc_name Config.Clang) with
            ProcAttributes.captured = captured_mangled;
            formals;
            const_formals;
            func_attributes = attributes;
            is_defined = defined;
            is_objc_instance_method = is_objc_inst_method;
            is_cpp_instance_method = is_cpp_inst_method;
            loc = loc_start;
            translation_unit = Some trans_unit_ctx.CFrontend_config.source_file;
            method_annotation;
            ret_type;
          } in
        Cfg.Procdesc.create cfg proc_attributes in
      if defined then
        (if !Config.arc_mode then
           Cfg.Procdesc.set_flag procdesc Mleak_buckets.objc_arc_flag "true";
         let start_kind = Cfg.Node.Start_node proc_name in
         let start_node = Cfg.Node.create cfg loc_start start_kind [] procdesc in
         let exit_kind = Cfg.Node.Exit_node proc_name in
         let exit_node = Cfg.Node.create cfg loc_exit exit_kind [] procdesc in
         Cfg.Procdesc.set_start_node procdesc start_node;
         Cfg.Procdesc.set_exit_node procdesc exit_node) in
  if should_create_procdesc cfg proc_name defined then
    (create_new_procdesc (); true)
  else false

(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc cfg proc_name is_objc_inst_method type_opt =
  match Cfg.Procdesc.find_from_name cfg proc_name with
  | Some _ -> ()
  | None ->
      let ret_type, formals =
        (match type_opt with
         | Some (ret_type, arg_types) ->
             ret_type, IList.map (fun typ -> (Mangled.from_string "x", typ)) arg_types
         | None -> Typ.Tvoid, []) in
      let loc = Location.dummy in
      let proc_attributes =
        { (ProcAttributes.default proc_name Config.Clang) with
          ProcAttributes.formals;
          is_objc_instance_method = is_objc_inst_method;
          loc;
          ret_type;
        } in
      ignore (Cfg.Procdesc.create cfg proc_attributes)

let create_procdesc_with_pointer context pointer class_name_opt name =
  let open CContext in
  match method_signature_of_pointer context.translation_unit_context context.tenv pointer with
  | Some callee_ms ->
      ignore (create_local_procdesc context.translation_unit_context context.cfg context.tenv
                callee_ms [] [] false);
      CMethod_signature.ms_get_name callee_ms
  | None ->
      let callee_name =
        match class_name_opt with
        | Some class_name ->
            General_utils.mk_procname_from_cpp_method class_name name None
        | None ->
            General_utils.mk_procname_from_function context.translation_unit_context name None in
      create_external_procdesc context.cfg callee_name false None;
      callee_name

let add_default_method_for_class trans_unit_ctx class_name decl_info =
  let loc = CLocation.get_sil_location_from_range trans_unit_ctx
      decl_info.Clang_ast_t.di_source_range true in
  let proc_name = Procname.get_default_objc_class_method class_name in
  let attrs = { (ProcAttributes.default proc_name Config.Clang) with loc = loc; } in
  AttributesTable.store_attributes attrs

let get_procname_from_cpp_lambda context dec =
  match dec with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rdi) ->
      (match cxx_rdi.xrdi_lambda_call_operator with
       | Some dr ->
           let name_info, decl_ptr, _ = Ast_utils.get_info_from_decl_ref dr in
           create_procdesc_with_pointer context decl_ptr None name_info.ni_name
       | _ -> assert false (* We should not get here *))
  | _ -> assert false (* We should not get here *)

(*
let instance_to_method_call_type instance =
  if instance then MCVirtual
  else MCStatic
*)
