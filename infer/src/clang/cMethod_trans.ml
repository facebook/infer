(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Methods for creating a procdesc from a method or function declaration *)
(**   and for resolving a method call and finding the right callee *)

open Utils
open CFrontend_utils
open CContext

module L = Logging

(** When the methoc call is MCStatic, means that it is a class method. *)
(** When it is MCVirtual, it means that it is an instance method and that *)
(** the method to be called will be determined at runtime. If it is MCNoVirtual *)
(** it means that it is an instance method but that the method to be called will *)
(** be determined at compile time *)
type method_call_type =
  | MCVirtual
  | MCNoVirtual
  | MCStatic

let mk_procname_from_function name type_name =
  let type_name_crc = CRC.crc16 type_name in
  Procname.mangled_cpp name type_name_crc

let mk_procname_from_method class_name method_name =
  Procname.mangled_objc class_name method_name

let rec resolve_method_class tenv class_name method_name =
  let type_name = Sil.TN_csu (Sil.Class, class_name) in
  match Sil.tenv_lookup tenv type_name with
  | Some (Sil.Tstruct (_, _, Sil.Class, cls, super_classes, methods, iann)) ->
      Some type_name
  | _ -> None

let resolve_method tenv class_name method_name =
  let class_name_mangled = Mangled.from_string class_name in
  match resolve_method_class tenv class_name_mangled method_name with
  | Some (Sil.TN_csu (Sil.Class, class_name)) ->
      let class_method_name = mk_procname_from_method (Mangled.to_string class_name) method_name in
      (try let ms = CMethod_signature.find class_method_name in
        Some ms
      with Not_found -> None)
  | _ -> None

let get_superclass_curr_class context =
  let retrive_super cname super_opt =
    let iname = Sil.TN_csu (Sil.Class, Mangled.from_string cname) in
    Printing.log_out "Checking for superclass = '%s'\n\n%!" (Sil.typename_to_string iname);
    match Sil.tenv_lookup (CContext.get_tenv context) iname with
    | Some Sil.Tstruct(_, _, _, _, (_, super_name):: _, _, _) ->
        Mangled.to_string super_name
    | _ ->
        Printing.log_err "NOT FOUND superclass = '%s'\n\n%!" (Sil.typename_to_string iname);
        (match super_opt with
          | Some super -> super
          | _ -> assert false) in
  match CContext.get_curr_class context with
  | CContext.ContextCls (cname, super_opt, _) ->
      retrive_super cname super_opt
  | CContext.ContextCategory (_, cls) ->
      retrive_super cls None
  | CContext.ContextNoCls
  | CContext.ContextProtocol _ -> assert false

let get_class_selector_instance context obj_c_message_expr_info act_params =
  let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
    | `Class qt -> (CTypes.get_type qt, selector, MCStatic)
    | `Instance ->
        (match act_params with
          | (instance_obj, Sil.Tptr(t, _)):: _
          | (instance_obj, t):: _ ->
              (CTypes.classname_of_type t, selector, MCVirtual)
          | _ -> assert false)
    | `SuperInstance ->
        let superclass = get_superclass_curr_class context in
        (superclass, selector, MCNoVirtual)
    | `SuperClass ->
        let superclass = get_superclass_curr_class context in
        (superclass, selector, MCStatic)

let get_formal_parameters tenv ms =
  let rec defined_parameters pl =
    match pl with
    | [] -> []
    | (name, raw_type, _):: pl' ->
        let qt =
          if (name = CFrontend_config.self && CMethod_signature.ms_is_instance ms) then
            (Ast_expressions.create_pointer_type raw_type)
          else Ast_expressions.create_qual_type raw_type in
        let typ = CTypes_decl.qual_type_to_sil_type tenv qt in
        (name, typ):: defined_parameters pl' in
  defined_parameters (CMethod_signature.ms_get_args ms)

(* Returns a list of captured variables. *)
(* In order to get the right mangled name we search among *)
(* the local variables of the defining function and it's formals.*)
let captured_vars_from_block_info context cvl =
  let formal2captured (s, t) =
    (Mangled.from_string s, t, false) in
  let find lv n =
    try
      list_find (fun (n', _, _) -> Mangled.to_string n' = n) lv
    with Not_found -> Printing.log_err "Trying to find variable %s@." n; assert false in
  let rec f cvl' =
    match cvl' with
    | [] -> []
    | cv:: cvl'' ->
        (match cv.Clang_ast_t.bcv_variable with
          | Some dr ->
              (match dr.Clang_ast_t.dr_name, dr.Clang_ast_t.dr_qual_type with
                | Some name_info, _ ->
                    let n = name_info.Clang_ast_t.ni_name in
                    if n = CFrontend_config.self && not context.is_instance then []
                    else
                      (let procdesc_formals = Cfg.Procdesc.get_formals context.procdesc in
                        (Printing.log_err "formals are %s@." (Utils.list_to_string (fun (x, _) -> x) procdesc_formals));
                        let formals = list_map formal2captured procdesc_formals in
                        [find (context.local_vars @ formals) n])
                | _ -> assert false)
          | None -> []) :: f cvl'' in
  list_flatten (f cvl)

let get_return_type tenv ms =
  let qt = CMethod_signature.ms_get_ret_type ms in
  CTypes_decl.qual_type_to_sil_type tenv
    (Ast_expressions.create_qual_type (CTypes.get_function_return_type qt))

let sil_func_attributes_of_attributes attrs =
  let rec do_translation acc al = match al with
    | [] -> list_rev acc
    | Clang_ast_t.SentinelAttr attribute_info::tl ->
      let (sentinel, null_pos) = match attribute_info.Clang_ast_t.ai_parameters with
        | a::b::[] -> (int_of_string a, int_of_string b)
        | _ -> assert false
      in
      do_translation (Sil.FA_sentinel(sentinel, null_pos)::acc) tl
    | _::tl -> do_translation acc tl in
  do_translation [] attrs

(** Creates a procedure description. *)
let create_local_procdesc cfg tenv ms fbody captured is_objc_inst_method =
  let defined = not ((list_length fbody) == 0) in
  let procname = CMethod_signature.ms_get_name ms in
  let pname = Procname.to_string procname in
  let attributes = sil_func_attributes_of_attributes (CMethod_signature.ms_get_attributes ms) in
  let create_new_procdesc () =
    let formals = get_formal_parameters tenv ms in
    let captured_str = list_map (fun (s, t, _) -> (Mangled.to_string s, t)) captured in
    (* Captured variables for blocks are treated as parameters *)
    let formals = captured_str @formals in
    let source_range = CMethod_signature.ms_get_loc ms in
    Printing.log_out
      "\n\n>>------------------------- Start creating a new procdesc for function: '%s' ---------<<\n" pname;
    let loc_start = CLocation.get_sil_location_from_range source_range true in
    let loc_exit = CLocation.get_sil_location_from_range source_range false in
    let ret_type = get_return_type tenv ms in
    let captured' = list_map (fun (s, t, _) -> (s, t)) captured in
    let procdesc =
      (* This part below is a boilerplate and the following list of        *)
      (* instructions should be moved in the Cfg.Procdesc module     *)
      let open Cfg.Procdesc in
      let proc_attributes =
        {
          Sil.access = Sil.Default;
          Sil.exceptions = [];
          Sil.is_abstract = false;
          Sil.is_bridge_method = false;
          Sil.is_objc_instance_method = is_objc_inst_method;
          Sil.is_synthetic_method = false;
          Sil.language = Sil.C_CPP;
          Sil.func_attributes = attributes;
          Sil.method_annotation = Sil.method_annotation_empty;
        } in
      create {
          cfg = cfg;
          name = procname;
          is_defined = defined;
          ret_type = ret_type;
          formals = formals;
          locals = [];
          captured = captured';
          loc = loc_start;
          proc_attributes = proc_attributes;
        } in
    if defined then
      (if !Config.arc_mode then
          Cfg.Procdesc.set_flag procdesc Mleak_buckets.objc_arc_flag "true";
        let start_kind = Cfg.Node.Start_node procdesc in
        let start_node = Cfg.Node.create cfg loc_start start_kind [] procdesc [] in
        let exit_kind = Cfg.Node.Exit_node procdesc in
        let exit_node = Cfg.Node.create cfg loc_exit exit_kind [] procdesc [] in
        Cfg.Procdesc.set_start_node procdesc start_node;
        Cfg.Procdesc.set_exit_node procdesc exit_node) in
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some prevoius_procdesc ->
      Printing.log_err "\n\n!!!WARNING: procdesc for %s already defined \n" pname;
      if defined && not (Cfg.Procdesc.is_defined prevoius_procdesc) then
        (Cfg.Procdesc.remove cfg (Cfg.Procdesc.get_proc_name prevoius_procdesc) true;
          create_new_procdesc ())
  | None -> create_new_procdesc ()

(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc cfg procname is_objc_inst_method type_opt =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some _ -> ()
  | None ->
      let ret_type, formals =
        (match type_opt with
          | Some (ret_type, arg_types) ->
              ret_type, list_map (fun typ -> ("x", typ)) arg_types
          | None -> Sil.Tvoid, []) in
      let loc = Sil.loc_none in
      let _ =
        let open Cfg.Procdesc in
        let proc_attributes =
          {
            Sil.access = Sil.Default;
            Sil.exceptions = [];
            Sil.is_abstract = false;
            Sil.is_bridge_method = false;
            Sil.is_objc_instance_method = is_objc_inst_method;
            Sil.is_synthetic_method = false;
            Sil.language = Sil.C_CPP;
            Sil.func_attributes = [];
            Sil.method_annotation = Sil.method_annotation_empty;
          } in
        create {
            cfg = cfg;
            name = procname;
            is_defined = false;
            ret_type = ret_type;
            formals = formals;
            locals = [];
            captured = [];
            loc = loc;
            proc_attributes = proc_attributes;
          } in
      ()

let instance_to_method_call_type instance =
  if instance then MCVirtual
  else MCStatic

(*Returns the procname and whether is instance, according to the selector *)
(* information and according to the method signature *)
let get_callee_objc_method context obj_c_message_expr_info act_params =
  let (class_name, method_name, mc_type) =
    get_class_selector_instance context obj_c_message_expr_info act_params in
  let is_instance = mc_type != MCStatic in
  match CTrans_models.get_predefined_model_method_signature class_name method_name
    mk_procname_from_method with
  | Some ms ->
      create_local_procdesc context.cfg context.tenv ms [] [] is_instance;
      CMethod_signature.ms_get_name ms, MCNoVirtual
  | None ->
      match resolve_method context.tenv class_name method_name with
      | Some callee_ms ->
          let is_instance = is_instance || (CMethod_signature.ms_is_instance callee_ms) in
          create_local_procdesc context.cfg context.tenv callee_ms [] [] is_instance;
          (CMethod_signature.ms_get_name callee_ms), mc_type
      | None ->
          let callee_pn = mk_procname_from_method class_name method_name in
          create_external_procdesc context.cfg callee_pn is_instance None;
          callee_pn, mc_type
