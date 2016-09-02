(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open CFrontend_utils

(* To create a new checker you should: *)
(* 1. Define a checker function, say my_checker, in this module. *)
(* my_checker should define: *)
(*   -a) a condition that determine if the checker fires *)
(*   -b) a warning_desc that describes the warning (see warning_desc definition) *)
(* 2. Add your checker to the CFrontend_checkers interface *)
(* 3. Decide in which element of the AST my_checker should be evaluated. *)
(*    - If it is a statement then you need to invoke my_checker from *)
(*    run_frontend_checkers_on_stmt in CFrontend_error module.*)
(*    - If it is a declaration invoke it from run_frontend_checkers_on_decl *)

(* Helper functions *)

let get_ivar_attributes ivar_decl =
  let open Clang_ast_t in
  match ivar_decl with
  | ObjCIvarDecl (ivar_decl_info, _, _, _, _) ->
      (match Ast_utils.get_property_of_ivar ivar_decl_info.Clang_ast_t.di_pointer with
       | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
           obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes
       | _ -> [])
  | _ -> []

let is_method_property_accessor_of_ivar method_decl ivar_pointer =
  let open Clang_ast_t in
  match method_decl with
  | ObjCMethodDecl (_, _, obj_c_method_decl_info) ->
      if obj_c_method_decl_info.Clang_ast_t.omdi_is_property_accessor then
        let prperty_opt = obj_c_method_decl_info.Clang_ast_t.omdi_property_decl in
        match Ast_utils.get_decl_opt_with_decl_ref prperty_opt with
        | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
            (match obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl with
             | Some decl_ref -> decl_ref.Clang_ast_t.dr_decl_pointer = ivar_pointer
             | None -> false)
        | _ -> false
      else false
  | _ -> false

(* checks if ivar is defined among a set of fields and if it is atomic *)
let is_ivar_atomic attributes =
  IList.exists (Ast_utils.property_attribute_eq `Atomic) attributes

let name_contains_word pname word =
  let rexp = Str.regexp_string_case_fold word in
  try
    Str.search_forward rexp pname.Clang_ast_t.ni_name 0 >= 0
  with Not_found -> false

let location_from_sinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.si_source_range true

let location_from_dinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.di_source_range true

let rec is_self s =
  let open Clang_ast_t in
  match s with
  | ImplicitCastExpr(_, [s], _, _) -> is_self s
  | DeclRefExpr(_, _, _, dr) ->
      (match dr.drti_decl_ref with
       | Some decl_ref ->
           (match decl_ref.dr_name with
            | Some n -> n.ni_name = CFrontend_config.self
            | _ -> false)
       | _ -> false)
  | _ -> false

let decl_ref_is_in names st =
  match st with
  | Clang_ast_t.DeclRefExpr (_, _, _, drti) ->
      (match drti.drti_decl_ref with
       | Some dr -> let ndi, _, _ = CFrontend_utils.Ast_utils.get_info_from_decl_ref dr in
           IList.exists (fun n -> n = ndi.ni_name) names
       | _ -> false)
  | _ -> false

let call_function_named st names =
  Ast_utils.exists_eventually_st decl_ref_is_in names st

let rec eventually_makes_a_call exp =
  CFrontend_utils.Ast_utils.exists_eventually_st is_expensive_function_or_method_call () exp

and is_expensive_function_or_method_call _ st =
  let white_list_functions = ["CGPointMake"] in
  let open Clang_ast_t in
  match st with
  | CallExpr (_, st :: _, _) -> (* for C *)
      not (call_function_named st white_list_functions)
  | CXXConstructExpr (_, stmt_list, _, _) ->
      (* Assumption: constructor is expensive iff it has a call inside *)
      (IList.exists eventually_makes_a_call) stmt_list
  | CXXTemporaryObjectExpr _ (* for C++ *)
  | CXXMemberCallExpr _ | CXXOperatorCallExpr _
  | ObjCMessageExpr _ -> true (* for ObjC *)
  | _ -> false

(* Call method m and on the pn parameter pred holds *)
(* st |= call_method(m(p1,...,pn,...pk)) /\ pred(pn) *)
let call_method_on_nth pred pn m st =
  match st with
  | Clang_ast_t.ObjCMessageExpr (_, params, _, omei) when omei.omei_selector = m ->
      (try
         let p = IList.nth params pn in
         pred p
       with _ -> false)
  | _ -> false

let dec_body_eventually atomic_pred param dec =
  match dec with
  | Clang_ast_t.ObjCMethodDecl (_, _, omdi) ->
      (match omdi.Clang_ast_t.omdi_body with
       | Some body -> Ast_utils.exists_eventually_st atomic_pred param body
       | _ -> false)
  | _ -> false

(* true if a variable is initialized with a method/function call.*)
(* implemented as decl |= EF is_function_or_method_call *)
let is_initialized_with_expensive_call decl =
  match decl with
  | Clang_ast_t.VarDecl (_, _ ,_, vdi) ->
      (match vdi.vdi_init_expr with
       | Some init_expr ->
           eventually_makes_a_call init_expr
       | _ -> false)
  | _ -> false

let captured_variables_cxx_ref captured_vars =
  let capture_var_is_cxx_ref reference_captured_vars captured_var =
    let decl_ref_opt = captured_var.Clang_ast_t.bcv_variable in
    match Ast_utils.get_decl_opt_with_decl_ref decl_ref_opt with
    | Some VarDecl (_, named_decl_info, qual_type, _)
    | Some ParmVarDecl (_, named_decl_info, qual_type, _)
    | Some ImplicitParamDecl (_, named_decl_info, qual_type, _) ->
        (match Ast_utils.get_desugared_type qual_type.Clang_ast_t.qt_type_ptr with
         | Some RValueReferenceType _ | Some LValueReferenceType _ ->
             named_decl_info::reference_captured_vars
         | _ -> reference_captured_vars)
    | _ -> reference_captured_vars in
  IList.fold_left capture_var_is_cxx_ref [] captured_vars

(* === Warnings on properties === *)

(* Assing Pointer Warning: a property with a pointer type should not be declared `assign` *)
let assign_pointer_warning _ decl_info pname obj_c_property_decl_info =
  let open Clang_ast_t in
  let condition =
    let has_assign_property () = ObjcProperty_decl.is_assign_property obj_c_property_decl_info in
    let is_pointer_type () =
      let type_ptr = obj_c_property_decl_info.opdi_type_ptr in
      let raw_ptr = Clang_ast_types.type_ptr_to_clang_pointer type_ptr in
      match Clang_ast_main.PointerMap.find raw_ptr !CFrontend_config.pointer_type_index with
      | MemberPointerType _ | ObjCObjectPointerType _ | BlockPointerType _ -> true
      | TypedefType (_, tti) -> (Ast_utils.name_of_typedef_type_info tti) = CFrontend_config.id_cl
      | exception Not_found -> false
      | _ -> false in
    has_assign_property() && is_pointer_type () in
  if condition then
    Some
      { CIssue.issue = CIssue.Assign_pointer_warning;
        CIssue.description =
          Printf.sprintf
            "Property `%s` is a pointer type marked with the `assign` attribute"
            pname.ni_name;
        CIssue.suggestion = Some "Use a different attribute like `strong` or `weak`.";
        CIssue.loc = location_from_dinfo decl_info
      }
  else None

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
let strong_delegate_warning _ decl_info pname obj_c_property_decl_info =
  let condition = (name_contains_word pname "delegate")
                  && not (name_contains_word pname "queue")
                  && ObjcProperty_decl.is_strong_property obj_c_property_decl_info in
  if condition then
    Some { CIssue.issue = CIssue.Strong_delegate_warning;
           CIssue.description = "Property or ivar "^pname.Clang_ast_t.ni_name^" declared strong";
           CIssue.suggestion = Some "In general delegates should be declared weak or assign";
           CIssue.loc = location_from_dinfo decl_info
         }
  else None

(* GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL warning: a global variable initialization should not *)
(* contain calls to functions or methods as these can be expensive an delay the starting time *)
(* of an app *)
let global_var_init_with_calls_warning _ decl =
  let decl_info, gvar =
    match Clang_ast_proj.get_named_decl_tuple decl with
    | Some (di, ndi) -> di, ndi.ni_name
    | None -> assert false (* we cannot be here *) in
  let condition = General_utils.is_objc_extension
                  && Ast_utils.is_syntactically_global_var decl
                  && (not (Ast_utils.is_const_expr_var decl))
                  && is_initialized_with_expensive_call decl in
  if condition then
    Some {
      CIssue.issue = CIssue.Global_variable_initialized_with_function_or_method_call;
      CIssue.description = "Global variable " ^ gvar ^
                           " is initialized using a function or method call";
      CIssue.suggestion = Some
          "If the function/method call is expensive, it can affect the starting time of the app.";
      CIssue.loc = location_from_dinfo decl_info
    }
  else None

(* Direct Atomic Property access:
   a property declared atomic should not be accessed directly via its ivar *)
let direct_atomic_property_access_warning context stmt_info ivar_decl_ref =
  let ivar_pointer = ivar_decl_ref.Clang_ast_t.dr_decl_pointer in
  match Ast_utils.get_decl ivar_pointer, context.CLintersContext.current_method with
  | Some (ObjCIvarDecl (_, named_decl_info, _, _, _) as d), Some method_decl ->
      let method_name = match Clang_ast_proj.get_named_decl_tuple method_decl with
        | Some (_, method_named_decl) -> method_named_decl.Clang_ast_t.ni_name
        | _ -> "" in
      let ivar_name = named_decl_info.Clang_ast_t.ni_name in
      let condition =
        (* We warn when:
           (1) we are not inside a @synchronized block
           (2) the property has the atomic attribute and
           (3) the access of the ivar is not in a getter or setter method
           (4) the access of the ivar is not in the init method
           Last two conditions avoids false positives *)
        not (context.CLintersContext.in_synchronized_block)
        && is_ivar_atomic (get_ivar_attributes d)
        && not (is_method_property_accessor_of_ivar method_decl ivar_pointer)
        && not (Procname.is_objc_constructor method_name)
        && not (Procname.is_objc_dealloc method_name) in
      if condition then
        Some {
          CIssue.issue = CIssue.Direct_atomic_property_access;
          CIssue.description = "Direct access to ivar " ^ ivar_name ^
                               " of an atomic property";
          CIssue.suggestion =
            Some "Accessing an ivar of an atomic property makes the property nonatomic";
          CIssue.loc = location_from_sinfo stmt_info
        }
      else None
  | _ -> None


(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
let captured_cxx_ref_in_objc_block_warning _ stmt_info captured_vars =
  let capt_refs = captured_variables_cxx_ref captured_vars in
  let var_descs =
    let var_desc vars var_named_decl_info =
      vars ^ "'" ^ var_named_decl_info.Clang_ast_t.ni_name ^ "'" in
    IList.fold_left var_desc "" capt_refs in
  (* Fire if the list of captured references is not empty *)
  let condition = IList.length capt_refs > 0 in
  if condition then
    Some {
      CIssue.issue = CIssue.Cxx_reference_captured_in_objc_block;
      CIssue.description = "C++ Reference variable(s) " ^ var_descs ^
                           " captured by Objective-C block";
      CIssue.suggestion = Some ("C++ References are unmanaged and may be invalid " ^
                                "by the time the block executes.");
      CIssue.loc = location_from_sinfo stmt_info
    }
  else None


(* BAD_POINTER_COMPARISON: Fires whenever a NSNumber is dangerously coerced to
    a boolean in a comparison *)
let bad_pointer_comparison_warning _ stmt_info stmts =
  let rec condition stmts =
    let condition_aux stmt =
      match (stmt: Clang_ast_t.stmt) with
      | BinaryOperator (_, _, _, boi) when
          (boi.boi_kind = `EQ) || (boi.boi_kind = `NE) -> false
      | BinaryOperator (_, stmts, _, _) -> condition stmts
      | UnaryOperator (_, stmts, _, uoi) when uoi.uoi_kind = `LNot ->
          condition stmts
      | ImplicitCastExpr (_, stmts, _, _)
      | ExprWithCleanups (_, stmts, _, _) ->
          condition stmts
      | stmt ->
          match Clang_ast_proj.get_expr_tuple stmt with
          | Some (_, _, expr_info) ->
              let typ = CFrontend_utils.Ast_utils.get_desugared_type expr_info.ei_type_ptr in
              CFrontend_utils.Ast_utils.is_ptr_to_objc_class typ "NSNumber"
          | _ -> false in
    IList.exists condition_aux stmts in
  if condition stmts then
    Some { CIssue.
           issue = CIssue.Bad_pointer_comparison;
           description = "Implicitly checking whether NSNumber pointer is nil";
           suggestion =
             Some ("Did you mean to compare against the unboxed value instead? " ^
                   "Please either explicitly compare the NSNumber instance to nil, " ^
                   "or use one of the NSNumber accessors before the comparison.");
           loc = location_from_sinfo stmt_info
         }
  else
    None


(* exist m1:  m1.body |- EF call_method(addObserver:) =>
   exists m2 : m2.body |- EF call_method(removeObserver:) *)
let checker_NSNotificationCenter _ decl_info impl_decl_info decls =
  let exists_method_calling_addObserver =
    IList.exists
      (dec_body_eventually (call_method_on_nth is_self 1)
         "addObserver:selector:name:object:") decls in
  let exists_method_calling_addObserverForName =
    IList.exists
      (dec_body_eventually (call_method_on_nth is_self 1)
         "addObserverForName:object:queue:usingBlock:") decls in
  let eventually_addObserver = exists_method_calling_addObserver
                               || exists_method_calling_addObserverForName in

  let eventually_removeObserver decls =
    let exists_method_calling_removeObserver =
      IList.exists (dec_body_eventually (call_method_on_nth is_self 1) "removeObserver:") decls in
    let exists_method_calling_removeObserverName =
      IList.exists
        (dec_body_eventually (call_method_on_nth is_self 1)
           "removeObserver:name:object:") decls in
    exists_method_calling_removeObserver || exists_method_calling_removeObserverName in

  let rec exists_on_hierarchy f super =
    match super with
    | Some (decl_list, impl_decl_info) ->
        (f decl_list
         || exists_on_hierarchy f (Ast_utils.get_super_impl impl_decl_info))
    | None -> false in

  let eventually_removeObserver_in_whole_hierarchy decls impl_decl_info =
    exists_on_hierarchy eventually_removeObserver (Some (decls, impl_decl_info)) in

  (* if registration happens among the given decls, then search for removeObserver across the *)
  (* whole hierarchy of classes *)
  let condition =
    eventually_addObserver &&
    match impl_decl_info with
    | Some idi ->
        not (eventually_removeObserver_in_whole_hierarchy decls idi)
    | None -> not (eventually_removeObserver decls) in

  if condition then
    Some {
      CIssue.issue = CIssue.Registered_observer_being_deallocated;
      CIssue.description =
        Localise.registered_observer_being_deallocated_str CFrontend_config.self;
      CIssue.suggestion =
        Some "Consider removing the object from the notification center before its deallocation.";
      CIssue.loc = location_from_dinfo decl_info
    }
  else None
