(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

open CFrontend_utils
(* To create a new checker you should: *)
(* 1. Define a checker function, say my_checker, in this module. *)
(* my_checker should define: *)
(*   -a) a condition that determine if the checker fires *)
(*   -b) a issue_desc that describes the warning (see warning_desc definition) *)
(* 2. Add your checker to the CFrontend_checkers interface *)
(* 3. Decide in which element of the AST my_checker should be evaluated. *)
(*    - If it is a statement then you need to invoke my_checker from *)
(*    run_frontend_checkers_on_stmt in CFrontend_error module.*)
(*    - If it is a declaration invoke it from run_frontend_checkers_on_decl *)

(* Helper functions *)
let location_from_stmt lctx stmt =
  let info, _ = Clang_ast_proj.get_stmt_tuple stmt in
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.si_source_range true

let location_from_dinfo lctx info =
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.di_source_range true

let location_from_decl lctx dec =
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.di_source_range true

let location_from_an lcxt an =
  match an with
  | CTL.Stmt st -> location_from_stmt lcxt st
  | CTL.Decl d -> location_from_decl lcxt d

let decl_name an =
  match an with
  | CTL.Decl dec ->
      (match Clang_ast_proj.get_named_decl_tuple dec with
       | Some (_, n) -> n.Clang_ast_t.ni_name
       | None -> "")
  | _ -> ""

let tag_name_of_node an =
  match an with
  | CTL.Stmt stmt -> Clang_ast_proj.get_stmt_kind_string stmt
  | CTL.Decl decl -> Clang_ast_proj.get_decl_kind_string decl

let decl_ref_or_selector_name an =
  match CTL.next_state_via_transition an (Some CTL.PointerToDecl) with
  | Some (CTL.Decl ObjCMethodDecl _ as decl_an) ->
      "The selector " ^ (decl_name decl_an)
  | Some (CTL.Decl _ as decl_an) ->
      "The reference " ^ (decl_name decl_an)
  | _ -> failwith("decl_ref_or_selector_name must be called with a DeclRefExpr \
                   or an ObjCMessageExpr, but got " ^ (tag_name_of_node an))

let iphoneos_target_sdk_version _ =
  match Config.iphoneos_target_sdk_version with
  | Some f -> f
  | None -> "0"

let available_ios_sdk an =
  match CTL.next_state_via_transition an (Some CTL.PointerToDecl) with
  | Some CTL.Decl decl ->
      (match Predicates.get_available_attr_ios_sdk decl with
       | Some version -> version
       | None -> "")
  | _ -> failwith("available_ios_sdk must be called with a DeclRefExpr \
                   or an ObjCMessageExpr, but got " ^ (tag_name_of_node an))

let ivar_name an =
  let open Clang_ast_t in
  match an with
  | CTL.Stmt (ObjCIvarRefExpr (_, _, _, rei)) ->
      let dr_ref = rei.ovrei_decl_ref in
      let ivar_pointer = dr_ref.dr_decl_pointer in
      (match Ast_utils.get_decl ivar_pointer with
       | Some (ObjCIvarDecl (_, named_decl_info, _, _, _)) ->
           named_decl_info.Clang_ast_t.ni_name
       | _ -> "")
  | _ -> ""

let var_name an =
  let capt_refs = match an with
    | CTL.Stmt (Clang_ast_t.BlockExpr (_, _ , _, decl)) ->
        Predicates.captured_variables_cxx_ref decl
    | _ -> [] in
  let var_desc vars var_named_decl_info =
    vars ^ "'" ^ var_named_decl_info.Clang_ast_t.ni_name ^ "'" in
  IList.fold_left var_desc "" capt_refs


(* (is_CallExpr /\ not call_function_named) ||
    is_CXXTemporaryObjectExpr || is_CXXMemberCallExpr
   || is_CXXOperatorCallExpr || is_ObjCMessageExpr *)
let ctl_makes_an_expensive_call () =
  let open CTL in
  let white_list_functions = ["CGPointMake"] in
  Or (Or (Or (Or (And (Atomic ("in_node", ["CallExpr"]),
                       Not(Atomic("call_function_named", white_list_functions))),
                  Atomic ("in_node", ["CXXTemporaryObjectExpr"])),
              Atomic ("in_node", ["CXXMemberCallExpr"])),
          Atomic ("in_node", ["CXXOperatorCallExpr"])),
      Atomic ("in_node", ["ObjCMessageExpr"]))


(*
   ET([ObjCMethodDecl][->Body] (EF call_addObserver
                                  Or EF call_addObserverForName)
   =>
   ET([ObjCImplementationDecl,ObjCProtocolDecl][->]
         ET([ObjCMethodDecl][->Body] EF remove_observer) Or
         EH([ObjCImplementationDecl, ObjCProtocolDecl] EF remove_observer)
*)
let ctl_ns_notification_warning lctx an =
  let open CTL in
  let exists_method_calling_addObserver =
    EF (None, (Atomic ("call_method", ["addObserver:selector:name:object:"]))) in
  let exists_method_calling_addObserverForName =
    EF (None, (Atomic ("call_method", ["addObserverForName:object:queue:usingBlock:"]))) in
  let add_observer = Or (exists_method_calling_addObserver,
                         exists_method_calling_addObserverForName) in
  let eventually_addObserver = ET(["ObjCMethodDecl"], Some Body, add_observer) in
  let exists_method_calling_removeObserver =
    EF (None, (Atomic ("call_method", ["removeObserver:"]))) in
  let exists_method_calling_removeObserverName =
    EF (None, (Atomic ("call_method", ["removeObserver:name:object:"]))) in
  let remove_observer = Or(exists_method_calling_removeObserver,
                           exists_method_calling_removeObserverName) in
  let remove_observer_in_block = ET(["BlockDecl"], Some Body, remove_observer) in
  let remove_observer' = Or(remove_observer, remove_observer_in_block) in
  let remove_observer_in_method = ET(["ObjCMethodDecl"], Some Body, remove_observer') in
  let eventually_removeObserver =
    ET(["ObjCImplementationDecl"; "ObjCProtocolDecl"], None,
       Or(remove_observer_in_method ,
          EH(["ObjCImplementationDecl"; "ObjCProtocolDecl"], remove_observer_in_method))) in
  let condition = InNode (["ObjCImplementationDecl"; "ObjCProtocolDecl"],
                          Not (Implies (eventually_addObserver, eventually_removeObserver))) in
  let issue_desc = {
    CIssue.name = "REGISTERED_OBSERVER_BEING_DEALLOCATED";
    severity = Exceptions.Kwarning;
    mode = CIssue.On;
    description =
      "Object self is registered in a notification center but not being removed before deallocation";
    suggestion =
      Some "Consider removing the object from the notification center before its deallocation.";
    loc = location_from_an lctx an;
  } in
  condition, Some issue_desc

(* BAD_POINTER_COMPARISON: Fires whenever a NSNumber is dangerously coerced to
    a boolean in a comparison *)
let ctl_bad_pointer_comparison_warning lctx an =
  let open CTL in
  let is_binop = Atomic ("in_node", ["BinaryOperator"]) in
  let is_binop_eq = Atomic ("is_binop_with_kind", ["EQ"]) in
  let is_binop_ne = Atomic ("is_binop_with_kind", ["NE"]) in
  let is_binop_neq = Or (is_binop_eq, is_binop_ne) in
  let is_unop_lnot = Atomic ("is_unop_with_kind", ["LNot"]) in
  let is_implicit_cast_expr = Atomic ("in_node", ["ImplicitCastExpr"]) in
  let is_expr_with_cleanups = Atomic ("in_node", ["ExprWithCleanups"]) in
  let is_nsnumber = Atomic ("isa", ["NSNumber"]) in
  (*
  NOT is_binop_neq AND
  (is_expr_with_cleanups OR is_implicit_cast_expr OR is_binop OR is_unop_lnot)
  UNTIL is_nsnumber
  *)
  let p = Or (is_expr_with_cleanups, Or (is_implicit_cast_expr, Or (is_binop, is_unop_lnot))) in
  let p' = And (Not is_binop_neq, p) in
  let etx = ETX (["IfStmt"; "ForStmt"; "WhileStmt"; "ConditionalOperator"], Some Cond,
                 EU (None, p', is_nsnumber)) in
  let condition = InNode (["IfStmt"; "ForStmt"; "WhileStmt"; "ConditionalOperator"], etx) in
  let issue_desc =
    { CIssue.
      name = "BAD_POINTER_COMPARISON";
      severity = Exceptions.Kwarning;
      mode = CIssue.On;
      description = "Implicitly checking whether NSNumber pointer is nil";
      suggestion =
        Some ("Did you mean to compare against the unboxed value instead? " ^
              "Please either explicitly compare the NSNumber instance to nil, " ^
              "or use one of the NSNumber accessors before the comparison.");
      loc = location_from_an lctx an
    } in
  condition, Some issue_desc

(* name_contains_delegate AND not name_contains_queue AND is_strong_property *)
let ctl_strong_delegate_warning lctx an =
  let open CTL in
  let name_contains_delegate =
    Atomic ("property_name_contains_word", ["delegate"]) in
  let name_does_not_contains_queue =
    Not(Atomic ("property_name_contains_word", ["queue"])) in
  let is_strong_property =
    Atomic("is_strong_property", []) in
  let condition = InNode (["ObjCPropertyDecl"], And (name_contains_delegate,
                                                     And (name_does_not_contains_queue,
                                                          is_strong_property))) in
  let issue_desc = {
    CIssue.name = "STRONG_DELEGATE_WARNING";
    severity = Exceptions.Kwarning;
    mode = CIssue.On;
    description =
      "Property or ivar %decl_name% declared strong";
    suggestion = Some "In general delegates should be declared weak or assign";
    loc = location_from_an lctx an
  } in
  condition, Some issue_desc

(* (is_ObjC || is_Objc++) /\ is_global_var /\ not is_const_var  /\
   ET([VarDecl][->InitExpr] EF ctl_makes_an_expensive_call)
*)
let ctl_global_var_init_with_calls_warning lctx an =
  let open CTL in
  let ctl_is_global_var =
    And (And (Atomic ("is_objc_extension", []), Atomic ("is_global_var", [])),
         Not (Atomic ("is_const_var", []))) in
  let ctl_is_initialized_with_expensive_call  =
    ET(["VarDecl"], Some InitExpr, EF (None, (ctl_makes_an_expensive_call ()))) in
  let condition =
    InNode (["VarDecl"], And (ctl_is_global_var, ctl_is_initialized_with_expensive_call)) in
  let issue_desc = {
    CIssue.name = "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL";
    severity = Exceptions.Kwarning;
    mode = CIssue.On;
    description =
      "Global variable %decl_name% is initialized using a function or method call";
    suggestion = Some
        "If the function/method call is expensive, it can affect the starting time of the app.";
    loc = location_from_an lctx an
  } in
  condition, Some issue_desc

(* is_assign_property AND is_property_pointer_type *)
let ctl_assign_pointer_warning lctx an =
  let open CTL in
  let condition = InNode(["ObjCPropertyDecl"],
                         And (Atomic ("is_assign_property", []),
                              Atomic ("is_property_pointer_type", []))) in
  let issue_desc =
    { CIssue.name = "ASSIGN_POINTER_WARNING";
      severity = Exceptions.Kwarning;
      mode = CIssue.On;
      description =
        "Property `%decl_name%` is a pointer type marked with the `assign` attribute";
      suggestion = Some "Use a different attribute like `strong` or `weak`.";
      loc = location_from_an lctx an
    } in
  condition, Some issue_desc

(*
  not context_in_synchronized_block /\ not is_method_property_accessor_of_ivar
  /\ not is_objc_constructor /\ not is_objc_dealloc
*)
let ctl_direct_atomic_property_access_warning lctx an =
  let open CTL in
  let condition = InNode (["ObjCIvarRefExpr"],
                          And (And (And (And (Not (Atomic ("context_in_synchronized_block", [])),
                                              Atomic("is_ivar_atomic", [])),
                                         Not (Atomic ("is_method_property_accessor_of_ivar", []))),
                                    Not (Atomic ("is_objc_constructor", []))),
                               Not (Atomic ("is_objc_dealloc", [])))) in
  let issue_desc = {
    CIssue.name = "DIRECT_ATOMIC_PROPERTY_ACCESS";
    severity = Exceptions.Kwarning;
    mode = CIssue.On;
    description = "Direct access to ivar %ivar_name% of an atomic property";
    suggestion =
      Some "Accessing an ivar of an atomic property makes the property nonatomic";
    loc = location_from_an lctx an
  } in
  condition, Some issue_desc

let ctl_captured_cxx_ref_in_objc_block_warning lctx an  =
  (* Fire if the list of captured references is not empty *)
  let open CTL in
  let condition = InNode (["BlockDecl"], Atomic ("captures_cxx_references", [])) in
  let issue_desc = {
    CIssue.name = "CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK";
    severity = Exceptions.Kwarning;
    mode = CIssue.On;
    description =
      "C++ Reference variable(s) %var_name% captured by Objective-C block";
    suggestion = Some ("C++ References are unmanaged and may be invalid " ^
                       "by the time the block executes.");
    loc = match an with
      | Stmt (Clang_ast_t.BlockExpr (_, _ , _, decl)) -> location_from_an lctx (Decl decl)
      | _ -> location_from_an lctx an;
  } in
  condition, Some issue_desc

(** If the declaration has avilability attributes, check that it's compatible with
    the iphoneos_target_sdk_version *)
let ctl_unavailable_api_in_supported_ios_sdk_error lctx an =
  let open CTL in
  let condition =
    InNode(["DeclRefExpr"; "ObjCMessageExpr"],
           EX (Some PointerToDecl, (Atomic ("decl_unavailable_in_supported_ios_sdk", [])))) in
  let issue_desc =
    { CIssue.name = "UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK";
      severity = Exceptions.Kerror;
      mode = CIssue.On;
      description =
        "%decl_ref_or_selector_name% is not available in the required iOS SDK version \
         %iphoneos_target_sdk_version% (only available from version %available_ios_sdk%)";
      suggestion = Some "This could cause a crash.";
      loc = location_from_an lctx an
    } in
  condition, Some issue_desc
