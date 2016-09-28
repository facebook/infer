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
(*   -b) a issue_desc that describes the warning (see warning_desc definition) *)
(* 2. Add your checker to the CFrontend_checkers interface *)
(* 3. Decide in which element of the AST my_checker should be evaluated. *)
(*    - If it is a statement then you need to invoke my_checker from *)
(*    run_frontend_checkers_on_stmt in CFrontend_error module.*)
(*    - If it is a declaration invoke it from run_frontend_checkers_on_decl *)

(* Helper functions *)
let location_from_sinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.si_source_range true

let location_from_stmt stmt =
  let info, _ = Clang_ast_proj.get_stmt_tuple stmt in
  CLocation.get_sil_location_from_range info.Clang_ast_t.si_source_range true

let location_from_dinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.di_source_range true

let location_from_decl dec =
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.get_sil_location_from_range info.Clang_ast_t.di_source_range true

let decl_name dec =
  match Clang_ast_proj.get_named_decl_tuple dec with
  | Some (_, n) -> n.Clang_ast_t.ni_name
  | None -> ""

let ivar_name stmt =
  let open Clang_ast_t in
  match stmt with
  | ObjCIvarRefExpr (_, _, _, rei) ->
      let dr_ref = rei.ovrei_decl_ref in
      let ivar_pointer = dr_ref.dr_decl_pointer in
      (match Ast_utils.get_decl ivar_pointer with
       | Some (ObjCIvarDecl (_, named_decl_info, _, _, _)) ->
           named_decl_info.Clang_ast_t.ni_name
       | _ -> "")
  | _ -> ""


(* (is_CallExpr /\ not call_function_named) ||
    is_CXXTemporaryObjectExpr || is_CXXMemberCallExpr
   || is_CXXOperatorCallExpr || is_ObjCMessageExpr *)
let ctl_makes_an_expensive_call () =
  let open CTL in
  let white_list_functions = ["CGPointMake"] in
  Or (Or (Or (Or (And (Atomic ("is_statement_kind", ["CallExpr"]),
                       Not(Atomic("call_function_named", white_list_functions))),
                  Atomic ("is_statement_kind", ["CXXTemporaryObjectExpr"])),
              Atomic ("is_statement_kind", ["CXXMemberCallExpr"])),
          Atomic ("is_statement_kind", ["CXXOperatorCallExpr"])),
      Atomic ("is_statement_kind", ["ObjCMessageExpr"]))


(*
   ET([ObjCMethodDecl][->Body] (EF call_addObserver
                                  Or EF call_addObserverForName)
   =>
   ET([ObjCImplementationDecl,ObjCProtocolDecl][->]
         ET([ObjCMethodDecl][->Body] EF remove_observer) Or
         EH([ObjCImplementationDecl, ObjCProtocolDecl] EF remove_observer)
*)
let ctl_ns_notification decl =
  let open CTL in
  let exists_method_calling_addObserver =
    EF (Atomic ("call_method", ["addObserver:selector:name:object:"])) in
  let exists_method_calling_addObserverForName =
    EF (Atomic ("call_method", ["addObserverForName:object:queue:usingBlock:"])) in
  let add_observer = Or (exists_method_calling_addObserver,
                         exists_method_calling_addObserverForName) in
  let eventually_addObserver = ET(["ObjCMethodDecl"], Some Body, add_observer) in
  let exists_method_calling_removeObserver =
    EF(Atomic ("call_method",["removeObserver:"])) in
  let exists_method_calling_removeObserverName =
    EF(Atomic ("call_method",["removeObserver:name:object:"])) in
  let remove_observer = Or(exists_method_calling_removeObserver,
                           exists_method_calling_removeObserverName) in
  let remove_observer_in_block = ET(["BlockDecl"], Some Body, remove_observer) in
  let remove_observer' = Or(remove_observer, remove_observer_in_block) in
  let remove_observer_in_method = ET(["ObjCMethodDecl"], Some Body, remove_observer') in
  let eventually_removeObserver =
    ET(["ObjCImplementationDecl"; "ObjCProtocolDecl"], None,
       Or(remove_observer_in_method ,
          EH(["ObjCImplementationDecl"; "ObjCProtocolDecl"], remove_observer_in_method))) in
  let condition = Not (Implies (eventually_addObserver, eventually_removeObserver)) in
  let issue_desc = {
    CIssue.issue = CIssue.Registered_observer_being_deallocated;
    CIssue.description =
      Localise.registered_observer_being_deallocated_str CFrontend_config.self;
    CIssue.suggestion =
      Some "Consider removing the object from the notification center before its deallocation.";
    CIssue.loc = location_from_decl decl;
  } in
  condition, issue_desc

(* name_contains_delegate AND not name_contains_queue AND is_strong_property *)
let ctl_strong_delegate dec =
  let open CTL in
  let name_contains_delegate =
    Atomic ("property_name_contains_word", ["delegate"]) in
  let name_does_not_contains_queue =
    Not(Atomic ("property_name_contains_word", ["queue"])) in
  let is_strong_property =
    Atomic("is_strong_property", []) in
  let condition = And (name_contains_delegate,
                       And (name_does_not_contains_queue,
                            is_strong_property)) in
  let issue_desc = {
    CIssue.issue = CIssue.Strong_delegate_warning;
    CIssue.description = Printf.sprintf
        "Property or ivar %s declared strong" (decl_name dec);
    CIssue.suggestion = Some "In general delegates should be declared weak or assign";
    CIssue.loc = location_from_decl dec
  } in
  condition, issue_desc

(* (is_ObjC || is_Objc++) /\ is_global_var /\ not is_const_var  /\
   ET([VarDecl][->InitExpr] EF ctl_makes_an_expensive_call)
*)
let ctl_global_var_init_with_calls_warning decl =
  let open CTL in
  let ctl_is_global_var =
    And (And (Atomic ("is_objc_extension", []), Atomic ("is_global_var", [])),
         Not (Atomic ("is_const_var", []))) in
  let ctl_is_initialized_with_expensive_call  =
    ET(["VarDecl"], Some InitExpr, EF (ctl_makes_an_expensive_call ())) in
  let condition = And (ctl_is_global_var, ctl_is_initialized_with_expensive_call) in
  let issue_desc = {
    CIssue.issue = CIssue.Global_variable_initialized_with_function_or_method_call;
    CIssue.description = Printf.sprintf
        "Global variable %s is initialized using a function or method call"
        (decl_name decl);
    CIssue.suggestion = Some
        "If the function/method call is expensive, it can affect the starting time of the app.";
    CIssue.loc = location_from_decl decl
  } in
  condition, issue_desc

(* is_assign_property AND is_property_pointer_type *)
let ctl_assign_pointer_warning decl =
  let open CTL in
  let condition =
    And (Atomic("is_assign_property", []), Atomic("is_property_pointer_type", [])) in
  let issue_desc =
    { CIssue.issue = CIssue.Assign_pointer_warning;
      CIssue.description =
        Printf.sprintf
          "Property `%s` is a pointer type marked with the `assign` attribute"
          (decl_name decl);
      CIssue.suggestion = Some "Use a different attribute like `strong` or `weak`.";
      CIssue.loc = location_from_decl decl
    } in
  condition, issue_desc

(*
  not context_in_synchronized_block /\ not is_method_property_accessor_of_ivar
  /\ not is_objc_constructor /\ not is_objc_dealloc
*)
let ctl_direct_atomic_property_access_warning stmt =
  let open CTL in
  let condition =
    And (And (And (And (Not (Atomic ("context_in_synchronized_block", [])),
                        Atomic("is_ivar_atomic", [])),
                   Not (Atomic ("is_method_property_accessor_of_ivar", []))),
              Not (Atomic ("is_objc_constructor", []))),
         Not (Atomic ("is_objc_dealloc", []))) in
  let issue_desc = {
    CIssue.issue = CIssue.Direct_atomic_property_access;
    CIssue.description = Printf.sprintf
        "Direct access to ivar %s of an atomic property" (ivar_name stmt);
    CIssue.suggestion =
      Some "Accessing an ivar of an atomic property makes the property nonatomic";
    CIssue.loc = location_from_stmt stmt
  } in
  condition, issue_desc

let ctl_captured_cxx_ref_in_objc_block_warning stmt  =
  (* Fire if the list of captured references is not empty *)
  let condition = CTL.Atomic ("captures_cxx_references", []) in
  let issue_desc = {
    CIssue.issue = CIssue.Cxx_reference_captured_in_objc_block;
    CIssue.description = Printf.sprintf
        "C++ Reference variable(s) %s captured by Objective-C block"
        (Predicates.var_descs_name stmt);
    CIssue.suggestion = Some ("C++ References are unmanaged and may be invalid " ^
                              "by the time the block executes.");
    CIssue.loc = location_from_stmt stmt
  } in
  condition, issue_desc

(* === Warnings on properties === *)

(* Assing Pointer Warning: a property with a pointer type should not be declared `assign` *)
let assign_pointer_warning lcxt decl =
  let open CTL in
  let condition, issue_desc = ctl_assign_pointer_warning decl in
  if CTL.eval_formula condition (Decl decl) lcxt then
    Some issue_desc
  else None

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
let strong_delegate_warning lcxt decl =
  let condition, issue_desc = ctl_strong_delegate decl in
  if CTL.eval_formula condition (Decl decl) lcxt then
    Some issue_desc
  else None

(* GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL warning: *)
(* a global variable initialization should not *)
(* contain calls to functions or methods as these can be expensive an delay the starting time *)
(* of an app *)
let global_var_init_with_calls_warning lcxt decl =
  let condition, issue_desc = ctl_global_var_init_with_calls_warning decl in
  if  CTL.eval_formula condition (CTL.Decl decl) lcxt then
    Some issue_desc
  else None

(* Direct Atomic Property access:
   a property declared atomic should not be accessed directly via its ivar *)
let direct_atomic_property_access_warning context stmt =
  let condition, issue_desc = ctl_direct_atomic_property_access_warning stmt in
  if  CTL.eval_formula condition (CTL.Stmt stmt) context then
    Some issue_desc
  else None

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
let captured_cxx_ref_in_objc_block_warning lcxt stmt  =
  let condition, issue_desc = ctl_captured_cxx_ref_in_objc_block_warning stmt  in
  if CTL.eval_formula condition (CTL.Stmt stmt) lcxt  then
    Some issue_desc
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


let checker_NSNotificationCenter lcxt dec =
  let condition, issue_desc = ctl_ns_notification dec in
  if CTL.eval_formula  condition (CTL.Decl dec) lcxt then
    Some issue_desc
  else None
