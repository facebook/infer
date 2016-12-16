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

(* List of checkers on decls *)
let decl_checkers_list = [CFrontend_checkers.ctl_strong_delegate_warning;
                          CFrontend_checkers.ctl_assign_pointer_warning;
                          CFrontend_checkers.ctl_ns_notification_warning;
                          CFrontend_checkers.ctl_global_var_init_with_calls_warning;
                          ComponentKit.component_with_unconventional_superclass_advice;
                          ComponentKit.mutable_local_vars_advice;
                          ComponentKit.component_factory_function_advice;
                          ComponentKit.component_file_cyclomatic_complexity_info;
                          ComponentKit.component_with_multiple_factory_methods_advice;]

(* List of checkers on stmts *)
let stmt_checkers_list =  [CFrontend_checkers.ctl_direct_atomic_property_access_warning;
                           CFrontend_checkers.ctl_captured_cxx_ref_in_objc_block_warning;
                           CFrontend_checkers.ctl_bad_pointer_comparison_warning;
                           ComponentKit.component_file_cyclomatic_complexity_info;
                           ComponentKit.component_initializer_with_side_effects_advice;
                           CFrontend_checkers.ctl_unavailable_api_in_supported_ios_sdk_error;]

(* List of checkers on translation unit that potentially output multiple issues *)
let translation_unit_checkers_list = [ComponentKit.component_file_line_count_info;]

let evaluate_place_holder ph an =
  match ph with
  | "%ivar_name%" -> CFrontend_checkers.ivar_name an
  | "%decl_name%" -> CFrontend_checkers.decl_name an
  | "%var_name%" ->  CFrontend_checkers.var_name an
  | "%decl_ref_or_selector_name%" ->
      CFrontend_checkers.decl_ref_or_selector_name an
  | "%iphoneos_target_sdk_version%" ->
      CFrontend_checkers.iphoneos_target_sdk_version an
  | "%available_ios_sdk%" -> CFrontend_checkers.available_ios_sdk an
  | _ -> (Logging.err "ERROR: helper function %s is unknown. Stop.\n" ph;
          assert false)

(* given a message this function searches for a place-holder identifier,
   eg %id%. Then it evaluates id and replaces %id% in message
   with the result of its evaluation. The function keeps on checking if
   other place-holders exist and repeats the process until there are
    no place-holder left.
*)
let rec expand_message_string message an =
  (* reg exp should match alphanumeric id with possibly somee _ *)
  let re = Str.regexp "%[a-zA-Z0-9_]+%" in
  try
    let _ = Str.search_forward re message 0 in
    let ms = Str.matched_string message in
    let res = evaluate_place_holder ms an in
    Logging.out "\nMatched string '%s'\n" ms;
    let re_ms = Str.regexp_string ms in
    let message' = Str.replace_first re_ms res message in
    Logging.out "Replacing %s in message: \n %s \n" ms message;
    Logging.out "Resulting message: \n %s \n" message';
    expand_message_string message' an
  with Not_found -> message


(* expands use of let defined formula id in checkers with their definition *)
let expand_checkers checkers =
  let open CTL in
  let open Ctl_parser_types in
  let rec expand acc map =
    match acc with
    | True
    | False -> acc
    | Atomic (name, [p]) when formula_id_const = p ->
        Logging.out "  -Expanding formula identifier '%s'\n" name;
        (match Core.Std.String.Map.find map name with
         | Some f1 -> expand f1 map
         | None ->
             Logging.out "[ERROR]: Formula identifier '%s' is undefined. Cannot expand." name;
             assert false);
    | Atomic _ -> acc
    | Not f1 -> Not (expand f1 map)
    | And (f1, f2) -> And (expand f1 map, expand f2 map)
    | Or (f1, f2) -> Or (expand f1 map, expand f2 map)
    | Implies (f1, f2) -> Implies (expand f1 map, expand f2 map)
    | InNode (node_type_list, f1) -> InNode (node_type_list, expand f1 map)
    | AU (f1, f2) -> AU (expand f1 map, expand f2 map)
    | EU (trans, f1, f2) -> EU (trans, expand f1 map, expand f2 map)
    | EF (trans, f1) -> EF (trans, expand f1 map)
    | AF f1 -> AF (expand f1 map)
    | AG f1 -> AG (expand f1 map)
    | EX (trans, f1) -> EX (trans, expand f1 map)
    | AX f1 -> AX (expand f1 map)
    | EH (cl, f1) -> EH (cl, expand f1 map)
    | EG (trans, f1) -> EG (trans, expand f1 map)
    | ET (tl, sw, f1) -> ET (tl, sw, expand f1 map)
    | ETX (tl, sw, f1) -> ETX (tl, sw, expand f1 map) in
  let expand_one_checker c =
    Logging.out " +Start expanding %s\n" c.name;
    let map : CTL.t Core.Std.String.Map.t = Core.Std.String.Map.empty in
    let map = IList.fold_left (fun map' d -> match d with
        | CLet (k,formula) -> Core.Std.Map.add map' ~key:k ~data:formula
        | _ -> map') map c.Ctl_parser_types.definitions in
    let exp_defs = IList.fold_left (fun defs clause ->
        match clause with
        | CSet (report_when_const, phi) ->
            Logging.out "  -Expanding report_when\n";
            CSet (report_when_const, expand phi map) :: defs
        | cl -> cl :: defs) [] c.definitions in
    { c with definitions = exp_defs} in
  let expanded_checkers = IList.map expand_one_checker checkers in
  expanded_checkers

let get_err_log translation_unit_context method_decl_opt =
  let procname = match method_decl_opt with
    | Some method_decl -> General_utils.procname_of_decl translation_unit_context method_decl
    | None -> Procname.Linters_dummy_method in
  LintIssues.get_err_log procname

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue translation_unit_context method_decl_opt key issue_desc =
  let name = issue_desc.CIssue.name in
  let loc = issue_desc.CIssue.loc in
  let errlog = get_err_log translation_unit_context method_decl_opt in
  let err_desc = Errdesc.explain_frontend_warning issue_desc.CIssue.description
      issue_desc.CIssue.suggestion loc in
  let exn = Exceptions.Frontend_warning (name, err_desc, __POS__) in
  let trace = [ Errlog.make_trace_element 0 issue_desc.CIssue.loc "" [] ] in
  let err_kind = issue_desc.CIssue.severity in
  let method_name = Ast_utils.full_name_of_decl_opt method_decl_opt in
  let key = Hashtbl.hash (key ^ method_name) in
  Reporting.log_issue_from_errlog err_kind errlog exn ~loc ~ltr:trace
    ~node_id:(0, key)

let invoke_set_of_checkers_an an context =
  let checkers, key  = match an with
    | CTL.Decl dec -> decl_checkers_list, Ast_utils.generate_key_decl dec
    | CTL.Stmt st -> stmt_checkers_list, Ast_utils.generate_key_stmt st in
  IList.iter (fun checker ->
      let condition, issue_desc_opt = checker context an in
      match issue_desc_opt with
      | Some issue_desc ->
          if CIssue.should_run_check issue_desc.CIssue.mode &&
             CTL.eval_formula condition an context then
            let desc' = expand_message_string issue_desc.CIssue.description an in
            let issue_desc' = {issue_desc with CIssue.description = desc'} in
            log_frontend_issue context.CLintersContext.translation_unit_context
              context.CLintersContext.current_method key issue_desc'
      | None -> ()) checkers


let run_frontend_checkers_on_an (context: CLintersContext.context) an =
  let open Clang_ast_t in
  let context' = match an with
    | CTL.Decl (ObjCImplementationDecl _ as dec) ->
        {context with current_objc_impl = Some dec}
    | CTL.Stmt (ObjCAtSynchronizedStmt _ )->
        { context with CLintersContext.in_synchronized_block = true }
    | _ -> context in
  invoke_set_of_checkers_an an context';
  context'

let run_translation_unit_checker (context: CLintersContext.context) dec =
  IList.iter (fun checker ->
      let issue_desc_list = checker context dec in
      IList.iter (fun issue_desc ->
          if (CIssue.should_run_check issue_desc.CIssue.mode) then
            let key = Ast_utils.generate_key_decl dec in
            log_frontend_issue context.CLintersContext.translation_unit_context
              context.CLintersContext.current_method key issue_desc
        ) issue_desc_list) translation_unit_checkers_list
