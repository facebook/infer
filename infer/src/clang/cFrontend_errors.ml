(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module MF = MarkupFormatter

type linter = {
  condition : CTL.t;
  issue_desc : CIssue.issue_desc;
  def_file : string option;
}

let single_to_multi checker =
  fun ctx an ->
    let condition, issue_desc_opt = checker ctx an in
    (condition, Option.to_list issue_desc_opt)

(* List of checkers on decls *that return 0 or 1 issue* *)
let decl_single_checkers_list =
  [ComponentKit.component_with_unconventional_superclass_advice;
   ComponentKit.mutable_local_vars_advice;
   ComponentKit.component_factory_function_advice;
   ComponentKit.component_file_cyclomatic_complexity_info;]

(* List of checkers on decls *)
let decl_checkers_list =
  ComponentKit.component_with_multiple_factory_methods_advice::
  (ComponentKit.component_file_line_count_info::
   (List.map ~f:single_to_multi decl_single_checkers_list))

(* List of checkers on stmts *that return 0 or 1 issue* *)
let stmt_single_checkers_list =
  [ComponentKit.component_file_cyclomatic_complexity_info;
   ComponentKit.component_initializer_with_side_effects_advice;
   GraphQL.DeprecatedAPIUsage.checker;]

let stmt_checkers_list = List.map ~f:single_to_multi stmt_single_checkers_list

(* List of checkers that will be filled after parsing them from
   input the linter def files *)
let parsed_linters = ref []

let evaluate_place_holder ph an =
  match ph with
  | "%ivar_name%" -> MF.monospaced_to_string (CFrontend_checkers.ivar_name an)
  | "%decl_name%" -> MF.monospaced_to_string (CFrontend_checkers.decl_name an)
  | "%cxx_ref_captured_in_block%" ->
      MF.monospaced_to_string (CFrontend_checkers.cxx_ref_captured_in_block an)
  | "%decl_ref_or_selector_name%" ->
      MF.monospaced_to_string (CFrontend_checkers.decl_ref_or_selector_name an)
  | "%iphoneos_target_sdk_version%" ->
      MF.monospaced_to_string (CFrontend_checkers.iphoneos_target_sdk_version an)
  | "%available_ios_sdk%" -> MF.monospaced_to_string (CFrontend_checkers.available_ios_sdk an)
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

let remove_new_lines message =
  String.substr_replace_all ~pattern:"\n" ~with_:" " message

let string_to_err_kind = function
  | "WARNING" -> Exceptions.Kwarning
  | "ERROR" -> Exceptions.Kerror
  | "INFO" -> Exceptions.Kinfo
  | "ADVICE" -> Exceptions.Kadvice
  | s -> (Logging.err "\n[ERROR] Severity %s does not exist. Stop.\n" s;
          assert false)

let string_to_issue_mode m =
  match m with
  | "ON" -> CIssue.On
  | "OFF" -> CIssue.Off
  | s ->
      (Logging.err "\n[ERROR] Mode %s does not exist. Please specify ON/OFF\n" s;
       assert false)

(** Convert a parsed checker in list of linters *)
let create_parsed_linters linters_def_file checkers : linter list =
  let open CIssue in
  let open CTL in
  let open Ctl_parser_types in
  Logging.out "\n Converting checkers in (condition, issue) pairs\n";
  let do_one_checker c =
    let dummy_issue = {
      name = c.name;
      description = "";
      suggestion = None;
      loc = Location.dummy;
      severity = Exceptions.Kwarning;
      mode = CIssue.On;
    } in
    let issue_desc, condition = List.fold ~f:(fun (issue', cond') d  ->
        match d with
        | CSet (s, phi) when String.equal s report_when_const ->
            issue', phi
        | CDesc (s, msg) when String.equal s message_const ->
            {issue' with description = msg}, cond'
        | CDesc (s, sugg) when String.equal s suggestion_const ->
            {issue' with suggestion = Some sugg}, cond'
        | CDesc (s, sev) when String.equal s severity_const ->
            {issue' with severity = string_to_err_kind sev}, cond'
        | CDesc (s, m) when String.equal s mode_const ->
            {issue' with mode = string_to_issue_mode m }, cond'
        | _ -> issue', cond') ~init:(dummy_issue, CTL.False) c.definitions in
    if Config.debug_mode then (
      Logging.out "\nMaking condition and issue desc for checker '%s'\n"
        c.name;
      Logging.out "\nCondition =\n     %a\n" CTL.Debug.pp_formula condition;
      Logging.out "\nIssue_desc = %a\n" CIssue.pp_issue issue_desc);
    {condition; issue_desc; def_file = Some linters_def_file} in
  List.map ~f:do_one_checker checkers


(* expands use of let defined formula id in checkers with their definition *)
let expand_checkers checkers =
  let open CTL in
  let open Ctl_parser_types in
  let rec expand acc map =
    match acc with
    | True
    | False -> acc
    | Atomic (name, [p]) when String.equal formula_id_const p ->
        Logging.out "  -Expanding formula identifier '%s'\n" name;
        (match Core.Std.String.Map.find map name with
         | Some f1 -> expand f1 map
         | None -> failwith
                     ("[ERROR]: Formula identifier '" ^ name ^ "' is undefined. Cannot expand."));
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
    let map = List.fold ~f:(fun map' d -> match d with
        | CLet (k,formula) -> Core.Std.Map.add map' ~key:k ~data:formula
        | _ -> map') ~init:map c.definitions in
    let exp_defs = List.fold ~f:(fun defs clause ->
        match clause with
        | CSet (report_when_const, phi) ->
            Logging.out "  -Expanding report_when\n";
            CSet (report_when_const, expand phi map) :: defs
        | cl -> cl :: defs) ~init:[] c.definitions in
    { c with definitions = exp_defs} in
  let expanded_checkers = List.map ~f:expand_one_checker checkers in
  expanded_checkers

let get_err_log translation_unit_context method_decl_opt =
  let procname = match method_decl_opt with
    | Some method_decl -> CProcname.from_decl translation_unit_context method_decl
    | None -> Typ.Procname.Linters_dummy_method in
  LintIssues.get_err_log procname

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue translation_unit_context method_decl_opt key issue_desc linters_def_file =
  let name = issue_desc.CIssue.name in
  let loc = issue_desc.CIssue.loc in
  let errlog = get_err_log translation_unit_context method_decl_opt in
  let err_desc = Errdesc.explain_frontend_warning issue_desc.CIssue.description
      issue_desc.CIssue.suggestion loc in
  let exn = Exceptions.Frontend_warning (name, err_desc, __POS__) in
  let trace = [ Errlog.make_trace_element 0 issue_desc.CIssue.loc "" [] ] in
  let err_kind = issue_desc.CIssue.severity in
  let method_name = CAst_utils.full_name_of_decl_opt method_decl_opt in
  let key = Hashtbl.hash (key ^ method_name) in
  Reporting.log_issue_from_errlog err_kind errlog exn ~loc ~ltr:trace
    ~node_id:(0, key) ?linters_def_file

let get_current_method context (an : Ctl_parser_types.ast_node) =
  match an with
  | Decl (FunctionDecl _ as d)
  | Decl (CXXMethodDecl _ as d)
  | Decl (CXXConstructorDecl _ as d)
  | Decl (CXXConversionDecl _ as d)
  | Decl (CXXDestructorDecl _ as d)
  | Decl (ObjCMethodDecl _ as d)
  | Decl (BlockDecl _ as d) -> Some d
  | _ -> context.CLintersContext.current_method

let fill_issue_desc_info_and_log context an key issue_desc linters_def_file loc =
  let desc = remove_new_lines
      (expand_message_string issue_desc.CIssue.description an) in
  let issue_desc' =
    {issue_desc with CIssue.description = desc; CIssue.loc = loc } in
  log_frontend_issue context.CLintersContext.translation_unit_context
    (get_current_method context an) key issue_desc' linters_def_file

(* Calls the set of hard coded checkers (if any) *)
let invoke_set_of_hard_coded_checkers_an context (an : Ctl_parser_types.ast_node) =
  let checkers, key  = match an with
    | Decl dec -> decl_checkers_list, CAst_utils.generate_key_decl dec
    | Stmt st -> stmt_checkers_list, CAst_utils.generate_key_stmt st in
  List.iter ~f:(fun checker ->
      let condition, issue_desc_list = checker context an in
      if CTL.eval_formula condition an context then
        List.iter ~f:(fun issue_desc ->
            if CIssue.should_run_check issue_desc.CIssue.mode then
              let loc = issue_desc.CIssue.loc in
              fill_issue_desc_info_and_log context an key issue_desc None loc
          ) issue_desc_list
    ) checkers

(* Calls the set of checkers parsed from files (if any) *)
let invoke_set_of_parsed_checkers_an parsed_linters context (an : Ctl_parser_types.ast_node) =
  let key = match an with
    | Decl dec -> CAst_utils.generate_key_decl dec
    | Stmt st -> CAst_utils.generate_key_stmt st in
  List.iter ~f:(fun (linter : linter) ->
      if CIssue.should_run_check linter.issue_desc.CIssue.mode &&
         CTL.eval_formula linter.condition an context then
        let loc = CFrontend_checkers.location_from_an context an in
        fill_issue_desc_info_and_log context an key linter.issue_desc linter.def_file loc
    ) parsed_linters

(* We decouple the hardcoded checkers from the parsed ones *)
let invoke_set_of_checkers_on_node context an =
  (match an with
   | Ctl_parser_types.Decl (Clang_ast_t.TranslationUnitDecl _) ->
       (* Don't run parsed linters on TranslationUnitDecl node.
          Because depending on the formula it may give an error at line -1 *)
       ()
   | _ -> invoke_set_of_parsed_checkers_an !parsed_linters context an);
  invoke_set_of_hard_coded_checkers_an context an
