(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

type linter =
  { condition: CTLTypes.t
  ; issue_desc: CIssue.t
  ; whitelist_paths: ALVar.t list
  ; blacklist_paths: ALVar.t list }

(* If in linter developer mode and if current linter was passed, filter it out *)
let filter_parsed_linters_developer parsed_linters =
  if List.length parsed_linters > 1 && Config.linters_developer_mode then
    match Config.linter with
    | None ->
        L.(die UserError)
          "In linters developer mode you should debug only one linter at a time. This is important \
           for debugging the rule. Pass the flag --linter <name> to specify the linter you want to \
           debug."
    | Some lint ->
        List.filter
          ~f:(fun (rule : linter) ->
            String.equal rule.issue_desc.issue_type.IssueType.unique_id lint )
          parsed_linters
  else parsed_linters


let filter_parsed_linters_by_path parsed_linters source_file =
  let filter_parsed_linter_by_path linter =
    let should_lint paths =
      List.exists
        ~f:(fun path -> ALVar.compare_str_with_alexp (SourceFile.to_rel_path source_file) path)
        paths
    in
    let whitelist_ok = List.is_empty linter.whitelist_paths || should_lint linter.whitelist_paths in
    let blacklist_ok =
      List.is_empty linter.blacklist_paths || not (should_lint linter.blacklist_paths)
    in
    whitelist_ok && blacklist_ok
  in
  List.filter ~f:filter_parsed_linter_by_path parsed_linters


let filter_parsed_linters parsed_linters source_file =
  let linters = filter_parsed_linters_developer parsed_linters in
  if Config.debug_mode || not Config.filtering then linters
    (* do not filter by path if in debug or no filtering mode *)
  else filter_parsed_linters_by_path linters source_file


let pp_linters fmt linters =
  let pp_linter fmt {issue_desc= {issue_type= {IssueType.unique_id}}} =
    F.fprintf fmt "%s@\n" unique_id
  in
  List.iter ~f:(pp_linter fmt) linters


(* Map a formula id to a triple (visited, parameters, definition).
   Visited is used during the expansion phase to understand if the
   formula was already expanded and, if yes we have a cyclic definifion *)
type macros_map = (bool * ALVar.t list * CTLTypes.t) ALVar.FormulaIdMap.t

(* Map a path name to a list of paths.  *)
type paths_map = ALVar.t list ALVar.VarMap.t

let single_to_multi checker ctx an =
  let issue_desc_opt = checker ctx an in
  Option.to_list issue_desc_opt


(* List of checkers on decls *that return 0 or 1 issue* *)
let decl_single_checkers_list = [ComponentKit.mutable_local_vars_advice]

(* List of checkers on decls *)
let decl_checkers_list =
  ComponentKit.component_with_multiple_factory_methods_advice
  :: List.map ~f:single_to_multi decl_single_checkers_list


let evaluate_place_holder ph an =
  match ph with
  | "%ivar_name%" ->
      MF.monospaced_to_string (ALUtils.ivar_name an)
  | "%decl_name%" ->
      MF.monospaced_to_string (Ctl_parser_types.ast_node_name an)
  | "%cxx_ref_captured_in_block%" ->
      MF.monospaced_to_string (ALUtils.cxx_ref_captured_in_block an)
  | "%decl_ref_or_selector_name%" ->
      MF.monospaced_to_string (ALUtils.decl_ref_or_selector_name an)
  | "%receiver_method_call%" ->
      MF.monospaced_to_string (ALUtils.receiver_method_call an)
  | "%type%" ->
      MF.monospaced_to_string (Ctl_parser_types.ast_node_type an)
  | "%class_name%" ->
      ALUtils.class_name an
  | "%child_type%" ->
      MF.monospaced_to_string (Ctl_parser_types.stmt_node_child_type an)
  | "%name%" ->
      MF.monospaced_to_string (Ctl_parser_types.ast_node_name an)
  | "%cxx_fully_qualified_name%" ->
      MF.monospaced_to_string (Ctl_parser_types.ast_node_cxx_fully_qualified_name an)
  | "%kind%" ->
      MF.monospaced_to_string (Ctl_parser_types.ast_node_kind an)
  | "%source_file%" ->
      (* N.B.: This is the source_file in which the AST node occurs, which is not
       *       necessarily the same as the translation unit's source file.
       *)
      MF.monospaced_to_string
        (Option.value_map
           ~f:(fun sf -> SourceFile.to_rel_path (SourceFile.create sf))
           ~default:""
           (Ctl_parser_types.get_source_file an))
  | "%ref_source_file%" ->
      MF.monospaced_to_string
        (Option.value_map
           ~f:(fun sf -> SourceFile.to_rel_path (SourceFile.create sf))
           ~default:""
           (Ctl_parser_types.get_referenced_decl_source_file an))
  | _ ->
      L.die InternalError "helper function %s is unknown" ph


(* given a message this function searches for a place-holder identifier,
   eg %id%. Then it evaluates id and replaces %id% in message
   with the result of its evaluation. The function keeps on checking if
   other place-holders exist and repeats the process until there are
    no place-holder left.
*)
let rec expand_message_string context message an =
  (* reg exp should match alphanumeric id with possibly somee _ *)
  let re = Str.regexp "%[a-zA-Z0-9_]+%" in
  try
    ignore (Str.search_forward re message 0) ;
    let ms = Str.matched_string message in
    let res = evaluate_place_holder ms an in
    L.(debug Linters Medium) "@\nMatched string '%s'@\n" ms ;
    let re_ms = Str.regexp_string ms in
    let message' = Str.replace_first re_ms res message in
    L.(debug Linters Medium) "Replacing %s in message: @\n %s @\n" ms message ;
    L.(debug Linters Medium) "Resulting message: @\n %s @\n" message' ;
    expand_message_string context message' an
  with Caml.Not_found -> message


let remove_new_lines_and_whitespace message =
  let words = List.map ~f:String.strip (String.split ~on:'\n' message) in
  String.concat words ~sep:" "


let string_to_severity = function
  | "WARNING" ->
      IssueType.Warning
  | "ERROR" ->
      IssueType.Error
  | "INFO" ->
      IssueType.Info
  | "ADVICE" ->
      IssueType.Advice
  | "LIKE" ->
      IssueType.Like
  | s ->
      L.die InternalError "Severity %s does not exist" s


let string_to_issue_mode m =
  match m with
  | "ON" ->
      CIssue.On
  | "OFF" ->
      CIssue.Off
  | s ->
      L.die InternalError "Mode %s does not exist. Please specify ON/OFF" s


(** building a {!CIssue.t} piece by piece *)
type issue_in_construction =
  { issue_type_doc_url: string option
  ; issue_type_name: string option
        (** issue name, if no name is given name will be a readable version of id, by removing
            underscores and capitalizing first letters of words *)
  ; description: string
  ; mode: CIssue.mode
  ; loc: Location.t
  ; severity: IssueType.severity
  ; suggestion: string option }

(** Convert a parsed checker in list of linters *)
let create_parsed_linters linters_def_file checkers : linter list =
  let open CTL in
  L.(debug Linters Medium) "@\nConverting checkers in (condition, issue) pairs@\n" ;
  let do_one_checker checker : linter =
    let dummy_issue : issue_in_construction =
      { issue_type_doc_url= None
      ; issue_type_name= None
      ; description= ""
      ; suggestion= None
      ; loc= Location.dummy
      ; severity= Warning
      ; mode= On }
    in
    let issue_desc, condition, whitelist_paths, blacklist_paths =
      let process_linter_definitions (issue, cond, wl_paths, bl_paths) description =
        match description with
        | CSet (av, phi) when ALVar.is_report_when_keyword av ->
            (issue, phi, wl_paths, bl_paths)
        | CDesc (av, msg) when ALVar.is_message_keyword av ->
            ({issue with description= msg}, cond, wl_paths, bl_paths)
        | CDesc (av, sugg) when ALVar.is_suggestion_keyword av ->
            ({issue with suggestion= Some sugg}, cond, wl_paths, bl_paths)
        | CDesc (av, sev) when ALVar.is_severity_keyword av ->
            ({issue with severity= string_to_severity sev}, cond, wl_paths, bl_paths)
        | CDesc (av, m) when ALVar.is_mode_keyword av ->
            ({issue with mode= string_to_issue_mode m}, cond, wl_paths, bl_paths)
        | CDesc (av, doc) when ALVar.is_doc_url_keyword av ->
            ({issue with issue_type_doc_url= Some doc}, cond, wl_paths, bl_paths)
        | CDesc (av, name) when ALVar.is_name_keyword av ->
            ({issue with issue_type_name= Some name}, cond, wl_paths, bl_paths)
        | CPath (`WhitelistPath, paths) ->
            (issue, cond, paths, bl_paths)
        | CPath (`BlacklistPath, paths) ->
            (issue, cond, wl_paths, paths)
        | _ ->
            (issue, cond, wl_paths, bl_paths)
      in
      List.fold ~f:process_linter_definitions
        ~init:(dummy_issue, CTLTypes.False, [], [])
        checker.definitions
    in
    L.(debug Linters Medium) "@\nMaking condition and issue desc for checker '%s'@\n" checker.id ;
    L.(debug Linters Medium) "@\nCondition =@\n    %a@\n" CTLTypes.pp_formula condition ;
    let issue_type =
      let doc_url =
        Option.first_some
          (Config.get_linter_doc_url ~linter_id:checker.id)
          issue_desc.issue_type_doc_url
      in
      IssueType.register_dynamic ~id:checker.id ?hum:issue_desc.issue_type_name ?doc_url
        ~linters_def_file:(Some linters_def_file) issue_desc.severity Linters
    in
    let issue_desc =
      { CIssue.issue_type
      ; description= issue_desc.description
      ; mode= issue_desc.mode
      ; loc= issue_desc.loc
      ; severity= issue_desc.severity
      ; suggestion= issue_desc.suggestion }
    in
    L.debug Linters Medium "@\nIssue_desc = %a@\n" CIssue.pp issue_desc ;
    {condition; issue_desc; whitelist_paths; blacklist_paths}
  in
  List.map ~f:do_one_checker checkers


let rec apply_substitution f sub =
  let sub_param p =
    try snd (List.find_exn sub ~f:(fun (a, _) -> ALVar.equal p a))
    with Not_found_s _ | Caml.Not_found -> p
  in
  let sub_list_param ps = List.map ps ~f:sub_param in
  let open CTLTypes in
  match f with
  | True | False ->
      f
  | Atomic (name, ps) ->
      Atomic (name, sub_list_param ps)
  | Not f1 ->
      Not (apply_substitution f1 sub)
  | And (f1, f2) ->
      And (apply_substitution f1 sub, apply_substitution f2 sub)
  | AndWithWitnesses (f1, f2, (name, ps)) ->
      AndWithWitnesses
        (apply_substitution f1 sub, apply_substitution f2 sub, (name, sub_list_param ps))
  | Or (f1, f2) ->
      Or (apply_substitution f1 sub, apply_substitution f2 sub)
  | Implies (f1, f2) ->
      Implies (apply_substitution f1 sub, apply_substitution f2 sub)
  | InNode (node_type_list, f1) ->
      InNode (sub_list_param node_type_list, apply_substitution f1 sub)
  | AU (trans, f1, f2) ->
      AU (trans, apply_substitution f1 sub, apply_substitution f2 sub)
  | EU (trans, f1, f2) ->
      EU (trans, apply_substitution f1 sub, apply_substitution f2 sub)
  | EF (trans, f1) ->
      EF (trans, apply_substitution f1 sub)
  | AF (trans, f1) ->
      AF (trans, apply_substitution f1 sub)
  | AG (trans, f1) ->
      AG (trans, apply_substitution f1 sub)
  | EX (trans, f1) ->
      EX (trans, apply_substitution f1 sub)
  | AX (trans, f1) ->
      AX (trans, apply_substitution f1 sub)
  | EH (cl, f1) ->
      EH (sub_list_param cl, apply_substitution f1 sub)
  | EG (trans, f1) ->
      EG (trans, apply_substitution f1 sub)
  | ET (ntl, sw, f1) ->
      ET (sub_list_param ntl, sw, apply_substitution f1 sub)
  | InObjCClass (f1, f2) ->
      InObjCClass (apply_substitution f1 sub, apply_substitution f2 sub)


let expand_formula phi map_ error_msg_ =
  let fail_with_circular_macro_definition name error_msg =
    L.(die ExternalError) "Macro '%s' has a circular definition.@\n Cycle:@\n%s" name error_msg
  in
  let open CTLTypes in
  let rec expand acc map error_msg =
    let expand_predicate av actual_param =
      match av with
      | ALVar.Formula_id name -> (
          (* it may be a macro *)
          let error_msg' = error_msg ^ "  -Expanding formula identifier '" ^ name ^ "'@\n" in
          try
            match ALVar.FormulaIdMap.find av map with
            | true, _, _ ->
                fail_with_circular_macro_definition name error_msg'
            | false, fparams, f1 -> (
              (* in this case it should be a defined macro *)
              match List.zip fparams actual_param with
              | Ok sub ->
                  let f1_sub = apply_substitution f1 sub in
                  let map' = ALVar.FormulaIdMap.add av (true, fparams, f1) map in
                  expand f1_sub map' error_msg'
              | Unequal_lengths ->
                  L.(die ExternalError)
                    "Formula identifier '%s' is not called with the right number of parameters" name
              )
          with Caml.Not_found -> acc )
      (* in this case it should be a predicate *)
    in
    match acc with
    | True | False ->
        acc
    | Atomic (av, actual_param) ->
        expand_predicate av actual_param
    | Not f1 ->
        Not (expand f1 map error_msg)
    | And (f1, f2) ->
        And (expand f1 map error_msg, expand f2 map error_msg)
    | AndWithWitnesses (f1, f2, pred) ->
        AndWithWitnesses (expand f1 map error_msg, expand f2 map error_msg, pred)
    | Or (f1, f2) ->
        Or (expand f1 map error_msg, expand f2 map error_msg)
    | Implies (f1, f2) ->
        Implies (expand f1 map error_msg, expand f2 map error_msg)
    | InNode (node_type_list, f1) ->
        InNode (node_type_list, expand f1 map error_msg)
    | AU (trans, f1, f2) ->
        AU (trans, expand f1 map error_msg, expand f2 map error_msg)
    | EU (trans, f1, f2) ->
        EU (trans, expand f1 map error_msg, expand f2 map error_msg)
    | EF (trans, f1) ->
        EF (trans, expand f1 map error_msg)
    | AF (trans, f1) ->
        AF (trans, expand f1 map error_msg)
    | AG (trans, f1) ->
        AG (trans, expand f1 map error_msg)
    | EX (trans, f1) ->
        EX (trans, expand f1 map error_msg)
    | AX (trans, f1) ->
        AX (trans, expand f1 map error_msg)
    | EH (cl, f1) ->
        EH (cl, expand f1 map error_msg)
    | EG (trans, f1) ->
        EG (trans, expand f1 map error_msg)
    | ET (tl, sw, f1) ->
        ET (tl, sw, expand f1 map error_msg)
    | InObjCClass (f1, f2) ->
        InObjCClass (expand f1 map error_msg, expand f2 map error_msg)
  in
  expand phi map_ error_msg_


let rec expand_path paths path_map =
  match paths with
  | [] ->
      []
  | ALVar.Var path_var :: rest -> (
    try
      let paths = ALVar.VarMap.find path_var path_map in
      List.append paths (expand_path rest path_map)
    with Caml.Not_found -> L.(die ExternalError) "Path variable %s not found. " path_var )
  | path :: rest ->
      path :: expand_path rest path_map


let build_macros_map_ macros init_map =
  let macros_map =
    List.fold
      ~f:(fun map' data ->
        match data with
        | CTL.CLet (key, params, formula) ->
            if ALVar.FormulaIdMap.mem key map' then
              L.(die ExternalError)
                "Macro '%s' has more than one definition." (ALVar.formula_id_to_string key)
            else ALVar.FormulaIdMap.add key (false, params, formula) map'
        | _ ->
            map' )
      ~init:init_map macros
  in
  macros_map


let build_macros_map macros =
  let init_map : macros_map = ALVar.FormulaIdMap.empty in
  build_macros_map_ macros init_map


let build_paths_map paths =
  let build_paths_map_aux paths init_map =
    let paths_map =
      List.fold
        ~f:(fun map' data ->
          match data with
          | path_name, paths ->
              if ALVar.VarMap.mem path_name map' then
                L.(die ExternalError) "Path '%s' has more than one definition." path_name
              else ALVar.VarMap.add path_name paths map' )
        ~init:init_map paths
    in
    paths_map
  in
  build_paths_map_aux paths ALVar.VarMap.empty


(* expands use of let defined formula id in checkers with their definition *)
let expand_checkers macro_map path_map checkers =
  let open CTL in
  let expand_one_checker c =
    L.(debug Linters Medium) " +Start expanding %s@\n" c.id ;
    let map = build_macros_map_ c.definitions macro_map in
    let exp_defs =
      List.fold
        ~f:(fun defs clause ->
          match clause with
          | CSet (report_when_const, phi) ->
              L.(debug Linters Medium) "  -Expanding report_when@\n" ;
              CSet (report_when_const, expand_formula phi map "") :: defs
          | CPath (black_or_white_list, paths) ->
              L.(debug Linters Medium) "  -Expanding path@\n" ;
              CPath (black_or_white_list, expand_path paths path_map) :: defs
          | cl ->
              cl :: defs )
        ~init:[] c.definitions
    in
    {c with definitions= exp_defs}
  in
  List.map ~f:expand_one_checker checkers


let issue_log = ref IssueLog.empty

(** Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue method_decl_opt (node : Ctl_parser_types.ast_node) (issue_desc : CIssue.t) =
  let procname =
    match method_decl_opt with
    | Some method_decl ->
        CType_decl.CProcname.from_decl_for_linters method_decl
    | None ->
        Procname.Linters_dummy_method
  in
  let issue_log', errlog = IssueLog.get_or_add ~proc:procname !issue_log in
  issue_log := issue_log' ;
  let err_desc =
    Localise.desc_frontend_warning issue_desc.description issue_desc.suggestion issue_desc.loc
  in
  let issue_to_report =
    {IssueToReport.issue_type= issue_desc.issue_type; description= err_desc; ocaml_pos= None}
  in
  let trace = [Errlog.make_trace_element 0 issue_desc.loc "" []] in
  let key_str =
    match node with
    | Decl dec ->
        CAst_utils.generate_key_decl dec
    | Stmt st ->
        CAst_utils.generate_key_stmt st
  in
  let node_key = Procdesc.NodeKey.of_frontend_node_key key_str in
  Reporting.log_frontend_issue errlog issue_to_report ~loc:issue_desc.loc ~ltr:trace ~node_key


let fill_issue_desc_info_and_log context ~witness ~current_node (issue_desc : CIssue.t) loc =
  let process_message message =
    remove_new_lines_and_whitespace (expand_message_string context message current_node)
  in
  let description = process_message issue_desc.description in
  let suggestion = Option.map ~f:process_message issue_desc.suggestion in
  let issue_desc' = {issue_desc with description; loc; suggestion} in
  try log_frontend_issue context.CLintersContext.current_method witness issue_desc'
  with CFrontend_errors.IncorrectAssumption _ -> ()


(* Calls the set of hard coded checkers (if any) *)
let invoke_set_of_hard_coded_checkers_an context (an : Ctl_parser_types.ast_node) =
  let checkers = match an with Decl _ -> decl_checkers_list | Stmt _ -> [] in
  List.iter
    ~f:(fun checker ->
      let issue_desc_list = checker context an in
      List.iter
        ~f:(fun issue_desc ->
          if CIssue.should_run_check issue_desc.CIssue.mode then
            fill_issue_desc_info_and_log context ~witness:an ~current_node:an issue_desc
              issue_desc.CIssue.loc )
        issue_desc_list )
    checkers


(* Calls the set of checkers parsed from files (if any) *)
let invoke_set_of_parsed_checkers_an parsed_linters context (an : Ctl_parser_types.ast_node) =
  List.iter
    ~f:(fun (linter : linter) ->
      if CIssue.should_run_check linter.issue_desc.CIssue.mode then
        match CTL.eval_formula linter.condition an context with
        | None ->
            ()
        | Some witness ->
            let loc = ALUtils.location_from_an context witness in
            fill_issue_desc_info_and_log context ~witness ~current_node:an linter.issue_desc loc )
    parsed_linters


(* We decouple the hardcoded checkers from the parsed ones *)
let invoke_set_of_checkers_on_node parsed_linters context an =
  ( match an with
  | Ctl_parser_types.Decl (Clang_ast_t.TranslationUnitDecl _) ->
      (* Don't run parsed linters on TranslationUnitDecl node.
         Because depending on the formula it may give an error at line -1 *)
      ()
  | _ ->
      invoke_set_of_parsed_checkers_an parsed_linters context an ) ;
  if Config.default_linters then invoke_set_of_hard_coded_checkers_an context an
