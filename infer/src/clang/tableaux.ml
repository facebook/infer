(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CTLFormulaSet = Caml.Set.Make (CTL)

module ClosureHashtbl = Caml.Map.Make (struct
  type t = CTL.t [@@deriving compare]
end)

(* This map associates a true/false to a formula in a node to
   say if that is a good context where to evaluate the formula or not.
  This is an optimization that avoids evaluation formulas where not necessary *)
type context_linter_map = bool ClosureHashtbl.t

(* This map associate to a formula the set of its subformulae
   (is state only formula, linter_condition, list of subformulae of linter_condition) *)
let closure_map : (bool * CTL.t list) ClosureHashtbl.t ref = ref ClosureHashtbl.empty

type node_valuation_key = int * string [@@deriving compare]

(* Given a linter and a node, a node valuation is a set of
   formulae valid in that node. The formulae are subformulae of the
   linter.condition, and therefore their use is to be able to
   evaluate the linter condition. *)
module NodesValuationHashtbl = Caml.Map.Make (struct
  type t = node_valuation_key [@@deriving compare]
end)

type node_valuation = CTLFormulaSet.t

let global_nodes_valuation : node_valuation NodesValuationHashtbl.t ref =
  ref NodesValuationHashtbl.empty


let init_global_nodes_valuation () =
  global_nodes_valuation := NodesValuationHashtbl.empty ;
  closure_map := ClosureHashtbl.empty


let add_formula_to_valuation k s =
  global_nodes_valuation := NodesValuationHashtbl.add k s !global_nodes_valuation


let get_node_valuation k =
  try NodesValuationHashtbl.find k !global_nodes_valuation with Caml.Not_found ->
    CTLFormulaSet.empty


let is_decl_allowed lcxt decl =
  let decl_info = Clang_ast_proj.get_decl_tuple decl in
  CLocation.should_do_frontend_check lcxt.CLintersContext.translation_unit_context.source_file
    decl_info.Clang_ast_t.di_source_range


(* true if it's an InNode formulae *)
let is_in_formula phi = match phi with CTL.InNode _ -> true | _ -> false

(* Map initialized with false for InNode formula and true for others  *)
let init_active_map parsed_linters =
  List.fold
    ~f:(fun acc_map linter ->
      let not_inf = not (is_in_formula linter.CFrontend_errors.condition) in
      ClosureHashtbl.add linter.CFrontend_errors.condition not_inf acc_map )
    ~init:ClosureHashtbl.empty parsed_linters


(* update the context map for formulae of type InNode(tl, phi). When we
   pass from a node in the list tl. The idea is that the context map tell us
   when we are in a node that is a discendant of a node in tl so that is make
   sense to keep evaluation phi. Otherwise we can skip the evaluation of phi and
   its subformulae *)
let update_linter_context_map parsed_linters an linter_context_map =
  let do_one_linter acc_map linter =
    let phi = linter.CFrontend_errors.condition in
    match phi with
    | CTL.InNode (tl, _) -> (
      try
        if ClosureHashtbl.find phi linter_context_map then acc_map
        else
          let res = Ctl_parser_types.ast_node_has_kind tl an in
          (*L.(debug Linters Medium) "@\n Updating linter map for node %i with '%b'"
              (Ctl_parser_types.ast_node_pointer an)  res; *)
          ClosureHashtbl.add phi res acc_map
      with Caml.Not_found ->
        Logging.die InternalError "Every linter condition should have an entry in the map." )
    | _ ->
        acc_map
  in
  List.fold ~f:do_one_linter ~init:linter_context_map parsed_linters


(* Takes phi and transform it by an equivalent formula containing
only a minimal set of operators *)
let rec normalize phi =
  let open CTL in
  match phi with
  | True | False | Atomic _ ->
      phi
  | Implies (phi1, phi2) ->
      normalize (Or (Not phi1, phi2))
  | Or (phi1, phi2) ->
      let phi1' = normalize phi1 in
      let phi2' = normalize phi2 in
      Or (phi1', phi2')
  | And (phi1, phi2) ->
      let phi1' = normalize phi1 in
      let phi2' = normalize phi2 in
      And (phi1', phi2')
  | Not phi1 ->
      let phi1' = normalize phi1 in
      Not phi1'
  | AG (trans, phi1) ->
      let phi1' = normalize phi1 in
      Not (EF (trans, Not phi1'))
  | EX (trans, phi1) ->
      let phi1' = normalize phi1 in
      EX (trans, phi1')
  | EF (trans, phi1) ->
      let phi1' = normalize phi1 in
      EF (trans, phi1')
  | EG (trans, phi1) ->
      let phi1' = normalize phi1 in
      EG (trans, phi1')
  | AX (trans, phi1) ->
      let phi1' = normalize phi1 in
      Not (EX (trans, Not phi1'))
  | AF (trans, phi1) ->
      let phi1' = normalize phi1 in
      Not (EG (trans, Not phi1'))
  | EU (trans, phi1, phi2) ->
      let phi1' = normalize phi1 in
      let phi2' = normalize phi2 in
      EU (trans, phi1', phi2')
  | AU (trans, phi1, phi2) ->
      let phi1' = normalize phi1 in
      let phi2' = normalize phi2 in
      Not (Or (EU (trans, Not phi2', Not (Or (phi1', phi2'))), EG (trans, phi2')))
  | EH (cl, phi1) ->
      normalize (ET (cl, None, EX (Some Super, EF (Some Super, phi1))))
  | ET (tl, trs, phi1) -> (
      let phi1' = normalize phi1 in
      match trs with
      | Some _ ->
          EF (None, InNode (tl, EX (trs, phi1')))
      | None ->
          EF (None, InNode (tl, phi1')) )
  | InNode (nl, phi1) ->
      let phi1' = normalize phi1 in
      InNode (nl, phi1')
  | InObjCClass (phi1, phi2) ->
      let phi1' = normalize phi1 in
      let phi2' = normalize phi2 in
      InObjCClass (phi1', phi2')


(* Given a phi0 build the list of its subformulae including itself.
   The order of the list is such that, for any formula phi, its strict subformulae
   occur before. The order is important for the evaluation. *)
let formula_closure phi0 =
  let open CTL in
  let rec do_subformula phi =
    match phi with
    | True | False | Atomic _ ->
        [phi]
    | Not phi1 ->
        phi :: do_subformula phi1
    | And (phi1, phi2) | Or (phi1, phi2) | EU (_, phi1, phi2) | InObjCClass (phi1, phi2) ->
        let cl1 = do_subformula phi1 in
        let cl2 = do_subformula phi2 in
        phi :: (cl1 @ cl2)
    | EX (_, phi1) | EF (_, phi1) | EG (_, phi1) | InNode (_, phi1) ->
        phi :: do_subformula phi1
    | AG _ | AX _ | AF _ | AU _ | EH _ | ET _ | Implies _ ->
        Logging.die InternalError "@\n  Failing with formula @\n   %a@\n" CTL.Debug.pp_formula phi
  in
  let cl0 = do_subformula phi0 in
  let cl0' = List.rev cl0 in
  List.fold
    ~f:(fun acc e -> if List.mem acc e ~equal:CTL.equal then acc else acc @ [e])
    ~init:[] cl0'


(* check if there is a formula phi in the set of valid formula of
a successor *)
let exists_formula_in_successor_nodes an checker trans phi =
  (*L.(debug Linters Medium) "@\n  Successor nodes of %i: " (Ctl_parser_types.ast_node_pointer an) ;*)
  let succs =
    match trans with
    | Some l ->
        (* L.(debug Linters Medium) " (passing by '%a' transition) " CTL.Debug.pp_transition trans ;*)
        CTL.next_state_via_transition an l
    | None ->
        (*L.(debug Linters Medium) " (passing by None) " ;*)
        Ctl_parser_types.get_direct_successor_nodes an
  in
  (*List.iter
    ~f:(fun an' -> L.(debug Linters Medium) " [%i] " (Ctl_parser_types.ast_node_pointer an'))
    succs ;*)
  List.exists
    ~f:(fun an' ->
      let node_pointer = Ctl_parser_types.ast_node_pointer an' in
      let key = (node_pointer, checker) in
      let succ_sat_set = get_node_valuation key in
      (*print_formula_set succ_sat_set "SUCC_SAT_SET" ;*)
      CTLFormulaSet.mem phi succ_sat_set )
    succs


(* Given a node an and a closure cl, returns the subset of valid formulae of
   cl in an. The hipothesis is that you have constructed the set of valid formulae
for the successors of the node an *)
let add_valid_formulae an checker lcxt cl =
  let open CTL in
  (*let name = Ctl_parser_types.ast_node_kind an in
    let pointer = Ctl_parser_types.ast_node_pointer an in *)
  let add_in_set phi acc_set =
    (* L.(debug Linters Medium)
      "@\n **** In (%i, %s) ADDING FORMULA **** @\n   %a@\n@\n" pointer name CTL.Debug.pp_formula
      phi ; *)
    CTLFormulaSet.add phi acc_set
  in
  let is_valid phi acc_set = CTLFormulaSet.mem phi acc_set in
  let do_formula acc_set phi =
    (*  L.(debug Linters Medium)
      "@\n In (%i, %s) Dealing with formula @\n   %a@\n" pointer name CTL.Debug.pp_formula phi ;
        L.(debug Linters Medium) "@\n ---------------------------- @\n" ;*)
    match phi with
    | True ->
        add_in_set phi acc_set
    | False ->
        acc_set
    | Atomic _ ->
        if Option.is_some (eval_formula phi an lcxt) then add_in_set phi acc_set else acc_set
    | And (phi1, phi2) when is_valid phi1 acc_set && is_valid phi2 acc_set ->
        add_in_set phi acc_set
    | InObjCClass (phi1, phi2) when is_valid phi1 acc_set && is_valid phi2 acc_set ->
        add_in_set phi acc_set
    | Or (phi1, phi2) when is_valid phi1 acc_set || is_valid phi2 acc_set ->
        add_in_set phi acc_set
    | Not phi1 when not (is_valid phi1 acc_set) ->
        add_in_set phi acc_set
    | InNode (tl, phi1) when Ctl_parser_types.ast_node_has_kind tl an && is_valid phi1 acc_set ->
        add_in_set phi acc_set
    | EX (trans, phi1) when exists_formula_in_successor_nodes an checker trans phi1 ->
        add_in_set phi acc_set
    | EF (trans, phi1)
      when is_valid phi1 acc_set || exists_formula_in_successor_nodes an checker trans phi ->
        add_in_set phi acc_set
    | EG (trans, phi1)
      when is_valid phi1 acc_set && exists_formula_in_successor_nodes an checker trans phi ->
        add_in_set phi acc_set
    | EU (trans, phi1, phi2)
      when is_valid phi2 acc_set
           || (is_valid phi1 acc_set && exists_formula_in_successor_nodes an checker trans phi) ->
        add_in_set phi acc_set
    | AG _ | AX _ | AF _ | AU _ | EH _ | ET _ | Implies _ ->
        Logging.die InternalError
          "@\n\
          \ We should not have operators AG, AX, AF, AU, EH, ET.\n\
          \  Failing with formula @\n\
          \   %a@\n"
          CTL.Debug.pp_formula phi
    | _ ->
        acc_set
  in
  List.fold ~f:do_formula cl ~init:CTLFormulaSet.empty


(* true if it's a formulae that does not contain temporal operators
   and can be decided in a single node *)
let rec is_state_only_formula phi =
  let open CTL in
  match phi with
  | True | False | Atomic _ ->
      (*L.(debug Linters Medium) "@\n ****** FOUND state_only_formula ***** @\n" ;*) true
  | Not phi1 ->
      is_state_only_formula phi1
  | And (phi1, phi2) | Or (phi1, phi2) | Implies (phi1, phi2) | InObjCClass (phi1, phi2) ->
      is_state_only_formula phi1 && is_state_only_formula phi2
  | InNode (_, phi1) ->
      is_state_only_formula phi1
  | AX _ | EX _ | AF _ | EF _ | AG _ | EG _ | AU _ | EU _ | EH _ | ET _ ->
      false


(*  Report an issue provided that a declaration is allowed
    (i.e., it's in the analized file )*)
let report_issue an lcxt linter (*npo_condition*) =
  let open Ctl_parser_types in
  let open CFrontend_errors in
  (*let name = Ctl_parser_types.ast_node_kind an in
  let pointer = Ctl_parser_types.ast_node_pointer an in
  L.(debug Linters Medium)
    "@\n@\n@\n  ***** In (%i, %s) Reporting because we found @\n%a@\n@\n@\n@\n" pointer name
    CTL.Debug.pp_formula linter.condition ;*)
  let loc = CFrontend_checkers.location_from_an lcxt an in
  let should_report = match an with Decl dec -> is_decl_allowed lcxt dec | Stmt _ -> true in
  if should_report then
    fill_issue_desc_info_and_log lcxt ~witness:an ~current_node:an linter.issue_desc loc


let check_linter_map linter_map_contex phi =
  try ClosureHashtbl.find phi linter_map_contex with Caml.Not_found ->
    Logging.die InternalError "@\n ERROR: linter_map must have an entry for each formula"


(* skip the evaluation of a InNode because an is not among the set tl *)
let skip_evaluation_InNode_formula an phi =
  match phi with
  | CTL.InNode (tl, _) when not (Ctl_parser_types.ast_node_has_kind tl an) ->
      true
  | _ ->
      false


(* Build valuation, i.e. set of valid subformula for a pair (node, checker) *)
let build_valuation parsed_linters an lcxt linter_map_context =
  let open CFrontend_errors in
  let node_pointer = Ctl_parser_types.ast_node_pointer an in
  (*L.(debug Linters Medium)
    "@\n@\n  ******** Tableaux called for node %i ******** @\n@\n" node_pointer ;*)
  let do_one_check linter =
    (*transition_set_in_formula := TransitionSet.empty ;
      build_transition_set npo_condition ; *)
    let normalized_condition = normalize linter.condition in
    let is_state_only, cl =
      try ClosureHashtbl.find normalized_condition !closure_map with Caml.Not_found ->
        let cl' = formula_closure normalized_condition in
        let is_state_only = is_state_only_formula normalized_condition in
        (*print_closure cl' ; *)
        closure_map := ClosureHashtbl.add normalized_condition (is_state_only, cl') !closure_map ;
        (is_state_only, cl')
    in
    if not (is_state_only && skip_evaluation_InNode_formula an normalized_condition) then (
      let sat_set =
        add_valid_formulae an linter.issue_desc.issue_type.IssueType.unique_id lcxt cl
      in
      (*L.progress " [Set Size: %i] @\n" (CTLFormulaSet.cardinal sat_set);*)
      if CTLFormulaSet.mem normalized_condition sat_set then report_issue an lcxt linter ;
      add_formula_to_valuation
        (node_pointer, linter.issue_desc.issue_type.IssueType.unique_id)
        sat_set )
  in
  List.iter
    ~f:(fun (linter : linter) ->
      if
        CIssue.should_run_check linter.issue_desc.CIssue.mode
        && check_linter_map linter_map_context linter.condition
      then do_one_check linter )
    parsed_linters
