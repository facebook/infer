(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module BoSummary = BufferOverrunAnalysisSummary
module BoUtils = BufferOverrunUtils
module CFG = BufferOverrunAnalysis.CFG
module Dom = BufferOverrunDomain
module F = Format
module L = Logging
module Models = BufferOverrunModels
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace

module UnusedBranch = struct
  type t = {node: CFG.Node.t; location: Location.t; condition: Exp.t; true_branch: bool}

  let report {InterproceduralAnalysis.proc_desc; err_log} {location; condition; true_branch} =
    let desc =
      let err_desc =
        let i = match condition with Exp.Const (Const.Cint i) -> i | _ -> IntLit.zero in
        Localise.desc_condition_always_true_false i None location
      in
      F.asprintf "%a" Localise.pp_error_desc err_desc
    in
    let issue_type =
      if true_branch then IssueType.condition_always_false else IssueType.condition_always_true
    in
    let ltr = [Errlog.make_trace_element 0 location "Here" []] in
    Reporting.log_issue proc_desc err_log ~loc:location ~ltr BufferOverrunChecker issue_type desc
end

module UnusedBranches = struct
  type t = UnusedBranch.t list

  let empty = []

  let report analysis_data unused_branches =
    List.iter unused_branches ~f:(UnusedBranch.report analysis_data)
end

module UnreachableStatement = struct
  type t = {location: Location.t}

  let report {InterproceduralAnalysis.proc_desc; err_log} {location} =
    let ltr = [Errlog.make_trace_element 0 location "Here" []] in
    Reporting.log_issue proc_desc err_log ~loc:location ~ltr BufferOverrunChecker
      IssueType.unreachable_code_after "Unreachable code after statement"
end

module UnreachableStatements = struct
  type t = UnreachableStatement.t list

  let empty = []

  let report analysis_data unreachable_statements =
    List.iter unreachable_statements ~f:(UnreachableStatement.report analysis_data)
end

module Checks = struct
  type t =
    { cond_set: PO.ConditionSet.checked_t
    ; unused_branches: UnusedBranches.t
    ; unreachable_statements: UnreachableStatements.t }

  let empty =
    { cond_set= PO.ConditionSet.empty
    ; unused_branches= UnusedBranches.empty
    ; unreachable_statements= UnreachableStatements.empty }
end

module ExitStatement = struct
  (* check that we are the last significant instruction
     * of a procedure (no more significant instruction)
     * or of a block (goes directly to a node with multiple predecessors)
  *)
  let rec is_end_of_block_or_procedure (cfg : CFG.t) node rem_instrs =
    Instrs.for_all rem_instrs ~f:Sil.instr_is_auxiliary
    &&
    match IContainer.singleton_or_more node ~fold:(CFG.fold_succs cfg) with
    | IContainer.Empty ->
        true
    | Singleton succ ->
        (* [succ] is a join, i.e. [node] is the end of a block *)
        IContainer.mem_nth succ 1 ~fold:(CFG.fold_preds cfg)
        || is_end_of_block_or_procedure cfg succ (CFG.instrs succ)
    | More ->
        false
end

let add_unreachable_code (cfg : CFG.t) (node : CFG.Node.t) instr rem_instrs (checks : Checks.t) =
  match instr with
  | Sil.Prune (_, _, _, (Ik_land_lor | Ik_bexp _)) ->
      checks
  | Sil.Prune (condition, location, true_branch, _) ->
      let unused_branch = UnusedBranch.{node; location; condition; true_branch} in
      {checks with unused_branches= unused_branch :: checks.unused_branches}
  (* special case for `exit` when we're at the end of a block / procedure *)
  | Sil.Call (_, Const (Cfun pname), _, _, _)
    when String.equal (Procname.get_method pname) "exit"
         && ExitStatement.is_end_of_block_or_procedure cfg node rem_instrs ->
      checks
  | _ ->
      let location = Sil.location_of_instr instr in
      let unreachable_statement = UnreachableStatement.{location} in
      {checks with unreachable_statements= unreachable_statement :: checks.unreachable_statements}


let check_binop_array_access :
       IntegerWidths.t
    -> is_plus:bool
    -> e1:Exp.t
    -> e2:Exp.t
    -> Location.t
    -> Dom.Mem.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t =
 fun integer_type_widths ~is_plus ~e1 ~e2 location mem cond_set ->
  let arr = Sem.eval integer_type_widths e1 mem in
  let idx = Sem.eval integer_type_widths e2 mem in
  let latest_prune = Dom.Mem.get_latest_prune mem in
  BoUtils.Check.array_access ~arr ~idx ~is_plus ~last_included:false ~latest_prune location cond_set


let check_binop :
       IntegerWidths.t
    -> bop:Binop.t
    -> e1:Exp.t
    -> e2:Exp.t
    -> Location.t
    -> Dom.Mem.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t =
 fun integer_type_widths ~bop ~e1 ~e2 location mem cond_set ->
  match bop with
  | Binop.PlusPI ->
      check_binop_array_access integer_type_widths ~is_plus:true ~e1 ~e2 location mem cond_set
  | Binop.MinusPI ->
      check_binop_array_access integer_type_widths ~is_plus:false ~e1 ~e2 location mem cond_set
  | _ ->
      cond_set


let check_expr_for_array_access :
       ?sub_expr_only:bool
    -> IntegerWidths.t
    -> Exp.t
    -> Location.t
    -> Dom.Mem.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t =
 fun ?(sub_expr_only = false) integer_type_widths exp location mem cond_set ->
  let rec check_sub_expr exp cond_set =
    match exp with
    | Exp.Lindex (array_exp, index_exp) ->
        cond_set |> check_sub_expr array_exp |> check_sub_expr index_exp
        |> BoUtils.Check.lindex integer_type_widths ~array_exp ~index_exp ~last_included:false mem
             location
    | Exp.BinOp (_, e1, e2) ->
        cond_set |> check_sub_expr e1 |> check_sub_expr e2
    | Exp.Lfield (e, _, _) | Exp.UnOp (_, e, _) | Exp.Exn e ->
        check_sub_expr e cond_set
    | Exp.Cast (_, e) ->
        check_sub_expr e cond_set
    | Exp.Closure {captured_vars} ->
        List.fold captured_vars ~init:cond_set ~f:(fun cond_set (e, _, _, _) ->
            check_sub_expr e cond_set )
    | Exp.Var _ | Exp.Lvar _ | Exp.Const _ | Exp.Sizeof _ ->
        cond_set
  in
  let cond_set = check_sub_expr exp cond_set in
  if sub_expr_only then cond_set
  else
    match exp with
    | Exp.Var _ ->
        let arr = Sem.eval integer_type_widths exp mem in
        let idx = Dom.Val.Itv.zero in
        let latest_prune = Dom.Mem.get_latest_prune mem in
        BoUtils.Check.array_access ~arr ~idx ~is_plus:true ~last_included:false ~latest_prune
          location cond_set
    | Exp.BinOp (bop, e1, e2) ->
        check_binop integer_type_widths ~bop ~e1 ~e2 location mem cond_set
    | _ ->
        cond_set


let check_binop_for_integer_overflow integer_type_widths pname bop ~lhs ~rhs location mem cond_set =
  match bop with
  | Binop.MinusA (Some typ) when Typ.ikind_is_unsigned typ && Exp.is_zero lhs && Exp.is_const rhs ->
      cond_set
  | Binop.PlusA (Some _) | Binop.MinusA (Some _) | Binop.Mult (Some _) ->
      let lhs_v = Sem.eval integer_type_widths lhs mem in
      let rhs_v = Sem.eval integer_type_widths rhs mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      BoUtils.Check.binary_operation integer_type_widths pname bop ~lhs:lhs_v ~rhs:rhs_v
        ~latest_prune location cond_set
  | _ ->
      cond_set


let rec check_expr_for_integer_overflow integer_type_widths pname exp location mem cond_set =
  match exp with
  | Exp.UnOp (_, e, _)
  | Exp.Exn e
  | Exp.Lfield (e, _, _)
  | Exp.Cast (_, e)
  | Exp.Sizeof {dynamic_length= Some e} ->
      check_expr_for_integer_overflow integer_type_widths pname e location mem cond_set
  | Exp.BinOp (bop, lhs, rhs) ->
      cond_set
      |> check_binop_for_integer_overflow integer_type_widths pname bop ~lhs ~rhs location mem
      |> check_expr_for_integer_overflow integer_type_widths pname lhs location mem
      |> check_expr_for_integer_overflow integer_type_widths pname rhs location mem
  | Exp.Lindex (e1, e2) ->
      cond_set
      |> check_expr_for_integer_overflow integer_type_widths pname e1 location mem
      |> check_expr_for_integer_overflow integer_type_widths pname e2 location mem
  | Exp.Closure {captured_vars} ->
      List.fold captured_vars ~init:cond_set ~f:(fun cond_set (e, _, _, _) ->
          check_expr_for_integer_overflow integer_type_widths pname e location mem cond_set )
  | Exp.Var _ | Exp.Const _ | Exp.Lvar _ | Exp.Sizeof {dynamic_length= None} ->
      cond_set


let instantiate_cond :
       is_args_ref:bool
    -> IntegerWidths.t
    -> Procname.t
    -> (Pvar.t * Typ.t) list
    -> (Exp.t * Typ.t) list
    -> (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
    -> Dom.Mem.t
    -> BufferOverrunCheckerSummary.t
    -> Location.t
    -> PO.ConditionSet.checked_t =
 fun ~is_args_ref integer_type_widths callee_pname callee_formals args captured_vars caller_mem
     callee_cond location ->
  let eval_sym_trace =
    Sem.mk_eval_sym_trace ~is_args_ref integer_type_widths callee_formals args captured_vars
      caller_mem
  in
  let latest_prune = Dom.Mem.get_latest_prune caller_mem in
  PO.ConditionSet.subst callee_cond eval_sym_trace callee_pname location latest_prune


type checks_summary = BufferOverrunCheckerSummary.t

type get_checks_summary = Procname.t -> checks_summary option

let check_call get_checks_summary get_summary get_formals pname tenv integer_type_widths node
    callee_pname args captured_vars location mem cond_set =
  let cond_set =
    List.fold args ~init:cond_set ~f:(fun cond_set (exp, _) ->
        check_expr_for_integer_overflow integer_type_widths pname exp location mem cond_set )
  in
  let fun_arg_list =
    List.map args ~f:(fun (exp, typ) ->
        ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
  in
  match Models.Call.dispatch tenv callee_pname fun_arg_list with
  | Some {Models.check} ->
      let model_env =
        let node_hash = CFG.Node.hash node in
        BoUtils.ModelEnv.mk_model_env callee_pname ~caller_pname:pname ~node_hash location tenv
          integer_type_widths get_summary
      in
      check model_env mem cond_set
  | None -> (
      let {BoUtils.ReplaceCallee.pname= callee_pname; args; is_args_ref} =
        BoUtils.ReplaceCallee.replace_make_shared tenv get_formals callee_pname args
      in
      match (get_checks_summary callee_pname, get_formals callee_pname) with
      | Some callee_condset, Some callee_formals ->
          instantiate_cond ~is_args_ref integer_type_widths callee_pname callee_formals args
            captured_vars mem callee_condset location
          |> PO.ConditionSet.join cond_set
      | _, _ ->
          (* unknown call / no inferbo payload *) cond_set )


let check_instr :
       get_checks_summary
    -> BoSummary.get_summary
    -> BoUtils.get_formals
    -> Procname.t
    -> Tenv.t
    -> IntegerWidths.t
    -> CFG.Node.t
    -> Sil.instr
    -> Dom.Mem.t
    -> PO.ConditionSet.checked_t
    -> PO.ConditionSet.checked_t =
 fun get_checks_summary get_summary get_formals pname tenv integer_type_widths node instr mem
     cond_set ->
  match instr with
  | Sil.Load {e= exp; loc= location} ->
      cond_set
      |> check_expr_for_array_access integer_type_widths exp location mem
      |> check_expr_for_integer_overflow integer_type_widths pname exp location mem
  | Sil.Store {e1= lexp; e2= rexp; loc= location} ->
      cond_set
      |> check_expr_for_array_access integer_type_widths lexp location mem
      |> check_expr_for_array_access ~sub_expr_only:true integer_type_widths rexp location mem
      |> check_expr_for_integer_overflow integer_type_widths pname lexp location mem
      |> check_expr_for_integer_overflow integer_type_widths pname rexp location mem
  | Sil.Call (_, Const (Cfun callee_pname), args, location, _) ->
      check_call get_checks_summary get_summary get_formals pname tenv integer_type_widths node
        callee_pname args [] location mem cond_set
  | Sil.Call (_, Closure {name= callee_pname; captured_vars}, args, location, _) ->
      check_call get_checks_summary get_summary get_formals pname tenv integer_type_widths node
        callee_pname args captured_vars location mem cond_set
  | Sil.Prune (exp, location, _, _) ->
      check_expr_for_integer_overflow integer_type_widths pname exp location mem cond_set
  | _ ->
      cond_set


let print_debug_info : Sil.instr -> Dom.Mem.t -> PO.ConditionSet.checked_t -> unit =
 fun instr pre cond_set ->
  L.(debug BufferOverrun Verbose) "@\n@\n================================@\n" ;
  L.(debug BufferOverrun Verbose) "@[<v 2>Pre-state : @,%a" Dom.Mem.pp pre ;
  L.(debug BufferOverrun Verbose) "@]@\n@\n%a" (Sil.pp_instr ~print_types:true Pp.text) instr ;
  L.(debug BufferOverrun Verbose) "@\n@\n@[<v 2>%a" PO.ConditionSet.pp cond_set ;
  L.(debug BufferOverrun Verbose) "@]@\n" ;
  L.(debug BufferOverrun Verbose) "================================@\n@."


let check_instrs :
       get_checks_summary
    -> BoSummary.get_summary
    -> BoUtils.get_formals
    -> Procname.t
    -> Tenv.t
    -> IntegerWidths.t
    -> CFG.t
    -> CFG.Node.t
    -> Instrs.not_reversed_t
    -> Dom.Mem.t AbstractInterpreter.State.t
    -> Checks.t
    -> Checks.t =
 fun get_checks_summary get_summary get_formals pname tenv integer_type_widths cfg node instrs state
     checks ->
  match state with
  | _ when Instrs.is_empty instrs ->
      checks
  | {AbstractInterpreter.State.pre= Dom.Mem.(Unreachable | ExcRaised)} ->
      checks
  | {AbstractInterpreter.State.pre= Reachable _ as pre; post} ->
      if Instrs.nth_exists instrs 1 then L.(die InternalError) "Did not expect several instructions" ;
      let instr = Instrs.nth_exn instrs 0 in
      let checks =
        match post with
        | Dom.Mem.(Unreachable | ExcRaised) ->
            add_unreachable_code cfg node instr Instrs.empty checks
        | Dom.Mem.Reachable _ ->
            checks
      in
      let cond_set =
        check_instr get_checks_summary get_summary get_formals pname tenv integer_type_widths node
          instr pre checks.cond_set
      in
      print_debug_info instr pre cond_set ;
      {checks with cond_set}


let check_node :
       get_checks_summary
    -> BoSummary.get_summary
    -> BoUtils.get_formals
    -> Procname.t
    -> Tenv.t
    -> IntegerWidths.t
    -> CFG.t
    -> BufferOverrunAnalysis.invariant_map
    -> Checks.t
    -> CFG.Node.t
    -> Checks.t =
 fun get_checks_summary get_summary get_formals pname tenv integer_type_widths cfg inv_map checks
     node ->
  match BufferOverrunAnalysis.extract_state (CFG.Node.id node) inv_map with
  | Some state ->
      let instrs = CFG.instrs node in
      check_instrs get_checks_summary get_summary get_formals pname tenv integer_type_widths cfg
        node instrs state checks
  | _ ->
      checks


type checks = Checks.t

let compute_checks :
       get_checks_summary
    -> BoSummary.get_summary
    -> BoUtils.get_formals
    -> Procname.t
    -> Tenv.t
    -> IntegerWidths.t
    -> CFG.t
    -> BufferOverrunAnalysis.invariant_map
    -> checks =
 fun get_checks_summary get_summary get_formals pname tenv integer_type_widths cfg inv_map ->
  CFG.fold_nodes cfg
    ~f:
      (check_node get_checks_summary get_summary get_formals pname tenv integer_type_widths cfg
         inv_map )
    ~init:Checks.empty


let report_errors ({InterproceduralAnalysis.proc_desc; err_log} as analysis_data)
    {Checks.cond_set; unused_branches; unreachable_statements} =
  UnusedBranches.report analysis_data unused_branches ;
  UnreachableStatements.report analysis_data unreachable_statements ;
  let report cond trace issue_type =
    let location = PO.ConditionTrace.get_report_location trace in
    let description ~markup = PO.description ~markup cond trace in
    let trace =
      let description = description ~markup:false in
      Trace.Issue.make_err_trace ~description (PO.ConditionTrace.get_val_traces trace)
      |> Errlog.concat_traces
    in
    Reporting.log_issue proc_desc err_log ~loc:location ~ltr:trace BufferOverrunChecker issue_type
      (description ~markup:true)
  in
  PO.ConditionSet.report_errors ~report cond_set


let get_checks_summary : checks -> checks_summary =
 fun Checks.
       { cond_set
       ; unused_branches= _ (* intra-procedural *)
       ; unreachable_statements= _ (* intra-procedural *) } ->
  PO.ConditionSet.for_summary cond_set


let checker ({InterproceduralAnalysis.proc_desc; tenv; exe_env; analyze_dependency} as analysis_data)
    =
  let open IOption.Let_syntax in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let+ inv_map =
    BufferOverrunAnalysis.cached_compute_invariant_map
      (InterproceduralAnalysis.bind_payload_opt analysis_data ~f:snd)
  in
  let underlying_exit_node = Procdesc.get_exit_node proc_desc in
  let pp_name f = F.pp_print_string f "bufferoverrun check" in
  AnalysisCallbacks.html_debug_new_node_session ~pp_name underlying_exit_node ~f:(fun () ->
      let cfg = CFG.from_pdesc proc_desc in
      let checks =
        let open IOption.Let_syntax in
        let get_checks_summary callee_pname =
          analyze_dependency callee_pname |> AnalysisResult.to_option >>= fst
        in
        let get_summary callee_pname =
          analyze_dependency callee_pname |> AnalysisResult.to_option >>= snd
        in
        let get_formals callee_pname =
          Attributes.load callee_pname >>| ProcAttributes.get_pvar_formals
        in
        compute_checks get_checks_summary get_summary get_formals proc_name tenv integer_type_widths
          cfg inv_map
      in
      report_errors analysis_data checks ;
      get_checks_summary checks )
