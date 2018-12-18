(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** backward analysis for computing set of maybe-live variables at each program point *)

module VarSet = AbstractDomain.FiniteSet (Var)
module Domain = VarSet

(* only kill pvars that are local; don't kill those that can escape *)
let is_always_in_scope pvar = Pvar.is_return pvar || Pvar.is_global pvar

(* compilers 101-style backward transfer functions for liveness analysis. gen a variable when it is
   read, kill the variable when it is assigned *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras

  (** add all of the vars read in [exp] to the live set *)
  let exp_add_live exp astate =
    let astate' =
      Exp.free_vars exp
      |> Sequence.fold ~init:astate ~f:(fun astate_acc id -> Domain.add (Var.of_id id) astate_acc)
    in
    Exp.program_vars exp
    |> Sequence.fold ~init:astate' ~f:(fun astate_acc pvar ->
           Domain.add (Var.of_pvar pvar) astate_acc )


  let add_live_actuals actuals call_exp live_acc =
    let add_live_actuals_ exps acc =
      List.fold exps ~f:(fun acc_ exp -> exp_add_live exp acc_) ~init:acc
    in
    let actuals = List.map actuals ~f:(fun (e, _) -> Exp.ignore_cast e) in
    match Exp.ignore_cast call_exp with
    | Exp.Const (Cfun (Typ.Procname.ObjC_Cpp _ as pname)) when Typ.Procname.is_constructor pname -> (
      (* first actual passed to a C++ constructor is actually written, not read *)
      match actuals with
      | Exp.Lvar pvar :: exps ->
          Domain.remove (Var.of_pvar pvar) live_acc |> add_live_actuals_ exps
      | exps ->
          add_live_actuals_ exps live_acc )
    | _ ->
        add_live_actuals_ actuals live_acc


  let exec_instr astate _ _ = function
    | Sil.Load (lhs_id, _, _, _) when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a read *)
        astate
    | Sil.Load (lhs_id, rhs_exp, _, _) ->
        Domain.remove (Var.of_id lhs_id) astate |> exp_add_live rhs_exp
    | Sil.Store (Lvar lhs_pvar, _, rhs_exp, _) ->
        let astate' =
          if is_always_in_scope lhs_pvar then astate (* never kill globals *)
          else Domain.remove (Var.of_pvar lhs_pvar) astate
        in
        exp_add_live rhs_exp astate'
    | Sil.Store (lhs_exp, _, rhs_exp, _) ->
        exp_add_live lhs_exp astate |> exp_add_live rhs_exp
    | Sil.Prune (exp, _, _, _) ->
        exp_add_live exp astate
    | Sil.Call ((ret_id, _), call_exp, actuals, _, {CallFlags.cf_assign_last_arg}) ->
        let actuals_to_read, astate =
          if cf_assign_last_arg then
            match IList.split_last_rev actuals with
            | Some ((Exp.Lvar pvar, _), actuals') when not (is_always_in_scope pvar) ->
                (actuals', Domain.remove (Var.of_pvar pvar) astate)
            | _ ->
                (actuals, astate)
          else (actuals, astate)
        in
        Domain.remove (Var.of_id ret_id) astate
        |> exp_add_live call_exp
        |> add_live_actuals actuals_to_read call_exp
    | Sil.ExitScope _ | Abstract _ | Nullify _ ->
        astate


  let pp_session_name node fmt = F.fprintf fmt "liveness %a" CFG.Node.pp_id (CFG.Node.id node)
end

module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Backward (ProcCfg.Exceptional))
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

(* It's fine to have a dead store on a type that uses the "scope guard" pattern. These types
   are only read in their destructors, and this is expected/ok.
   (e.g., https://github.com/facebook/folly/blob/master/folly/ScopeGuard.h). *)
let matcher_scope_guard =
  let of_json init = function
    | `List scope_guards ->
        List.fold scope_guards ~f:(fun acc json -> Yojson.Basic.Util.to_string json :: acc) ~init
    | _ ->
        init
  in
  let default_scope_guards = ["CKComponentKey"; "CKComponentScope"] in
  of_json default_scope_guards Config.cxx_scope_guards
  |> QualifiedCppName.Match.of_fuzzy_qual_names


module CapturedByRefTransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = VarSet

  type extras = ProcData.no_extras

  let exec_instr astate _ _ instr =
    List.fold (Sil.instr_get_exps instr)
      ~f:(fun acc exp ->
        Exp.fold_captured exp
          ~f:(fun acc exp ->
            match Exp.ignore_cast exp with
            | Exp.Lvar pvar ->
                (* captured by reference, add *)
                Domain.add (Var.of_pvar pvar) acc
            | _ ->
                (* captured by value or init-capture, skip *)
                acc )
          acc )
      ~init:astate


  let pp_session_name _node fmt = F.pp_print_string fmt "captured by ref"
end

module CapturedByRefAnalyzer =
  AbstractInterpreter.MakeRPO (CapturedByRefTransferFunctions (ProcCfg.Exceptional))

let get_captured_by_ref_invariant_map proc_desc proc_data =
  let cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
  CapturedByRefAnalyzer.exec_cfg cfg proc_data ~initial:VarSet.empty


let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
  let proc_data = ProcData.make_default proc_desc tenv in
  let captured_by_ref_invariant_map = get_captured_by_ref_invariant_map proc_desc proc_data in
  let cfg = CFG.from_pdesc proc_desc in
  let invariant_map = Analyzer.exec_cfg cfg proc_data ~initial:Domain.empty in
  (* we don't want to report in harmless cases like int i = 0; if (...) { i = ... } else { i = ... }
     that create an intentional dead store as an attempt to imitate default value semantics.
     use dead stores to a "sentinel" value as a heuristic for ignoring this case *)
  let rec is_sentinel_exp = function
    | Exp.Cast (_, e) ->
        is_sentinel_exp e
    | Exp.Const (Cint i) ->
        IntLit.iszero i || IntLit.isnull i
    | Exp.Const (Cfloat 0.0) ->
        true
    | _ ->
        false
  in
  let rec is_scope_guard = function
    | {Typ.desc= Tstruct name} ->
        QualifiedCppName.Match.match_qualifiers matcher_scope_guard (Typ.Name.qual_name name)
    | {Typ.desc= Tptr (typ, _)} ->
        is_scope_guard typ
    | _ ->
        false
  in
  let locals = Procdesc.get_locals proc_desc in
  let is_constexpr pvar =
    List.find locals ~f:(fun local_data ->
        Mangled.equal (Pvar.get_name pvar) local_data.ProcAttributes.name )
    |> Option.exists ~f:(fun local -> local.ProcAttributes.is_constexpr)
  in
  let should_report pvar typ live_vars captured_by_ref_vars =
    (* T32000971: a temporay fix for dead store false positives until we support __unused__ attribute *)
    let is_unused_tmp_var pvar = String.equal "__tmp" (Pvar.to_string pvar) in
    not
      ( Pvar.is_frontend_tmp pvar || Pvar.is_return pvar || Pvar.is_global pvar || is_constexpr pvar
      || is_unused_tmp_var pvar
      || VarSet.mem (Var.of_pvar pvar) captured_by_ref_vars
      || Domain.mem (Var.of_pvar pvar) live_vars
      || Procdesc.is_captured_pvar proc_desc pvar
      || is_scope_guard typ
      || Procdesc.has_modify_in_block_attr proc_desc pvar )
  in
  let log_report pvar typ loc =
    let message =
      F.asprintf "The value written to %a (type %a) is never used" (Pvar.pp Pp.text) pvar
        (Typ.pp_full Pp.text) typ
    in
    let ltr = [Errlog.make_trace_element 0 loc "Write of unused value" []] in
    Reporting.log_error summary ~loc ~ltr IssueType.dead_store message
  in
  let report_dead_store live_vars captured_by_ref_vars = function
    | Sil.Store (Lvar pvar, typ, rhs_exp, loc)
      when should_report pvar typ live_vars captured_by_ref_vars && not (is_sentinel_exp rhs_exp)
      ->
        log_report pvar typ loc
    | Sil.Call (_, e_fun, (arg, typ) :: _, loc, _) -> (
      match (Exp.ignore_cast e_fun, Exp.ignore_cast arg) with
      | Exp.Const (Cfun (Typ.Procname.ObjC_Cpp _ as pname)), Exp.Lvar pvar
        when Typ.Procname.is_constructor pname
             && should_report pvar typ live_vars captured_by_ref_vars ->
          log_report pvar typ loc
      | _, _ ->
          () )
    | _ ->
        ()
  in
  let report_on_node node =
    let captured_by_ref_vars =
      match
        CapturedByRefAnalyzer.extract_post
          (ProcCfg.Exceptional.Node.id (CFG.Node.underlying_node node))
          captured_by_ref_invariant_map
      with
      | Some post ->
          post
      | None ->
          VarSet.empty
    in
    let node_id = CFG.Node.id node in
    Instrs.iter (CFG.instrs node) ~f:(fun instr ->
        match Analyzer.extract_pre node_id invariant_map with
        | Some live_vars ->
            report_dead_store live_vars captured_by_ref_vars instr
        | None ->
            () )
  in
  Container.iter cfg ~fold:CFG.fold_nodes ~f:report_on_node ;
  summary
