(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** Forward analysis to compute uninitialized variables at each program point *)
module D =
UninitDomain.Domain
module UninitVars = AbstractDomain.FiniteSet (Var)
module AliasedVars = AbstractDomain.FiniteSet (UninitDomain.VarPair)
module PrePost = AbstractDomain.Pair (D) (D)
module RecordDomain = UninitDomain.Record (UninitVars) (AliasedVars) (D)

module Summary = Summary.Make (struct
  type payload = UninitDomain.summary

  let update_payload sum (summary: Specs.summary) =
    {summary with payload= {summary.payload with uninit= Some sum}}


  let read_payload (summary: Specs.summary) = summary.payload.uninit
end)

let intraprocedural_only = true

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RecordDomain

  type extras = FormalMap.t

  let exec_instr (astate: Domain.astate) {ProcData.extras} _ (instr: HilInstr.t) =
    match instr with
    | Assign (((lhs_var, _), _), HilExp.AccessPath (((rhs_var, rhs_typ) as rhs_base), _), _) ->
        let uninit_vars = D.remove lhs_var astate.uninit_vars in
        let prepost =
          if FormalMap.is_formal rhs_base extras
             && match rhs_typ.desc with Typ.Tptr _ -> true | _ -> false
          then
            let pre' = D.add rhs_var (fst astate.prepost) in
            let post = snd astate.prepost in
            (pre', post)
          else astate.prepost
        in
        {astate with uninit_vars; prepost}
    | Assign (((lhs_var, _), _), _, _) ->
        let uninit_vars = D.remove lhs_var astate.uninit_vars in
        {astate with uninit_vars}
    | Call (_, _, actuals, _, _) ->
        (* in case of intraprocedural only analysis we assume that parameters passed by reference
           to a function will be initialized inside that function *)
        let uninit_vars =
          List.fold
            ~f:(fun acc actual_exp ->
              match actual_exp with
              | HilExp.AccessPath ((actual_var, {Typ.desc= Tptr _}), _) ->
                  D.remove actual_var acc
              | _ ->
                  acc)
            ~init:astate.uninit_vars actuals
        in
        {astate with uninit_vars}
    | Assume _ ->
        astate

end

module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
module Analyzer =
  AbstractInterpreter.Make (CFG) (LowerHil.Make (TransferFunctions) (LowerHil.DefaultConfig))

let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
  let cfg = CFG.from_pdesc proc_desc in
  (* start with empty set of uninit local vars and  empty set of init formal params *)
  let formal_map = FormalMap.make proc_desc in
  let uninit_vars =
    List.map
      ~f:(fun (name, _) ->
        let pvar = Pvar.mk name (Procdesc.get_proc_name proc_desc) in
        Var.of_pvar pvar)
      (Procdesc.get_locals cfg)
  in
  let init =
    ( { RecordDomain.uninit_vars= UninitVars.of_list uninit_vars
      ; RecordDomain.aliased_vars= AliasedVars.empty
      ; RecordDomain.prepost= (D.empty, D.empty) }
    , IdAccessPathMapDomain.empty )
  in
  let invariant_map =
    Analyzer.exec_cfg cfg (ProcData.make proc_desc tenv formal_map) ~initial:init ~debug:true
  in
  (* Check if an expression is in set of variables *)
  let exp_in_set exp vset =
    let _, pvars = Exp.get_vars exp in
    List.exists ~f:(fun pv -> D.mem (Var.of_pvar pv) vset) pvars
  in
  let zip_actual_formal_params callee_pname actual_params =
    match Ondemand.get_proc_desc callee_pname with
    | Some pdesc ->
        let formals, _ = List.unzip (Procdesc.get_formals pdesc) in
        let actual, _ = List.unzip actual_params in
        (List.zip actual formals, actual)
    | _ ->
        (None, [])
  in
  let deref_actual_params callee_pname actual_params deref_formal_params =
    match zip_actual_formal_params callee_pname actual_params with
    | None, _ ->
        []
    | Some assoc_actual_formal, _ ->
        List.fold
          ~f:(fun acc (a, f) ->
            let fe = Exp.Lvar (Pvar.mk f callee_pname) in
            if exp_in_set fe deref_formal_params then a :: acc else acc)
          ~init:[] assoc_actual_formal
  in
  let report_uninit_value uninit_vars instr =
    let report message loc =
      let issue_id = IssueType.uninitialized_value.unique_id in
      let ltr = [Errlog.make_trace_element 0 loc "" []] in
      let exn = Exceptions.Checkers (issue_id, Localise.verbatim_desc message) in
      Reporting.log_error summary ~loc ~ltr exn
    in
    match instr with
    | Sil.Load (_, Exp.Lvar pv, _, loc)
    | Sil.Store (_, _, Exp.Lvar pv, loc)
      when exp_in_set (Exp.Lvar pv) uninit_vars ->
        let message =
          F.asprintf "The value read from %a was never initialized" Exp.pp (Exp.Lvar pv)
        in
        report message loc
    | Sil.Call (_, Exp.Const Const.Cfun callee_pname, actual_params, loc, _)
      when not intraprocedural_only -> (
      match Summary.read_summary proc_desc callee_pname with
      | Some {pre= deref_formal_params; post= _} ->
          let deref_actual_params =
            deref_actual_params callee_pname actual_params deref_formal_params
          in
          List.iter
            ~f:(fun (e, _) ->
              if exp_in_set e uninit_vars && List.mem ~equal:Exp.equal deref_actual_params e then
                let message =
                  F.asprintf "The value of %a is read in %a but was never initialized" Exp.pp e
                    Typ.Procname.pp callee_pname
                in
                report message loc)
            actual_params
      | _ ->
          () )
    | _ ->
        ()
  in
  let report_on_node node =
    List.iter (CFG.instr_ids node) ~f:(fun (instr, node_id_opt) ->
        match node_id_opt with
        | Some node_id -> (
          match Analyzer.extract_pre node_id invariant_map with
          | Some
              ( { RecordDomain.uninit_vars= uninitialized_vars
                ; RecordDomain.aliased_vars= _
                ; RecordDomain.prepost= _ }
              , _ ) ->
              report_uninit_value uninitialized_vars instr
          | None ->
              () )
        | None ->
            () )
  in
  List.iter (CFG.nodes cfg) ~f:report_on_node ;
  match Analyzer.extract_post (CFG.id (CFG.exit_node cfg)) invariant_map with
  | Some
      ( {RecordDomain.uninit_vars= _; RecordDomain.aliased_vars= _; RecordDomain.prepost= pre, post}
      , _ ) ->
      Summary.update_summary {pre; post} summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Uninit analyzer failed to compute post for %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary

