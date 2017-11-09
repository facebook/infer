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

let blacklisted_functions = [BuiltinDecl.__set_array_length]

let is_type_pointer t = match t.Typ.desc with Typ.Tptr _ -> true | _ -> false

let is_blacklisted_function call =
  match call with
  | HilInstr.Direct pname ->
      List.exists ~f:(fun fname -> Typ.Procname.equal pname fname) blacklisted_functions
  | _ ->
      false


let exp_in_set exp vset =
  let _, pvars = Exp.get_vars exp in
  List.exists ~f:(fun pv -> D.mem (Var.of_pvar pv) vset) pvars


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RecordDomain

  let report var loc summary =
    let message = F.asprintf "The value read from %a was never initialized" Var.pp var in
    let issue_id = IssueType.uninitialized_value.unique_id in
    let ltr = [Errlog.make_trace_element 0 loc "" []] in
    let exn = Exceptions.Checkers (issue_id, Localise.verbatim_desc message) in
    Reporting.log_error summary ~loc ~ltr exn


  type extras = FormalMap.t * Specs.summary

  let should_report_var pdesc uninit_vars var =
    match var with
    | Var.ProgramVar pv ->
        not (Pvar.is_frontend_tmp pv) && not (Procdesc.is_captured_var pdesc pv)
        && exp_in_set (Exp.Lvar pv) uninit_vars
    | _ ->
        false


  let report_on_function_params pdesc uninit_vars actuals loc extras =
    List.iter
      ~f:(fun e ->
        match e with
        | HilExp.AccessPath ((var, t), _)
          when should_report_var pdesc uninit_vars var && not (is_type_pointer t) ->
            report var loc (snd extras)
        | _ ->
            ())
      actuals


  let exec_instr (astate: Domain.astate) {ProcData.pdesc; ProcData.extras} _ (instr: HilInstr.t) =
    match instr with
    | Assign (((lhs_var, lhs_typ), _), HilExp.AccessPath (((rhs_var, rhs_typ) as rhs_base), _), loc) ->
        let uninit_vars = D.remove lhs_var astate.uninit_vars in
        let prepost =
          if FormalMap.is_formal rhs_base (fst extras)
             && match rhs_typ.desc with Typ.Tptr _ -> true | _ -> false
          then
            let pre' = D.add rhs_var (fst astate.prepost) in
            let post = snd astate.prepost in
            (pre', post)
          else astate.prepost
        in
        (* check on lhs_typ to avoid false positive when assigning a pointer to another *)
        if should_report_var pdesc uninit_vars rhs_var && not (is_type_pointer lhs_typ) then
          report rhs_var loc (snd extras) ;
        {astate with uninit_vars; prepost}
    | Assign (((lhs_var, _), _), _, _) ->
        let uninit_vars = D.remove lhs_var astate.uninit_vars in
        {astate with uninit_vars}
    | Call (_, call, actuals, _, loc) ->
        (* in case of intraprocedural only analysis we assume that parameters passed by reference
           to a function will be initialized inside that function *)
        let uninit_vars =
          List.fold
            ~f:(fun acc actual_exp ->
              match actual_exp with
              | HilExp.AccessPath ((actual_var, {Typ.desc= Tarray _}), _)
                when is_blacklisted_function call ->
                  D.remove actual_var acc
              | HilExp.AccessPath ((actual_var, {Typ.desc= Tptr _}), _) ->
                  D.remove actual_var acc
              | HilExp.Closure (_, apl) ->
                  (* remove the captured variables of a block/lambda *)
                  List.fold ~f:(fun acc' ((av, _), _) -> D.remove av acc') ~init:acc apl
              | _ ->
                  acc)
            ~init:astate.uninit_vars actuals
        in
        report_on_function_params pdesc uninit_vars actuals loc extras ;
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
    Analyzer.exec_cfg cfg
      (ProcData.make proc_desc tenv (formal_map, summary))
      ~initial:init ~debug:false
  in
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

