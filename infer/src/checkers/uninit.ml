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

(* Check if an expression is in set of variables *)
let exp_in_set exp vset =
  let _, pvars = Exp.get_vars exp in
  List.exists ~f:(fun pv -> D.mem (Var.of_pvar pv) vset) pvars

let zip_actual_formal_params callee_pname actual_params =
  match Ondemand.get_proc_desc callee_pname with
  | Some pdesc
   -> let formals, _ = List.unzip (Procdesc.get_formals pdesc) in
      let actual, _ = List.unzip actual_params in
      (List.zip actual formals, actual)
  | _
   -> (None, [])

let deref_actual_params callee_pname actual_params deref_formal_params =
  match zip_actual_formal_params callee_pname actual_params with
  | None, _
   -> []
  | Some assoc_actual_formal, _
   -> List.fold
        ~f:(fun acc (a, f) ->
          let fe = Exp.Lvar (Pvar.mk f callee_pname) in
          if exp_in_set fe deref_formal_params then a :: acc else acc)
        ~init:[] assoc_actual_formal

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  (*  a state is a pair: ({set of uninitialized locals}, {set of written formal params})  *)
  (*  module Domain = AbstractDomain.Pair (UninitVars) (PrePost)*)
  module Domain = RecordDomain

  type extras = ProcData.no_extras

  let apply_fun_exp_set f exp vset =
    let _, pvars = Exp.get_vars exp in
    List.fold ~f:(fun acc_set pvar -> f (Var.of_pvar pvar) acc_set) ~init:vset pvars

  let exp_add_set exp vset = apply_fun_exp_set D.add exp vset

  let exp_remove_set exp vset = apply_fun_exp_set D.remove exp vset

  let set_difference set1 set2 =
    List.fold ~f:(fun acc_set e -> exp_remove_set e acc_set) ~init:set1 set2

  (* check if its a basic type *)
  let rec is_basic_typ t =
    match t.Typ.desc with
    | Typ.Tint _ | Typ.Tfloat _ | Typ.Tvoid
     -> true
    | Typ.Tptr (t', _)
     -> is_basic_typ t'
    | _
     -> false

  (* returns the formal parameter of a procedure that have a pointer type *)
  let ptr_formals pdesc =
    let proc_name = Procdesc.get_proc_name pdesc in
    let is_ptr (_, t) = match t.Typ.desc with Typ.Tptr _ -> true | _ -> false in
    let pfs = List.filter ~f:is_ptr (Procdesc.get_formals pdesc) in
    List.map ~f:(fun (name, _) -> Exp.Lvar (Pvar.mk name proc_name)) pfs

  (* Update the set of written formals when it finds a sequence
     n$0=*&i;
     ....
    *n$0=..;
*)
  let update_state_formal node pdesc formal_set =
    let ptr_formals = ptr_formals pdesc in
    let ids_assigned_to_formals, dereferenced_ids =
      List.fold
        ~f:(fun (acc1, acc2) (i, _) ->
          match i with
          | Sil.Load (lhs_id, rhs_exp, _, _) when List.mem ptr_formals rhs_exp ~equal:Exp.equal
           -> ((lhs_id, rhs_exp) :: acc1, acc2)
          | Sil.Store (Exp.Var lhs_id, _, _, _)
           -> (acc1, lhs_id :: acc2)
          | _
           -> (acc1, acc2))
        (CFG.instr_ids node) ~init:([], [])
    in
    List.fold
      ~f:(fun acc_set (id, formal_param) ->
        if List.mem dereferenced_ids id ~equal:Ident.equal then exp_add_set formal_param acc_set
        else acc_set)
      ids_assigned_to_formals ~init:formal_set

  let exec_instr
      ( {Domain.uninit_vars= uninit; Domain.aliased_vars= _; Domain.prepost= pre, wformals} as
      astate ) {ProcData.pdesc} node = function
    | Sil.Load (_, rhs_exp, _, _)
     -> let post = update_state_formal node pdesc wformals in
        let set_ptr_formals = ptr_formals pdesc in
        let pre' =
          if List.mem set_ptr_formals ~equal:Exp.equal rhs_exp && not (exp_in_set rhs_exp post)
          then exp_add_set rhs_exp pre
          else pre
        in
        {astate with prepost= (pre', post)}
    | Sil.Store (lhs_exp, _, rhs_exp, _)
     -> if exp_in_set rhs_exp uninit then astate
        else {astate with Domain.uninit_vars= exp_remove_set lhs_exp uninit}
    | Sil.Call (ret_id, Exp.Const Const.Cfun callee_pname, actual_params, _, _)
      when not intraprocedural_only
     -> (
        let astate' =
          Option.value_map
            ~f:(fun (ret_id, _) ->
              {astate with Domain.uninit_vars= exp_remove_set (Exp.Var ret_id) uninit})
            ~default:astate ret_id
        in
        match
          ( zip_actual_formal_params callee_pname actual_params
          , Summary.read_summary pdesc callee_pname )
        with
        | (Some assoc_actual_formal, actual), Some {pre= _; post= wformals_callee}
         -> let initialized_actual_params =
              List.filter
                ~f:(fun a ->
                  match List.Assoc.find assoc_actual_formal ~equal:Exp.equal a with
                  | Some fp
                   -> D.mem (Var.of_pvar (Pvar.mk fp callee_pname)) wformals_callee
                  | None
                   -> false)
                actual
            in
            let uninit' = set_difference uninit initialized_actual_params in
            {astate with Domain.uninit_vars= uninit'; Domain.prepost= astate'.prepost}
        | _, _
         -> L.(debug Linters Medium)
              "Warning: unable to deal with callee '%s' summary@\n"
              (Typ.Procname.to_string callee_pname) ;
            astate' )
    | Sil.Call (_, _, actual_params, _, _)
     -> (* in case of intraprocedural only analysis we assume that parameters
           passed by reference to a function will be initialized inside that function *)
        let actual_passed_by_ref =
          List.fold
            ~f:(fun acc (e, t) -> match t.Typ.desc with Typ.Tptr _ -> e :: acc | _ -> acc)
            ~init:[] actual_params
        in
        let uninit' = set_difference uninit actual_passed_by_ref in
        {astate with Domain.uninit_vars= uninit'}
    | Sil.Declare_locals (pvar_list, _)
     -> List.fold
          ~f:(fun astate_acc (pvar, t) ->
            if is_basic_typ t then
              { astate_acc with
                Domain.uninit_vars= D.add (Var.of_pvar pvar) astate_acc.Domain.uninit_vars }
            else astate_acc)
          ~init:astate pvar_list
    | Sil.Remove_temps _ | Sil.Abstract _ | Sil.Nullify _ | Sil.Prune _
     -> astate
end

module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
module Analyzer = AbstractInterpreter.Make (CFG) (TransferFunctions)

let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
  let cfg = CFG.from_pdesc proc_desc in
  (* start with empty set of uninit local vars and  empty set of init formal params *)
  let init =
    { RecordDomain.uninit_vars= UninitVars.empty
    ; RecordDomain.aliased_vars= AliasedVars.empty
    ; RecordDomain.prepost= (D.empty, D.empty) }
  in
  let invariant_map =
    Analyzer.exec_cfg cfg (ProcData.make_default proc_desc tenv) ~initial:init ~debug:true
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
      when exp_in_set (Exp.Lvar pv) uninit_vars
     -> let message =
          F.asprintf "The value read from %a was never initialized" Exp.pp (Exp.Lvar pv)
        in
        report message loc
    | Sil.Call (_, Exp.Const Const.Cfun callee_pname, actual_params, loc, _)
      when not intraprocedural_only -> (
      match Summary.read_summary proc_desc callee_pname with
      | Some {pre= deref_formal_params; post= _}
       -> let deref_actual_params =
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
      | _
       -> () )
    | _
     -> ()
  in
  let report_on_node node =
    List.iter (CFG.instr_ids node) ~f:(fun (instr, node_id_opt) ->
        match node_id_opt with
        | Some node_id -> (
          match Analyzer.extract_pre node_id invariant_map with
          | Some
              { RecordDomain.uninit_vars= uninitialized_vars
              ; RecordDomain.aliased_vars= _
              ; RecordDomain.prepost= _ }
           -> report_uninit_value uninitialized_vars instr
          | None
           -> () )
        | None
         -> () )
  in
  List.iter (CFG.nodes cfg) ~f:report_on_node ;
  match Analyzer.extract_post (CFG.id (CFG.exit_node cfg)) invariant_map with
  | Some
      {RecordDomain.uninit_vars= _; RecordDomain.aliased_vars= _; RecordDomain.prepost= pre, post}
   -> Summary.update_summary {pre; post} summary
  | None
   -> L.(die InternalError)
        "Analyzer failed to compute post for %a" Typ.Procname.pp (Procdesc.get_proc_name proc_desc)
