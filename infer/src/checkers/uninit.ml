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
module UninitVars = AbstractDomain.FiniteSet (AccessPath)
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

let rec is_basic_type t =
  match t.Typ.desc with
  | Tint _ | Tfloat _ | Tvoid ->
      true
  | Tptr (t', _) ->
      is_basic_type t'
  | _ ->
      false


let is_blacklisted_function pname =
  List.exists ~f:(fun fname -> Typ.Procname.equal pname fname) blacklisted_functions


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

  let should_report_var pdesc tenv uninit_vars ap =
    match (AccessPath.get_typ ap tenv, ap) with
    | Some typ, ((Var.ProgramVar pv, _), _) ->
        not (Pvar.is_frontend_tmp pv) && not (Procdesc.is_captured_var pdesc pv)
        && D.mem ap uninit_vars && is_basic_type typ
    | _, _ ->
        false


  let report_on_function_params pdesc tenv uninit_vars actuals loc extras =
    List.iter
      ~f:(fun e ->
        match e with
        | HilExp.AccessPath ((var, t), al)
          when should_report_var pdesc tenv uninit_vars ((var, t), al) && not (is_type_pointer t) ->
            report var loc (snd extras)
        | _ ->
            ())
      actuals


  let remove_fields tenv base uninit_vars =
    match base with
    | _, {Typ.desc= Tptr ({Typ.desc= Tstruct name_struct}, _)} -> (
      match Tenv.lookup tenv name_struct with
      | Some {fields} ->
          List.fold
            ~f:(fun acc (fn, _, _) -> D.remove (base, [AccessPath.FieldAccess fn]) acc)
            fields ~init:uninit_vars
      | _ ->
          uninit_vars )
    | _ ->
        uninit_vars


  let remove_array_element base uninit_vars =
    D.remove (base, [AccessPath.ArrayAccess (snd base, [])]) uninit_vars


  let exec_instr (astate: Domain.astate) {ProcData.pdesc; ProcData.extras; ProcData.tenv} _
      (instr: HilInstr.t) =
    match instr with
    | Assign
        ( (((_, lhs_typ), _) as lhs_ap)
        , HilExp.AccessPath (((rhs_var, rhs_typ) as rhs_base), al)
        , loc ) ->
        let uninit_vars = D.remove lhs_ap astate.uninit_vars in
        let prepost =
          if FormalMap.is_formal rhs_base (fst extras)
             && match rhs_typ.desc with Typ.Tptr _ -> true | _ -> false
          then
            let pre' = D.add (rhs_base, al) (fst astate.prepost) in
            let post = snd astate.prepost in
            (pre', post)
          else astate.prepost
        in
        (* check on lhs_typ to avoid false positive when assigning a pointer to another *)
        if should_report_var pdesc tenv uninit_vars (rhs_base, al) && not (is_type_pointer lhs_typ)
        then report rhs_var loc (snd extras) ;
        {astate with uninit_vars; prepost}
    | Assign (lhs, _, _) ->
        let uninit_vars = D.remove lhs astate.uninit_vars in
        {astate with uninit_vars}
    | Call (_, Direct callee_pname, _, _, _)
      when Typ.Procname.equal callee_pname BuiltinDecl.objc_cpp_throw ->
        {astate with uninit_vars= D.empty}
    | Call (_, HilInstr.Direct call, actuals, _, loc) ->
        (* in case of intraprocedural only analysis we assume that parameters passed by reference
           to a function will be initialized inside that function *)
        let uninit_vars =
          List.fold
            ~f:(fun acc actual_exp ->
              match actual_exp with
              | HilExp.AccessPath (((_, {Typ.desc= Tarray _}) as base), al)
                when is_blacklisted_function call ->
                  D.remove (base, al) acc
              | HilExp.AccessPath ap when Typ.Procname.is_constructor call ->
                  remove_fields tenv (fst ap) (D.remove ap acc)
              | HilExp.AccessPath (((_, {Typ.desc= Tptr _}) as base), al)
                when not (Typ.Procname.is_constructor call) ->
                  let acc' = D.remove (base, al) acc in
                  remove_fields tenv base acc'
              | HilExp.AccessPath (((_, {Typ.desc= Tptr (t', _)}) as base), al) ->
                  let acc' = D.remove (base, al) acc in
                  remove_array_element (fst base, t') acc'
              | HilExp.Closure (_, apl) ->
                  (* remove the captured variables of a block/lambda *)
                  List.fold ~f:(fun acc' (base, _) -> D.remove (base, []) acc') ~init:acc apl
              | _ ->
                  acc)
            ~init:astate.uninit_vars actuals
        in
        report_on_function_params pdesc tenv uninit_vars actuals loc extras ;
        {astate with uninit_vars}
    | Call _ | Assume _ ->
        astate

end

module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
module Analyzer =
  AbstractInterpreter.Make (CFG) (LowerHil.Make (TransferFunctions) (LowerHil.DefaultConfig))

let get_locals cfg tenv pdesc =
  List.fold
    ~f:(fun acc (name, t) ->
      let pvar = Pvar.mk name (Procdesc.get_proc_name pdesc) in
      let base_ap = ((Var.of_pvar pvar, t), []) in
      match t.Typ.desc with
      | Typ.Tstruct qual_name -> (
        match Tenv.lookup tenv qual_name with
        | Some {fields} ->
            let flist =
              List.fold
                ~f:(fun acc' (fn, _, _) -> (fst base_ap, [AccessPath.FieldAccess fn]) :: acc')
                ~init:acc fields
            in
            base_ap :: flist
            (* for struct we take the struct address, and the access_path
                                    to the fields one level down *)
        | _ ->
            acc )
      | Typ.Tarray (t', _, _) ->
          (fst base_ap, [AccessPath.ArrayAccess (t', [])]) :: acc
      | _ ->
          base_ap :: acc)
    ~init:[] (Procdesc.get_locals cfg)


let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
  let cfg = CFG.from_pdesc proc_desc in
  (* start with empty set of uninit local vars and  empty set of init formal params *)
  let formal_map = FormalMap.make proc_desc in
  let uninit_vars = get_locals cfg tenv proc_desc in
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
