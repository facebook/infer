(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module ModifiedVarSet = Caml.Set.Make (Var)

let debug fmt = L.(debug Analysis Verbose fmt)

(* A simple purity checker *)

module Payload = SummaryPayload.Make (struct
  type t = PurityDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with purity= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.purity
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PurityDomain

  type extras = Var.t list

  let get_modified_params formals ~f =
    List.foldi ~init:Domain.ModifiedParamIndices.empty
      ~f:(fun i modified_acc var ->
        if f var then Domain.ModifiedParamIndices.add i modified_acc else modified_acc )
      formals


  (* given a heap access to ae, find which parameter indices of pdesc
     it modifies *)
  let track_modified_params formals ae =
    let base_var, _ = AccessExpression.get_base ae in
    let modified_params = get_modified_params formals ~f:(Var.equal base_var) in
    Domain.impure modified_params


  let rec is_heap_access ae =
    match (ae : AccessExpression.t) with
    | FieldOffset _ | ArrayOffset _ ->
        true
    | Dereference ae | AddressOf ae ->
        is_heap_access ae
    | Base _ ->
        false


  (* given the purity of the callee and the respective args, find
     parameters of the current pdesc that match, i.e have been
     modified by the callee.

     E.g. : for the below call to 'impure_fun' in 'foo', we return 2
     (i.e. index of a).

     void foo (int x, Object a, Object b){
        for (...){
           impure_fun(b, 10, a); // modifies only 4th argument, i.e. a
        }
     }
  *)
  let find_params_matching_modified_args formals args callee_summary =
    match Domain.get_modified_params callee_summary with
    | Some callee_modified_params ->
        let vars_of_modified_args =
          List.foldi ~init:ModifiedVarSet.empty
            ~f:(fun i modified_acc arg_exp ->
              if Domain.ModifiedParamIndices.mem i callee_modified_params then (
                debug "Argument %a is modified.\n" HilExp.pp arg_exp ;
                HilExp.get_access_exprs arg_exp
                |> List.fold ~init:modified_acc ~f:(fun modified_acc ae ->
                       ModifiedVarSet.add (AccessExpression.get_base ae |> fst) modified_acc ) )
              else modified_acc )
            args
        in
        Domain.impure
          ((* find the respective parameter of pdesc, matching the modified vars *)
           get_modified_params formals ~f:(fun formal_var ->
               ModifiedVarSet.mem formal_var vars_of_modified_args ))
    | None ->
        Domain.pure


  let exec_instr (astate : Domain.astate) {ProcData.pdesc; tenv; ProcData.extras= formals} _
      (instr : HilInstr.t) =
    match instr with
    | Assign (ae, _, _) when is_heap_access ae ->
        track_modified_params formals ae |> Domain.join astate
    | Call (_, Direct called_pname, args, _, _) ->
        let all_params_modified = Domain.all_params_modified args in
        Domain.join astate
          ( match InvariantModels.Call.dispatch tenv called_pname [] with
          | Some inv ->
              Domain.with_purity (InvariantModels.is_invariant inv) all_params_modified
          | None ->
              Payload.read pdesc called_pname
              |> Option.value_map ~default:(Domain.impure all_params_modified) ~f:(fun summary ->
                     debug "Reading from %a \n" Typ.Procname.pp called_pname ;
                     find_params_matching_modified_args formals args summary ) )
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "purity checker"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Normal) (TransferFunctions)

let should_report pdesc =
  (not Config.loop_hoisting)
  &&
  match Procdesc.get_proc_name pdesc with
  | Typ.Procname.Java java_pname as proc_name ->
      (not (Typ.Procname.is_constructor proc_name))
      && (not (Typ.Procname.Java.is_class_initializer java_pname))
      && not (Typ.Procname.Java.is_access_method java_pname)
  | _ ->
      L.(die InternalError "Not supposed to run on non-Java code.")


let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let formals =
    Procdesc.get_formals proc_desc
    |> List.map ~f:(fun (mname, _) -> Var.of_pvar (Pvar.mk mname proc_name))
  in
  let proc_data = ProcData.make proc_desc tenv formals in
  let report_pure () =
    let loc = Procdesc.get_loc proc_desc in
    let exp_desc = F.asprintf "Side-effect free function %a" Typ.Procname.pp proc_name in
    let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
    Reporting.log_error summary ~loc ~ltr IssueType.pure_function exp_desc
  in
  match Analyzer.compute_post proc_data ~initial:PurityDomain.pure with
  | Some astate ->
      ( match PurityDomain.get_modified_params astate with
      | Some modified_params ->
          debug "Modified parameter indices of %a: %a \n" Typ.Procname.pp proc_name
            PurityDomain.ModifiedParamIndices.pp modified_params
      | None ->
          if should_report proc_desc then report_pure () ) ;
      Payload.update_summary astate summary
  | None ->
      L.internal_error "Analyzer failed to compute purity information for %a@." Typ.Procname.pp
        proc_name ;
      summary
