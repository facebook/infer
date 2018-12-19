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
module InstrCFG = ProcCfg.NormalOneInstrPerNode

let debug fmt = L.(debug Analysis Verbose fmt)

(* A simple purity checker *)

module Payload = SummaryPayload.Make (struct
  type t = PurityDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with purity= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.purity
end)

type purity_extras =
  {inferbo_invariant_map: BufferOverrunChecker.invariant_map; formals: Var.t list}

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = PurityDomain

  type extras = purity_extras

  let get_alias_set inferbo_mem var =
    let default = ModifiedVarSet.empty in
    let alias_v = BufferOverrunDomain.Mem.find (AbsLoc.Loc.of_var var) inferbo_mem in
    let pow_locs = BufferOverrunDomain.Val.get_pow_loc alias_v in
    AbsLoc.PowLoc.fold
      (fun loc modified_acc ->
        AbsLoc.Loc.get_path loc
        |> Option.value_map ~default:modified_acc ~f:(fun path ->
               Symb.SymbolPath.get_pvar path
               |> Option.value_map ~default:modified_acc ~f:(fun pvar ->
                      debug "Add alias of %a -> %a " Var.pp var (Pvar.pp Pp.text) pvar ;
                      ModifiedVarSet.add (Var.of_pvar pvar) modified_acc ) ) )
      pow_locs default


  let get_modified_params formals ~f =
    List.foldi ~init:Domain.ModifiedParamIndices.empty
      ~f:(fun i modified_acc var ->
        if f var then Domain.ModifiedParamIndices.add i modified_acc else modified_acc )
      formals


  (* given a heap access to ae, find which parameter indices of pdesc
     it modifies *)
  let track_modified_params inferbo_mem formals ae =
    let base_var, _ = HilExp.AccessExpression.get_base ae in
    (* treat writes to global (static) variables separately since they
       are not considered to be explicit parameters. *)
    let modified_params =
      if Var.is_global base_var then Domain.ModifiedParamIndices.singleton Domain.global
      else
        get_modified_params formals ~f:(fun var ->
            Var.equal var base_var || ModifiedVarSet.mem var (get_alias_set inferbo_mem base_var)
        )
    in
    Domain.impure modified_params


  let rec is_heap_access ae =
    match (ae : HilExp.AccessExpression.t) with
    | FieldOffset _ | ArrayOffset _ ->
        true
    | Dereference ae | AddressOf ae ->
        is_heap_access ae
    | Base _ ->
        false


  (* given the modified parameters and the args of the callee, find
     parameter indices of the current procedure that match, i.e have
     been modified by the callee. Note that index counting starts from
     0, reserved for the implicit parameter (formal) this .

     E.g. : for the below call to 'impure_fun' in 'foo', we return 2
     (i.e. index of a wrt. foo's formals).

     void foo (int x, Object a, Object b){
        for (...){
           impure_fun(b, 10, a); // modifies only 3rd argument, i.e. a
        }
     }
  *)
  let find_params_matching_modified_args inferbo_mem formals callee_args callee_modified_params =
    let vars_of_modified_args =
      List.foldi ~init:ModifiedVarSet.empty
        ~f:(fun i modified_acc arg_exp ->
          if Domain.ModifiedParamIndices.mem i callee_modified_params then (
            debug "Argument %a is modified.\n" HilExp.pp arg_exp ;
            HilExp.get_access_exprs arg_exp
            |> List.fold ~init:modified_acc ~f:(fun modified_acc ae ->
                   let base_var, typ = HilExp.AccessExpression.get_base ae in
                   let modified_acc' =
                     if Typ.is_pointer typ then ModifiedVarSet.add base_var modified_acc
                     else modified_acc
                   in
                   get_alias_set inferbo_mem base_var |> ModifiedVarSet.union modified_acc' ) )
          else modified_acc )
        callee_args
    in
    (* find the respective parameter of the caller, matching the modified vars *)
    let caller_modified_params =
      get_modified_params formals ~f:(fun formal_var ->
          ModifiedVarSet.mem formal_var vars_of_modified_args )
    in
    (* if callee modified global, caller also indirectly does so*)
    if Domain.contains_global callee_modified_params then
      Domain.ModifiedParamIndices.add Domain.global caller_modified_params
    else caller_modified_params


  (* if the callee is impure, find the parameters that have been modified by the callee *)
  let find_modified_if_impure inferbo_mem formals args callee_summary =
    match Domain.get_modified_params callee_summary with
    | Some callee_modified_params ->
        debug "Callee modified params %a \n" Domain.ModifiedParamIndices.pp callee_modified_params ;
        Domain.impure
          (find_params_matching_modified_args inferbo_mem formals args callee_modified_params)
    | None ->
        Domain.pure


  let exec_instr (astate : Domain.t)
      {ProcData.pdesc; tenv; ProcData.extras= {inferbo_invariant_map; formals}} (node : CFG.Node.t)
      (instr : HilInstr.t) =
    let (node_id : InstrCFG.Node.id) =
      CFG.Node.underlying_node node |> InstrCFG.last_of_underlying_node |> InstrCFG.Node.id
    in
    let inferbo_mem =
      Option.value_exn (BufferOverrunChecker.extract_post node_id inferbo_invariant_map)
    in
    match instr with
    | Assign (ae, _, _) when is_heap_access ae ->
        track_modified_params inferbo_mem formals ae |> Domain.join astate
    | Call (_, Direct called_pname, args, _, _) ->
        let matching_modified =
          find_params_matching_modified_args inferbo_mem formals args
            (Domain.all_params_modified args)
        in
        Domain.join astate
          ( match InvariantModels.Call.dispatch tenv called_pname [] with
          | Some inv ->
              Domain.with_purity (InvariantModels.is_invariant inv) matching_modified
          | None ->
              Payload.read pdesc called_pname
              |> Option.value_map ~default:(Domain.impure matching_modified) ~f:(fun summary ->
                     debug "Reading from %a \n" Typ.Procname.pp called_pname ;
                     find_modified_if_impure inferbo_mem formals args summary ) )
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "purity checker"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions)

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


let checker ({Callbacks.tenv; summary; proc_desc} as callback_args) : Summary.t =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let inferbo_invariant_map, _ =
    BufferOverrunChecker.compute_invariant_map_and_check callback_args
  in
  let formals =
    Procdesc.get_formals proc_desc
    |> List.map ~f:(fun (mname, _) -> Var.of_pvar (Pvar.mk mname proc_name))
  in
  let proc_data = ProcData.make proc_desc tenv {inferbo_invariant_map; formals} in
  let report_pure () =
    let loc = Procdesc.get_loc proc_desc in
    let exp_desc = F.asprintf "Side-effect free function %a" Typ.Procname.pp proc_name in
    let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
    Reporting.log_error summary ~loc ~ltr IssueType.pure_function exp_desc
  in
  match Analyzer.compute_post proc_data ~initial:PurityDomain.pure with
  | Some astate ->
      let astate' =
        PurityDomain.get_modified_params astate
        |> Option.value_map ~default:astate ~f:(fun modified_params ->
               debug "Modified parameter indices of %a: %a \n" Typ.Procname.pp proc_name
                 PurityDomain.ModifiedParamIndices.pp modified_params ;
               if PurityDomain.ModifiedParamIndices.is_empty modified_params then PurityDomain.pure
               else astate )
      in
      if should_report proc_desc && PurityDomain.is_pure astate' then report_pure () ;
      Payload.update_summary astate' summary
  | None ->
      L.internal_error "Analyzer failed to compute purity information for %a@." Typ.Procname.pp
        proc_name ;
      summary
