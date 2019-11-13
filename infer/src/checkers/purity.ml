(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module ModifiedVarSet = PrettyPrintable.MakePPSet (Var)
module InstrCFG = ProcCfg.NormalOneInstrPerNode

let debug fmt = L.(debug Analysis Verbose fmt)

(* A simple purity checker *)

module Payload = SummaryPayload.Make (struct
  type t = PurityDomain.summary

  let field = Payloads.Fields.purity
end)

type purity_extras =
  { inferbo_invariant_map: BufferOverrunAnalysis.invariant_map
  ; formals: Var.t list
  ; get_callee_summary: Typ.Procname.t -> PurityDomain.summary option }

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = PurityDomain

  type extras = purity_extras

  let get_alias_set inferbo_mem var =
    let default = ModifiedVarSet.empty in
    let alias_v = BufferOverrunDomain.Mem.find (AbsLoc.Loc.of_var var) inferbo_mem in
    let pow_locs = BufferOverrunDomain.Val.get_all_locs alias_v in
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
    if Var.is_global base_var then Domain.impure_global
    else
      let alias_set = lazy (get_alias_set inferbo_mem base_var) in
      get_modified_params formals ~f:(fun var ->
          Var.equal var base_var || ModifiedVarSet.mem var (Lazy.force alias_set) )
      |> Domain.impure_params


  let rec is_heap_access ae =
    match (ae : HilExp.AccessExpression.t) with
    | FieldOffset _ | ArrayOffset _ ->
        true
    | Dereference ae | AddressOf ae ->
        is_heap_access ae
    | Base _ ->
        false


  exception Modified_Global

  (* find all the variables occurring in the given modified argument exp,
     unless the variable is a global (which makes all other function
     calls impure since they may read from that modified global var ) *)
  let get_modified_vars_unless_global inferbo_mem modified_acc modified_arg_exp =
    debug "Argument %a is modified.\n" HilExp.pp modified_arg_exp ;
    let ae_list = HilExp.get_access_exprs modified_arg_exp in
    List.fold ae_list ~init:modified_acc ~f:(fun modified_acc ae ->
        let base_var, typ = HilExp.AccessExpression.get_base ae in
        let alias_set = get_alias_set inferbo_mem base_var in
        debug "Alias set for %a : %a \n\n\n" Var.pp base_var ModifiedVarSet.pp alias_set ;
        if Var.is_global base_var || ModifiedVarSet.exists Var.is_global alias_set then
          raise Modified_Global
        else
          let modified_acc' =
            if Typ.is_pointer typ then ModifiedVarSet.add base_var modified_acc else modified_acc
          in
          ModifiedVarSet.union modified_acc' alias_set )


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
    try
      let vars_of_modified_args =
        List.foldi ~init:ModifiedVarSet.empty
          ~f:(fun i acc arg_exp ->
            if Domain.ModifiedParamIndices.mem i callee_modified_params then
              get_modified_vars_unless_global inferbo_mem acc arg_exp
            else acc )
          callee_args
      in
      (* find the respective parameter of the caller, matching the modified vars *)
      let caller_modified_params =
        get_modified_params formals ~f:(fun formal_var ->
            ModifiedVarSet.mem formal_var vars_of_modified_args )
      in
      Domain.impure_params caller_modified_params
    with Modified_Global -> Domain.impure_global


  (* if the callee is impure, find the parameters that have been modified by the callee *)
  let find_modified_if_impure inferbo_mem formals args callee_summary =
    match callee_summary with
    | AbstractDomain.Types.Top ->
        Domain.impure_global
    | AbstractDomain.Types.NonTop callee_modified_params ->
        Domain.(
          debug "Callee modified params %a \n" ModifiedParamIndices.pp callee_modified_params ;
          if ModifiedParamIndices.is_empty callee_modified_params then pure
          else find_params_matching_modified_args inferbo_mem formals args callee_modified_params)


  let modified_global ae = HilExp.AccessExpression.get_base ae |> fst |> Var.is_global

  let exec_instr (astate : Domain.t)
      {tenv; ProcData.extras= {inferbo_invariant_map; formals; get_callee_summary}}
      (node : CFG.Node.t) (instr : HilInstr.t) =
    let (node_id : InstrCFG.Node.id) =
      CFG.Node.underlying_node node |> InstrCFG.last_of_underlying_node |> InstrCFG.Node.id
    in
    let inferbo_mem =
      Option.value_exn (BufferOverrunAnalysis.extract_post node_id inferbo_invariant_map)
    in
    match instr with
    | Assign (ae, _, _)
      when is_heap_access ae || (Language.curr_language_is Clang && modified_global ae) ->
        track_modified_params inferbo_mem formals ae |> Domain.join astate
    | Call (_, Direct called_pname, args, _, _) ->
        Domain.join astate
          ( match PurityModels.ProcName.dispatch tenv called_pname with
          | Some callee_summary ->
              find_modified_if_impure inferbo_mem formals args callee_summary
          | None -> (
            match get_callee_summary called_pname with
            | Some callee_summary ->
                debug "Reading from %a \n" Typ.Procname.pp called_pname ;
                find_modified_if_impure inferbo_mem formals args callee_summary
            | None ->
                if Typ.Procname.is_constructor called_pname then Domain.pure
                else Domain.impure_global ) )
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java *)
        debug "Unexpected indirect call %a" HilInstr.pp instr ;
        AbstractDomain.Types.Top
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "purity checker"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions)

let should_report pdesc =
  let proc_name = Procdesc.get_proc_name pdesc in
  (not (Typ.Procname.is_constructor proc_name))
  &&
  match proc_name with
  | Typ.Procname.Java java_pname ->
      not
        ( Typ.Procname.Java.is_class_initializer java_pname
        || Typ.Procname.Java.is_access_method java_pname )
  | Typ.Procname.ObjC_Cpp name ->
      not
        ( Typ.Procname.ObjC_Cpp.is_destructor name
        || Typ.Procname.ObjC_Cpp.is_objc_constructor name.method_name )
  | _ ->
      true


let report_errors astate summary =
  let pdesc = Summary.get_proc_desc summary in
  let proc_name = Procdesc.get_proc_name pdesc in
  match astate with
  | Some astate ->
      if should_report pdesc && PurityDomain.is_pure astate then
        let loc = Procdesc.get_loc pdesc in
        let exp_desc = F.asprintf "Side-effect free function %a" Typ.Procname.pp proc_name in
        let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
        Reporting.log_error summary ~loc ~ltr IssueType.pure_function exp_desc
  | None ->
      L.internal_error "Analyzer failed to compute purity information for %a@." Typ.Procname.pp
        proc_name


let compute_summary summary tenv get_callee_summary inferbo_invariant_map =
  let proc_name = Summary.get_proc_name summary in
  let formals =
    Procdesc.get_formals (Summary.get_proc_desc summary)
    |> List.map ~f:(fun (mname, _) -> Var.of_pvar (Pvar.mk mname proc_name))
  in
  let proc_data = ProcData.make summary tenv {inferbo_invariant_map; formals; get_callee_summary} in
  Analyzer.compute_post proc_data ~initial:PurityDomain.pure


let checker {Callbacks.exe_env; summary} : Summary.t =
  let proc_name = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let inferbo_invariant_map =
    BufferOverrunAnalysis.cached_compute_invariant_map summary tenv integer_type_widths
  in
  let get_callee_summary callee_pname = Payload.read ~caller_summary:summary ~callee_pname in
  let astate = compute_summary summary tenv get_callee_summary inferbo_invariant_map in
  report_errors astate summary ;
  match astate with
  | Some astate ->
      debug "Purity summary :%a \n" PurityDomain.pp astate ;
      Payload.update_summary astate summary
  | None ->
      summary
