(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
module FuncArg = ProcnameDispatcher.Call.FuncArg

let get_procname_and_captured value {InterproceduralAnalysis.analyze_dependency} astate =
  let open IOption.Let_syntax in
  let* procname = AddressAttributes.get_closure_proc_name value astate in
  let+ pdesc, _ = analyze_dependency procname in
  let captured_vars = Procdesc.get_captured pdesc in
  (procname, captured_vars)


let get_caller_values_to_blocks {InterproceduralAnalysis.proc_desc} path call_loc astate =
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let caller_attributes = Procdesc.get_attributes proc_desc in
  let caller_pname = Procdesc.get_proc_name proc_desc in
  match caller_attributes.ProcAttributes.specialized_with_blocks_info with
  | None ->
      AbstractValue.Map.empty
  | Some {formals_to_blocks} ->
      Pvar.Map.fold
        (fun pvar passed_block map ->
          let pvar = Pvar.specialize_pvar pvar caller_pname in
          match PulseResult.ok (PulseOperations.eval path Read call_loc (Exp.Lvar pvar) astate) with
          | Some (_, (value, _)) ->
              let rec get_deepest value =
                match BaseMemory.find_opt value post.heap with
                | None ->
                    value
                | Some edges ->
                    BaseMemory.Edges.fold edges ~init:value
                      ~f:(fun value (access, (accessed_value, _)) ->
                        match access with Dereference -> get_deepest accessed_value | _ -> value )
              in
              AbstractValue.Map.add (get_deepest value) passed_block map
          | None ->
              map )
        formals_to_blocks AbstractValue.Map.empty


let captured_vars_of_captured caller_pname captured path call_loc astate =
  let astate_captured_vars =
    List.fold captured
      ~init:(Some (astate, []))
      ~f:(fun astate_captured_vars CapturedVar.{pvar; typ; capture_mode} ->
        let pvar = Pvar.specialize_pvar pvar caller_pname in
        match astate_captured_vars with
        | None ->
            None
        | Some (astate, captured_vars) -> (
            let astate_addr_hist =
              let is_local = Pvar.equal pvar (Pvar.mk (Pvar.get_name pvar) caller_pname) in
              match capture_mode with
              | ByReference when is_local ->
                  PulseOperations.eval path Read call_loc (Exp.Lvar pvar) astate
              | _ ->
                  PulseOperations.eval_deref path call_loc (Exp.Lvar pvar) astate
            in
            match PulseResult.ok astate_addr_hist with
            | Some (astate, addr_hist) ->
                let id = Ident.create_fresh Ident.knormal in
                let astate = PulseOperations.write_id id addr_hist astate in
                let captured_var = (Exp.Var id, pvar, typ, capture_mode) in
                Some (astate, captured_var :: captured_vars)
            | None ->
                None ) )
  in
  match astate_captured_vars with
  | None ->
      None
  | Some (astate, captured_vars) ->
      Some (astate, List.rev captured_vars)


let captured_by_actuals ({InterproceduralAnalysis.proc_desc} as analysis_data) func_actuals
    caller_values_to_blocks path call_loc astate =
  let caller_captured = Procdesc.get_captured proc_desc in
  let caller_pname = Procdesc.get_proc_name proc_desc in
  let seen = ref AbstractValue.Set.empty in
  let rec captured_vars_of value exp astate =
    if AbstractValue.Set.mem value !seen then (* cycles not handled yet *) (astate, [])
    else (
      seen := AbstractValue.Set.add value !seen ;
      match get_procname_and_captured value analysis_data astate with
      | Some (_, captured) ->
          (* Test if the captured variables exist in the current context (they are
             either local or captured). If not, they will be accessed using Exp.Lfield.
             This is necessary in case the block was created via a function call
             because its captured variables only exist as the fields in current context
             so using their pvars would create whole new variables different from the fields
          *)
          let easy_access =
            List.for_all captured ~f:(fun ({CapturedVar.pvar} as captured) ->
                let is_local =
                  let pvar = Pvar.specialize_pvar pvar caller_pname in
                  Stack.find_opt (Var.of_pvar pvar) astate |> Option.is_some
                in
                is_local || List.mem caller_captured captured ~equal:CapturedVar.equal )
          in
          let captured_vars =
            if easy_access then captured_vars_of_captured caller_pname captured path call_loc astate
            else None
          in
          IOption.if_none_eval captured_vars ~f:(fun () ->
              ( astate
              , List.mapi captured ~f:(fun id CapturedVar.{pvar; typ; capture_mode} ->
                    let field_exp =
                      let fieldname = Fieldname.mk_fake_capture_field ~id typ capture_mode in
                      Exp.Lfield (exp, fieldname, typ)
                    in
                    (field_exp, pvar, typ, capture_mode) ) ) )
      | None ->
          let astate_captured_vars =
            match AbstractValue.Map.find_opt value caller_values_to_blocks with
            | Some passed_block ->
                let captured = BlockSpecialization.get_captured [Some passed_block] in
                captured_vars_of_captured caller_pname captured path call_loc astate
            | None ->
                None
          in
          IOption.if_none_eval astate_captured_vars ~f:(fun () ->
              let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
              match BaseMemory.find_opt value post.heap with
              | None ->
                  (astate, [])
              | Some edges ->
                  let astate, captured_vars =
                    BaseMemory.Edges.fold edges ~init:(astate, [])
                      ~f:(fun (astate, res) (_, ((accessed_value, _) as addr_hist)) ->
                        let id = Ident.create_fresh Ident.knormal in
                        let astate = PulseOperations.write_id id addr_hist astate in
                        let astate, captured_vars =
                          captured_vars_of accessed_value (Exp.Var id) astate
                        in
                        (astate, captured_vars :: res) )
                  in
                  (astate, List.concat captured_vars) ) )
  in
  let astate, captured_vars =
    List.fold func_actuals ~init:(astate, [])
      ~f:(fun (astate, acc) {FuncArg.arg_payload= value, _; exp} ->
        let astate, captured_vars =
          match exp with
          | Exp.Closure {captured_vars} ->
              (astate, captured_vars)
          | _ ->
              captured_vars_of value exp astate
        in
        (astate, captured_vars :: acc) )
  in
  (astate, List.rev captured_vars |> List.concat)


let actuals_of analysis_data func_actuals caller_values_to_blocks astate =
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let actual_of value =
    let rec actual_of value seen =
      if AbstractValue.Set.mem value seen then (* cycles not handled yet *) None
      else
        let seen = AbstractValue.Set.add value seen in
        match get_procname_and_captured value analysis_data astate with
        | Some procname_and_captured ->
            Some (ProcAttributes.Block procname_and_captured)
        | None -> (
            let default = AbstractValue.Map.find_opt value caller_values_to_blocks in
            match BaseMemory.find_opt value post.heap with
            | None ->
                default
            | Some edges ->
                BaseMemory.Edges.fold edges ~init:default
                  ~f:(fun res (access, (accessed_value, _)) ->
                    match access with
                    | FieldAccess fieldname -> (
                      match actual_of accessed_value seen with
                      | Some passed_block -> (
                        match res with
                        | None ->
                            Some
                              (ProcAttributes.Fields
                                 (Fieldname.Map.singleton fieldname passed_block) )
                        | Some (ProcAttributes.Fields map) ->
                            Some
                              (ProcAttributes.Fields (Fieldname.Map.add fieldname passed_block map))
                        | Some _ ->
                            res )
                      | None ->
                          res )
                    | Dereference ->
                        actual_of accessed_value seen
                    | _ ->
                        res ) )
    in
    actual_of value AbstractValue.Set.empty
  in
  List.map func_actuals ~f:(fun {FuncArg.arg_payload= value, _} -> actual_of value)


let make_specialized_call_exp analysis_data func_args callee_pname call_kind path call_loc astate =
  let orig_captured_vars =
    match call_kind with
    | `Closure captured_vars ->
        captured_vars
    | `Var _ | `ResolvedProcname ->
        []
  in
  let rev_func_captured_vars =
    let open PulseResult.Let_syntax in
    PulseResult.list_fold orig_captured_vars ~init:(astate, [])
      ~f:(fun (astate, rev_func_captured_vars) (exp, _, typ, _) ->
        let+ astate, evaled = PulseOperations.eval path Read call_loc exp astate in
        ( astate
        , ProcnameDispatcher.Call.FuncArg.{exp; arg_payload= evaled; typ} :: rev_func_captured_vars
        ) )
  in
  let open IOption.Let_syntax in
  let* astate, rev_func_captured_vars = PulseResult.ok rev_func_captured_vars in
  let func_captured_vars = List.rev rev_func_captured_vars in
  let caller_values_to_blocks = get_caller_values_to_blocks analysis_data path call_loc astate in
  let captured_actuals =
    actuals_of analysis_data func_captured_vars caller_values_to_blocks astate
  in
  let arg_actuals = actuals_of analysis_data func_args caller_values_to_blocks astate in
  let* specialized_pname =
    BlockSpecialization.create_specialized_procdesc callee_pname ~captured_actuals ~arg_actuals
  in
  let+ orig_pdesc =
    let* specialized_pdesc = Procdesc.load specialized_pname in
    let* {ProcAttributes.orig_proc} =
      (Procdesc.get_attributes specialized_pdesc).specialized_with_blocks_info
    in
    Procdesc.load orig_proc
  in
  let astate, captured_vars =
    let astate, captured_vars_by_captured_vars =
      captured_by_actuals analysis_data func_captured_vars caller_values_to_blocks path call_loc
        astate
    in
    let astate, captured_vars_by_args =
      captured_by_actuals analysis_data func_args caller_values_to_blocks path call_loc astate
    in
    let missing_captured_vars =
      (* This happens when specializing a block. In this case, we want to keep the
         old captured_vars that matched with the default captured of the block.
         They are at the end of the given call's captured_vars *)
      let nb_missing = List.length (Procdesc.get_captured orig_pdesc) in
      List.split_n orig_captured_vars (List.length orig_captured_vars - nb_missing) |> snd
    in
    (* Order matters:
       the new captured are added at the start of the new procdesc's captured list
       (see BlockSpecialization.create_specialized_procdesc):
        1. captured vars' always come first
        2. then the args'
        3. finally, the previously existing captured
    *)
    ( astate
    , List.concat [captured_vars_by_captured_vars; captured_vars_by_args; missing_captured_vars] )
  in
  let specialized_exp = Exp.Closure {name= specialized_pname; captured_vars} in
  (specialized_pname, specialized_exp, astate)
