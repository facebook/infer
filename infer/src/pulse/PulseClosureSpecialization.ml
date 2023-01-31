(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes
module FuncArg = ProcnameDispatcher.Call.FuncArg
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

(* Because we use the callee's captured vars' pvars to specialize the callee in the
   caller's context, the pvars might be wrong if the caller has been specialized:
   th callee was created thinking it captured local pvars from the unspecialized caller
   so its captured vars' pvars are linked to the unspecialized caller. However, because
   the caller is now specialized, the local pvars are linked to the specialized caller.
   Hence, we want to relocalize them before trying to do anything with them in the
   caller's context
*)
let specialize_captured_vars {InterproceduralAnalysis.proc_desc} captured_vars =
  let caller_pname = Procdesc.get_proc_name proc_desc in
  List.map captured_vars ~f:(fun ({CapturedVar.pvar} as captured_var) ->
      {captured_var with pvar= Pvar.specialize_pvar pvar caller_pname} )


let get_procname_and_captured value ({InterproceduralAnalysis.analyze_dependency} as analysis_data)
    astate =
  let open IOption.Let_syntax in
  let* procname = AddressAttributes.get_closure_proc_name value astate in
  let+ pdesc, _ = analyze_dependency procname in
  let captured_vars = Procdesc.get_captured pdesc |> specialize_captured_vars analysis_data in
  (procname, captured_vars)


let get_caller_values_to_closures {InterproceduralAnalysis.proc_desc} path call_loc astate =
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let caller_attributes = Procdesc.get_attributes proc_desc in
  let caller_pname = Procdesc.get_proc_name proc_desc in
  match caller_attributes.ProcAttributes.specialized_with_closures_info with
  | None ->
      AbstractValue.Map.empty
  | Some {formals_to_closures} ->
      Pvar.Map.fold
        (fun pvar passed_closure map ->
          let pvar = Pvar.specialize_pvar pvar caller_pname in
          match
            PulseOperations.eval path Read call_loc (Exp.Lvar pvar) astate
            |> PulseOperationResult.sat_ok
          with
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
              AbstractValue.Map.add (get_deepest value) passed_closure map
          | None ->
              map )
        formals_to_closures AbstractValue.Map.empty


let captured_vars_of_captured caller_pname captured path call_loc astate =
  let astate_captured_vars =
    List.fold captured
      ~init:(Some (astate, []))
      ~f:(fun astate_captured_vars CapturedVar.{pvar; typ; capture_mode} ->
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
            match PulseOperationResult.sat_ok astate_addr_hist with
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


let captured_vars_of_captured_and_exp caller_proc_desc captured exp path call_loc astate =
  let caller_captured = Procdesc.get_captured caller_proc_desc in
  let caller_pname = Procdesc.get_proc_name caller_proc_desc in
  (* Test if the captured variables exist in the current context (they are
     either local or captured). If not, they will be accessed using Exp.Lfield.
     This is necessary in case the closure was created via a function call
     because its captured variables only exist as the fields in current context
     so using their pvars would create whole new variables different from the fields
  *)
  let easy_access =
    List.for_all captured ~f:(fun ({CapturedVar.pvar} as captured) ->
        let is_local = Stack.find_opt (Var.of_pvar pvar) astate |> Option.is_some in
        is_local || List.mem caller_captured captured ~equal:CapturedVar.equal )
  in
  let astate_captured_vars =
    if easy_access then captured_vars_of_captured caller_pname captured path call_loc astate
    else None
  in
  match astate_captured_vars with
  | Some _ ->
      astate_captured_vars
  | None ->
      Option.map exp ~f:(fun exp ->
          ( astate
          , List.mapi captured ~f:(fun id CapturedVar.{pvar; typ; capture_mode} ->
                let field_exp =
                  let fieldname = Fieldname.mk_fake_capture_field ~id typ capture_mode in
                  Exp.Lfield (exp, fieldname, typ)
                in
                (field_exp, pvar, typ, capture_mode) ) ) )


let captured_by_actuals ({InterproceduralAnalysis.proc_desc} as analysis_data) func_actuals
    caller_values_to_closures path call_loc astate =
  let caller_pname = Procdesc.get_proc_name proc_desc in
  let seen = ref AbstractValue.Set.empty in
  let rec captured_vars_of value exp astate =
    if AbstractValue.Set.mem value !seen then (* cycles not handled yet *) (astate, [])
    else (
      seen := AbstractValue.Set.add value !seen ;
      match get_procname_and_captured value analysis_data astate with
      | Some (_, captured) ->
          captured_vars_of_captured_and_exp proc_desc captured (Some exp) path call_loc astate
          |> IOption.if_none_eval ~f:(fun () -> (astate, []))
      | None ->
          let astate_captured_vars =
            match AbstractValue.Map.find_opt value caller_values_to_closures with
            | Some passed_closure ->
                let captured =
                  ClosureSpecialization.get_captured [Some passed_closure]
                  |> specialize_captured_vars analysis_data
                in
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


let actuals_of_func analysis_data func_actuals caller_values_to_closures astate =
  let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
  let actual_of value =
    let rec actual_of value seen =
      if AbstractValue.Set.mem value seen then (* cycles not handled yet *) None
      else
        let seen = AbstractValue.Set.add value seen in
        match get_procname_and_captured value analysis_data astate with
        | Some (pname, captured) ->
            let captured = specialize_captured_vars analysis_data captured in
            Some (ProcAttributes.Closure (pname, captured))
        | None -> (
            let default = AbstractValue.Map.find_opt value caller_values_to_closures in
            match BaseMemory.find_opt value post.heap with
            | None ->
                default
            | Some edges ->
                BaseMemory.Edges.fold edges ~init:default
                  ~f:(fun res (access, (accessed_value, _)) ->
                    match access with
                    | FieldAccess fieldname -> (
                      match actual_of accessed_value seen with
                      | Some passed_closure -> (
                        match res with
                        | Some (ProcAttributes.Fields map) ->
                            Some
                              (ProcAttributes.Fields
                                 (Fieldname.Map.add fieldname passed_closure map) )
                        | _ ->
                            Some
                              (ProcAttributes.Fields
                                 (Fieldname.Map.singleton fieldname passed_closure) ) )
                      | None ->
                          res )
                    | Dereference when Option.is_none res ->
                        actual_of accessed_value seen
                    | _ ->
                        res ) )
    in
    actual_of value AbstractValue.Set.empty
  in
  List.map func_actuals ~f:(fun {FuncArg.arg_payload= value, _} -> actual_of value)


let deep_formals_to_closures_of_captured_vars ({InterproceduralAnalysis.proc_desc} as analysis_data)
    captured_vars caller_values_to_closures path call_loc astate =
  (* Notation: using dftb as an alias for deep_formals_to_closures in the rest of this function *)
  let rec dftb_of_captured_var (exp, pvar, _, _) map astate seen =
    if Pvar.Map.mem pvar map then Sat (Ok (astate, map))
    else
      let++ astate', (value, _) = PulseOperations.eval path Read call_loc exp astate in
      match dftb_and_passed_closure_of_value value exp map astate' seen with
      | _, _, None ->
          (astate, map)
      | astate, map, Some passed_closure ->
          let map = Pvar.Map.add pvar passed_closure map in
          (astate, map)
  and dftb_and_passed_closure_of_closure (pname, captured) ?exp map astate seen =
    let astate_captured_vars =
      captured_vars_of_captured_and_exp proc_desc captured exp path call_loc astate
    in
    match astate_captured_vars with
    | None ->
        (astate, map, None)
    | Some (astate, captured_vars) -> (
        (* complete the map with the new captured vars info *)
        let astate_map =
          PulseOperationResult.list_fold captured_vars ~init:(astate, map)
            ~f:(fun (astate, map) captured_var ->
              dftb_of_captured_var captured_var map astate seen )
        in
        match PulseOperationResult.sat_ok astate_map with
        | Some (astate, map) ->
            (astate, map, Some (ProcAttributes.Closure (pname, captured_vars)))
        | None ->
            (astate, map, Some (ProcAttributes.Closure (pname, captured_vars))) )
  and dftb_and_passed_closure_of_value value exp map astate seen =
    if AbstractValue.Set.mem value seen then (* cycles not handled yet *) (astate, map, None)
    else
      let seen = AbstractValue.Set.add value seen in
      match get_procname_and_captured value analysis_data astate with
      | Some (pname, captured) ->
          dftb_and_passed_closure_of_closure (pname, captured) ~exp map astate seen
      | None -> (
          (* If we already have a passed_closure associated with the current value, we want
             to keep it and build the new passed_closure over it because the information it
             holds may not exist in memory. E.g. if a caller's captured var is a closure and
             is just passing through down to the callee, then all the relevant information
             about this closure's captured variables only exist in the passed_closure *)
          let astate_map_passed_closure =
            let open IOption.Let_syntax in
            let+ passed_closure = AbstractValue.Map.find_opt value caller_values_to_closures in
            let rec get_closure passed_closure map astate =
              let open ProcAttributes in
              match passed_closure with
              | Closure (pname, captured) ->
                  let captured = specialize_captured_vars analysis_data captured in
                  dftb_and_passed_closure_of_closure (pname, captured) map astate seen
              | Fields passed_closures ->
                  let astate, map, fields =
                    Fieldname.Map.fold
                      (fun fieldname passed_closure (astate, map, fields) ->
                        match fields with
                        | None ->
                            (astate, map, None)
                        | Some fields -> (
                          match get_closure passed_closure map astate with
                          | _, _, None ->
                              (astate, map, None)
                          | astate, map, Some passed_closure ->
                              let fields = Fieldname.Map.add fieldname passed_closure fields in
                              (astate, map, Some fields) ) )
                      passed_closures
                      (astate, map, Some Fieldname.Map.empty)
                  in
                  let fields = Option.map fields ~f:(fun fields -> Fields fields) in
                  (astate, map, fields)
            in
            get_closure passed_closure map astate
          in
          let default =
            match astate_map_passed_closure with
            | Some astate_map_passed_closure ->
                astate_map_passed_closure
            | None ->
                (astate, map, None)
          in
          let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
          match BaseMemory.find_opt value post.heap with
          | None ->
              default
          | Some edges ->
              BaseMemory.Edges.fold edges ~init:default
                ~f:(fun (astate, map, res) (access, ((accessed_value, _) as addr_hist)) ->
                  match access with
                  | FieldAccess fieldname -> (
                      let id = Ident.create_fresh Ident.knormal in
                      let astate' = PulseOperations.write_id id addr_hist astate in
                      match
                        dftb_and_passed_closure_of_value accessed_value (Exp.Var id) map astate'
                          seen
                      with
                      | _, _, None ->
                          (astate, map, res)
                      | astate, map, Some passed_closure -> (
                        match res with
                        | None ->
                            ( astate
                            , map
                            , Some
                                (ProcAttributes.Fields
                                   (Fieldname.Map.singleton fieldname passed_closure) ) )
                        | Some (ProcAttributes.Fields fields) ->
                            ( astate
                            , map
                            , Some
                                (ProcAttributes.Fields
                                   (Fieldname.Map.add fieldname passed_closure fields) ) )
                        | Some _ ->
                            (astate, map, res) ) )
                  | Dereference -> (
                      let id = Ident.create_fresh Ident.knormal in
                      let astate' = PulseOperations.write_id id addr_hist astate in
                      match
                        dftb_and_passed_closure_of_value accessed_value (Exp.Var id) map astate'
                          seen
                      with
                      | _, _, None ->
                          (astate, map, res)
                      | astate_map_res ->
                          astate_map_res )
                  | _ ->
                      (astate, map, res) ) )
  in
  PulseOperationResult.list_fold captured_vars ~init:(astate, Pvar.Map.empty)
    ~f:(fun (astate, map) captured_var ->
      dftb_of_captured_var captured_var map astate AbstractValue.Set.empty )


let get_orig_captured_vars analysis_data callee_pname call_kind =
  match call_kind with
  | `Closure captured_vars ->
      (* If the current callee is specialized, we want to get rid of the variables captured
         by the specialization to only keep the ones of the unspecialized version because we
         always do a full specialization. (See ClosureSpecialization.create_specialized_procdesc)
      *)
      let captured =
        let attributes =
          Option.bind (IRAttributes.load callee_pname) ~f:(fun attributes ->
              match attributes.ProcAttributes.specialized_with_closures_info with
              | None ->
                  Some attributes
              | Some {orig_proc} ->
                  IRAttributes.load orig_proc )
        in
        match attributes with None -> [] | Some attributes -> attributes.captured
      in
      if List.is_empty captured then []
      else
        let drop_n = List.length captured_vars - List.length captured in
        if Int.equal 0 drop_n then captured_vars
        else
          let caller_pname =
            Procdesc.get_proc_name analysis_data.InterproceduralAnalysis.proc_desc
          in
          List.drop captured_vars drop_n
          |> List.map ~f:(fun (exp, pvar, typ, capture_mode) ->
                 let pvar = Pvar.specialize_pvar pvar caller_pname in
                 (exp, pvar, typ, capture_mode) )
  | `Var _ | `ResolvedProcname ->
      []


let prepend_captured_vars analysis_data ~func_captured_vars ~func_args caller_values_to_closures
    path call_loc astate orig_captured_vars =
  let astate, captured_vars_by_captured_vars =
    captured_by_actuals analysis_data func_captured_vars caller_values_to_closures path call_loc
      astate
  in
  let astate, captured_vars_by_args =
    captured_by_actuals analysis_data func_args caller_values_to_closures path call_loc astate
  in
  (* Order matters:
     the new captured are added at the start of the new procdesc's captured list
     (see ClosureSpecialization.create_specialized_procdesc):
      1. captured vars' always come first
      2. then the args'
      3. finally, the previously existing captured
  *)
  (astate, List.concat [captured_vars_by_captured_vars; captured_vars_by_args; orig_captured_vars])


let get_deep_formals_to_closures analysis_data captured_vars caller_values_to_closures path call_loc
    astate =
  let res =
    deep_formals_to_closures_of_captured_vars analysis_data captured_vars caller_values_to_closures
      path call_loc astate
  in
  PulseOperationResult.sat_ok res


let prepend_deep_captured_vars deep_formals_to_closures captured_vars =
  let deep_captured_vars =
    let rec get_captured_vars_in_passed_closure captured_vars_acc = function
      | ProcAttributes.Closure (_, captured_vars) ->
          captured_vars :: captured_vars_acc
      | ProcAttributes.Fields passed_closures ->
          Fieldname.Map.fold
            (fun _ actual captured_vars_acc ->
              get_captured_vars_in_passed_closure captured_vars_acc actual )
            passed_closures captured_vars_acc
    in
    Pvar.Map.fold
      (fun _ actual captured_vars -> get_captured_vars_in_passed_closure captured_vars actual)
      deep_formals_to_closures []
    |> List.concat
  in
  (* Order matters:
     the new captured are added at the start of the new procdesc's captured list *)
  deep_captured_vars @ captured_vars


let make_specialized_call_exp analysis_data func_args callee_pname call_kind path call_loc astate =
  let open IOption.Let_syntax in
  let orig_captured_vars = get_orig_captured_vars analysis_data callee_pname call_kind in
  let rev_func_captured_vars =
    PulseOperationResult.list_fold orig_captured_vars ~init:(astate, [])
      ~f:(fun (astate, rev_func_captured_vars) (exp, _, typ, _) ->
        let++ astate, evaled = PulseOperations.eval path Read call_loc exp astate in
        ( astate
        , ProcnameDispatcher.Call.FuncArg.{exp; arg_payload= evaled; typ} :: rev_func_captured_vars
        ) )
  in
  let* astate, rev_func_captured_vars = PulseOperationResult.sat_ok rev_func_captured_vars in
  let func_captured_vars = List.rev rev_func_captured_vars in
  let caller_values_to_closures =
    get_caller_values_to_closures analysis_data path call_loc astate
  in
  let astate, captured_vars =
    prepend_captured_vars analysis_data ~func_captured_vars ~func_args caller_values_to_closures
      path call_loc astate orig_captured_vars
  in
  let* astate, deep_formals_to_closures =
    get_deep_formals_to_closures analysis_data captured_vars caller_values_to_closures path call_loc
      astate
  in
  let extra_formals_to_closures =
    let rec convert = function
      | ProcAttributes.Closure (pname, captured_vars) ->
          ProcAttributes.Closure
            ( pname
            , List.map captured_vars ~f:(fun (_, pvar, typ, capture_mode) ->
                  CapturedVar.{pvar; typ; capture_mode} ) )
      | ProcAttributes.Fields fields ->
          ProcAttributes.Fields (Fieldname.Map.map convert fields)
    in
    Pvar.Map.map convert deep_formals_to_closures
  in
  let captured_actuals =
    actuals_of_func analysis_data func_captured_vars caller_values_to_closures astate
  in
  let arg_actuals = actuals_of_func analysis_data func_args caller_values_to_closures astate in
  let+ specialized_pname =
    ClosureSpecialization.create_specialized_procdesc callee_pname ~extra_formals_to_closures
      ~captured_actuals ~arg_actuals
  in
  let captured_vars = prepend_deep_captured_vars deep_formals_to_closures captured_vars in
  let specialized_exp = Exp.Closure {name= specialized_pname; captured_vars} in
  (specialized_pname, specialized_exp, astate)
