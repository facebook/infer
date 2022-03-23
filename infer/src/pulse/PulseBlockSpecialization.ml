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


let captured_by_args func_args analysis_data astate =
  let seen = ref AbstractValue.Set.empty in
  let rec captured_vars_of value exp astate =
    if AbstractValue.Set.mem value !seen then (* cycles not handled yet *) (astate, [])
    else (
      seen := AbstractValue.Set.add value !seen ;
      match get_procname_and_captured value analysis_data astate with
      | Some (_, captured) ->
          ( astate
          , List.mapi captured ~f:(fun id CapturedVar.{pvar; typ; capture_mode} ->
                let field_exp =
                  let fieldname = Fieldname.mk_fake_capture_field ~id typ capture_mode in
                  Exp.Lfield (exp, fieldname, typ)
                in
                (field_exp, pvar, typ, capture_mode) ) )
      | None -> (
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
    List.fold func_args ~init:(astate, [])
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


let args_of func_args analysis_data astate =
  let arg_of value =
    let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
    let rec arg_of value seen =
      if AbstractValue.Set.mem value seen then (* cycles not handled yet *) None
      else
        let seen = AbstractValue.Set.add value seen in
        match get_procname_and_captured value analysis_data astate with
        | Some procname_and_captured ->
            Some (ProcAttributes.Block procname_and_captured)
        | None -> (
          match BaseMemory.find_opt value post.heap with
          | None ->
              None
          | Some edges ->
              BaseMemory.Edges.fold edges ~init:None ~f:(fun res (access, (accessed_value, _)) ->
                  match access with
                  | FieldAccess fieldname -> (
                    match arg_of accessed_value seen with
                    | Some passed_block -> (
                      match res with
                      | None ->
                          Some
                            (ProcAttributes.Fields (Fieldname.Map.singleton fieldname passed_block))
                      | Some (ProcAttributes.Fields map) ->
                          Some (ProcAttributes.Fields (Fieldname.Map.add fieldname passed_block map))
                      | Some _ ->
                          res )
                    | None ->
                        res )
                  | Dereference ->
                      arg_of accessed_value seen
                  | _ ->
                      res ) )
    in
    arg_of value AbstractValue.Set.empty
  in
  List.map func_args ~f:(fun {FuncArg.arg_payload= value, _} -> arg_of value)


let make_specialized_call_exp func_args callee_pname analysis_data astate =
  let args = args_of func_args analysis_data astate in
  match BlockSpecialization.create_specialized_procdesc callee_pname args with
  | Some specialized_pname ->
      let astate, captured_vars = captured_by_args func_args analysis_data astate in
      let specialized_exp = Exp.Closure {name= specialized_pname; captured_vars} in
      Some (specialized_pname, specialized_exp, astate)
  | None ->
      None
