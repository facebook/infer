(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface
module FuncArg = ProcnameDispatcher.Call.FuncArg

let get_procname_and_captured value {InterproceduralAnalysis.analyze_dependency} astate =
  let open IOption.Let_syntax in
  let* procname = AddressAttributes.get_closure_proc_name value astate in
  let+ pdesc, _ = analyze_dependency procname in
  let captured_vars = Procdesc.get_captured pdesc in
  (procname, captured_vars)


let captured_by_args func_args analysis_data astate =
  List.concat_map func_args ~f:(fun {FuncArg.arg_payload= value, _; exp} ->
      match get_procname_and_captured value analysis_data astate with
      | Some (_, captured) ->
          (* another solution could be to look into the access and
             get all the fields after checking that the value is a block *)
          List.mapi captured ~f:(fun id CapturedVar.{pvar; typ; capture_mode} ->
              let field_exp =
                let fieldname = Fieldname.mk_fake_capture_field ~id typ capture_mode in
                Exp.Lfield (exp, fieldname, typ)
              in
              (field_exp, pvar, typ, capture_mode) )
      | None ->
          [] )


let make_specialized_call_exp func_args callee_pname analysis_data astate =
  let args =
    List.map func_args ~f:(fun {FuncArg.arg_payload= value, _} ->
        match get_procname_and_captured value analysis_data astate with
        | Some procname_and_captured ->
            BlockSpecialization.Block procname_and_captured
        | None ->
            BlockSpecialization.Var )
  in
  match BlockSpecialization.create_specialized_procdesc callee_pname args with
  | Some specialized_pname ->
      let specialized_exp =
        Exp.Closure
          {name= specialized_pname; captured_vars= captured_by_args func_args analysis_data astate}
      in
      Some (specialized_pname, specialized_exp)
  | None ->
      None
