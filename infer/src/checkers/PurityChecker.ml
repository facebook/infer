(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let should_report proc_name =
  not
    ( Procname.is_constructor proc_name
    ||
    match proc_name with
    | Procname.Java java_pname ->
        Procname.Java.is_class_initializer java_pname || Procname.Java.is_access_method java_pname
    | Procname.ObjC_Cpp name ->
        Procname.ObjC_Cpp.is_destructor name
        || Procname.ObjC_Cpp.is_objc_constructor name.method_name
    | Procname.Hack hack_pname ->
        Procname.Hack.is_xinit hack_pname
    | _ ->
        false )


let checker {IntraproceduralAnalysis.proc_desc; err_log} astate_opt =
  let proc_name = Procdesc.get_proc_name proc_desc in
  match astate_opt with
  | Some astate ->
      if should_report proc_name && PurityDomain.is_pure astate then
        let loc = Procdesc.get_loc proc_desc in
        let exp_desc = F.asprintf "Side-effect free function %a" Procname.pp proc_name in
        let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
        Reporting.log_issue proc_desc err_log ~loc ~ltr PurityChecker IssueType.pure_function
          exp_desc
  | None ->
      L.internal_error "Analyzer failed to compute purity information for %a@." Procname.pp
        proc_name
