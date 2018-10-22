(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

let exec_instr pdesc astate _ (instr : Sil.instr) =
  match instr with
  | Call (_, Const (Cfun callee), _, loc, _) ->
      Payload.read pdesc callee
      |> Option.fold ~init:astate ~f:(ClassLoadsDomain.integrate_summary callee loc)
  | _ ->
      astate


let report_loads proc_desc summary astate =
  let report_load ({ClassLoadsDomain.Event.loc} as event) =
    let ltr = ClassLoadsDomain.Event.make_loc_trace event in
    Reporting.log_warning summary ~loc ~ltr IssueType.class_load "Class load"
  in
  let pname = Procdesc.get_proc_name proc_desc in
  ClassLoadsDomain.get_java_class pname
  |> Option.iter ~f:(fun clazz ->
         let method_strname = Typ.Procname.get_method pname in
         let fullname = clazz ^ "." ^ method_strname in
         (* Logging.debug_dev "Pname = %s" method_strname ; *)
         if String.Set.mem Config.class_loads_roots fullname then
           ClassLoadsDomain.iter report_load astate )


let analyze_procedure {Callbacks.proc_desc; summary} =
  let init = ClassLoadsDomain.empty in
  let post = Procdesc.fold_instrs proc_desc ~init ~f:(exec_instr proc_desc) in
  report_loads proc_desc summary post ;
  Payload.update_summary post summary
