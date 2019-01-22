(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

let get_java_class = function
  | Typ.Procname.Java java_pname ->
      Some (Typ.Procname.Java.get_class_name java_pname)
  | _ ->
      None


let exec_instr pdesc astate _ (instr : Sil.instr) =
  match instr with
  | Call (_, Const (Cfun callee), _, loc, _) ->
      Payload.read pdesc callee
      |> Option.fold ~init:astate ~f:(ClassLoadsDomain.integrate_summary callee loc)
  | _ ->
      astate


let report_loads proc_desc summary astate =
  let report_load ({ClassLoadsDomain.Event.loc; elem} as event) =
    let ltr = ClassLoadsDomain.Event.make_loc_trace event in
    let msg = Format.asprintf "Class %s loaded" elem in
    Reporting.log_warning summary ~loc ~ltr IssueType.class_load msg
  in
  let pname = Procdesc.get_proc_name proc_desc in
  get_java_class pname
  |> Option.iter ~f:(fun clazz ->
         let method_strname = Typ.Procname.get_method pname in
         let fullname = clazz ^ "." ^ method_strname in
         if String.Set.mem Config.class_loads_roots fullname then
           ClassLoadsDomain.iter report_load astate )


(* if [pdesc] is *not* a class initializer (to avoid infinite recursion), return the 
   class initializer of [pdesc]'s class *)
let class_initializer_of_method pdesc =
  let open Typ.Procname in
  match Procdesc.get_proc_name pdesc with
  | Java java_pname when Java.is_class_initializer java_pname ->
      None
  | Java java_pname ->
      let class_name = Java.get_class_type_name java_pname in
      Some (Java (Java.get_class_initializer class_name))
  | _ ->
      assert false


let analyze_procedure {Callbacks.proc_desc; summary} =
  let proc_name = Procdesc.get_proc_name proc_desc in
  L.debug Analysis Verbose "CL: ANALYZING %a@." Typ.Procname.pp proc_name ;
  let loc = Procdesc.get_loc proc_desc in
  (* add a load for the method's class *)
  let init =
    let class_opt = get_java_class proc_name in
    L.debug Analysis Verbose "CL: CLASS = %a@." (Pp.option F.pp_print_string) class_opt ;
    Option.fold class_opt ~init:ClassLoadsDomain.empty ~f:(ClassLoadsDomain.add_load loc)
  in
  (* add loads done by the static initialization of this method's class *)
  let after_class_init =
    class_initializer_of_method proc_desc
    |> Option.bind ~f:(Payload.read proc_desc)
    (* pretend there is a call to class initializer before the method body *)
    |> Option.fold ~init ~f:(ClassLoadsDomain.integrate_summary proc_name loc)
  in
  let post = Procdesc.fold_instrs proc_desc ~init:after_class_init ~f:(exec_instr proc_desc) in
  report_loads proc_desc summary post ;
  let result = Payload.update_summary post summary in
  L.debug Analysis Verbose "CL: FINISHED ANALYZING %a@." Typ.Procname.pp proc_name ;
  result
