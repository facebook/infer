(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(* Boilerplate to write/read our summaries alongside the summaries of other analyzers *)
module Payload = SummaryPayload.Make (struct
  type t = ResourceLeakDomain.summary

  let field = Payloads.Fields.lab_resource_leaks
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ResourceLeakDomain

  type extras = unit

  let is_closeable_typename tenv typename =
    let is_closable_interface typename _ =
      match Typ.Name.name typename with
      | "java.io.AutoCloseable" | "java.io.Closeable" ->
          true
      | _ ->
          false
    in
    PatternMatch.supertype_exists tenv is_closable_interface typename


  let is_closeable_procname tenv procname =
    match procname with
    | Typ.Procname.Java java_procname ->
        is_closeable_typename tenv
          (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_procname))
    | _ ->
        false


  let acquires_resource tenv procname =
    (* We assume all constructors of a subclass of Closeable acquire a resource *)
    Typ.Procname.is_constructor procname && is_closeable_procname tenv procname


  let releases_resource tenv procname =
    (* We assume the close method of a Closeable releases all of its resources *)
    String.equal "close" (Typ.Procname.get_method procname) && is_closeable_procname tenv procname


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ResourceLeakDomain.t) {ProcData.pdesc; tenv} _ (instr : HilInstr.t) =
    match instr with
    | Call (_return, Direct callee_procname, HilExp.AccessExpression allocated :: _, _, _loc)
      when acquires_resource tenv callee_procname ->
        ResourceLeakDomain.acquire_resource
          (HilExp.AccessExpression.to_access_path allocated)
          astate
    | Call (_, Direct callee_procname, [actual], _, _loc)
      when releases_resource tenv callee_procname -> (
      match actual with
      | HilExp.AccessExpression access_expr ->
          ResourceLeakDomain.release_resource
            (HilExp.AccessExpression.to_access_path access_expr)
            astate
      | _ ->
          astate )
    | Call (return, Direct callee_procname, actuals, _, _loc) -> (
      match Payload.read pdesc callee_procname with
      | Some summary ->
          (* interprocedural analysis produced a summary: use it *)
          ResourceLeakDomain.Summary.apply ~summary ~return ~actuals astate
      | None ->
          (* No summary for [callee_procname]; it's native code or missing for some reason *)
          astate )
    | Assign (access_expr, AccessExpression rhs_access_expr, _loc) ->
        ResourceLeakDomain.assign
          (HilExp.AccessExpression.to_access_path access_expr)
          (HilExp.AccessExpression.to_access_path rhs_access_expr)
          astate
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        astate
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | ExitScope _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "resource leaks lab"
end

module CFG = ProcCfg.Normal
(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
   ignore them *)

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak post summary formal_map (proc_data : unit ProcData.t) =
  if ResourceLeakDomain.has_leak formal_map post then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_data.pdesc) in
    let message = F.asprintf "Leaked %a resource(s)" ResourceLeakDomain.pp post in
    Reporting.log_error summary ~loc:last_loc IssueType.resource_leak message


(* Callback for invoking the checker from the outside--registered in RegisterCheckers *)
let checker {Callbacks.summary; proc_desc; tenv} : Summary.t =
  let proc_data = ProcData.make proc_desc tenv () in
  match Analyzer.compute_post proc_data ~initial:ResourceLeakDomain.initial with
  | Some post ->
      let formal_map = FormalMap.make proc_desc in
      report_if_leak post summary formal_map proc_data ;
      Payload.update_summary (ResourceLeakDomain.Summary.make formal_map post) summary
  | None ->
      L.(die InternalError)
        "Analyzer failed to compute post for %a" Typ.Procname.pp
        (Procdesc.get_proc_name proc_data.pdesc)
