(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ResourceLeakDomain

  type analysis_data = ResourceLeakDomain.t InterproceduralAnalysis.t

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
    | Procname.Java java_procname ->
        is_closeable_typename tenv (Procname.Java.get_class_type_name java_procname)
    | _ ->
        false


  let acquires_resource tenv procname =
    (* We assume all constructors of a subclass of Closeable acquire a resource *)
    Procname.is_constructor procname && is_closeable_procname tenv procname


  let releases_resource tenv procname =
    (* We assume the close method of a Closeable releases all of its resources *)
    String.equal "close" (Procname.get_method procname) && is_closeable_procname tenv procname


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ResourceLeakDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv; analyze_dependency; _} _ (instr : HilInstr.t) =
    match instr with
    | Call (_return_opt, Direct callee_procname, _actuals, _, _loc) -> (
        if
          (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
          acquires_resource tenv callee_procname
        then ResourceLeakDomain.acquire_resource astate
        else if releases_resource tenv callee_procname then
          ResourceLeakDomain.release_resource astate
        else
          match analyze_dependency callee_procname with
          | Some (_callee_proc_desc, callee_summary) ->
              (* interprocedural analysis produced a summary: use it *)
              ResourceLeakDomain.apply_summary ~summary:callee_summary astate
          | None ->
              (* No summary for [callee_procname]; it's native code or missing for some reason *)
              astate )
    | Assign
        (Base (ret, {Typ.desc= Typ.Tptr ({Typ.desc= Typ.Tstruct ret_typename}, _)}), _rhs_exp, _loc)
      when Var.is_return ret && is_closeable_typename tenv ret_typename ->
        (* return a resource *)
        ResourceLeakDomain.record_return_resource astate |> ResourceLeakDomain.release_resource
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        astate
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "resource leaks lab"
end

(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
    ignore them *)
module CFG = ProcCfg.Normal

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} post =
  if ResourceLeakDomain.has_leak post then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "Leaked %a resource(s)" ResourceLeakDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc ResourceLeakLabExercise
      IssueType.lab_resource_leak message


(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result = Analyzer.compute_post analysis_data ~initial:ResourceLeakDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_leak analysis_data post) ;
  result
