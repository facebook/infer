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


  let _acquires_resource tenv procname =
    (* We assume all constructors of a subclass of [Closeable] acquire a resource *)
    Procname.is_constructor procname && is_closeable_procname tenv procname


  let _releases_resource tenv procname =
    (* We assume the [close] method of a [Closeable] releases all of its resources *)
    String.equal "close" (Procname.get_method procname) && is_closeable_procname tenv procname


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ResourceLeakDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv= _; analyze_dependency= _; _} _ _
      (instr : Sil.instr) =
    match instr with
    (* function call of the form [_return = _callee_proc_name(..._actuals)] *)
    | Call (_return, Const (Cfun _callee_proc_name), _actuals, _loc, _) ->
        astate
    (* load of an address [_lhs:_lhs_typ = *_rhs] *)
    | Load {id= _lhs; e= _rhs; typ= _lhs_typ; loc= _loc} ->
        astate
    (* store at an address [*_lhs = _rhs:_rhs_typ] *)
    | Store {e1= _lhs; e2= _rhs; typ= _rhs_typ; loc= _loc} ->
        astate
    (* a conditional [assume(assume_exp)] blocks if [assume_exp] evaluates to false *)
    | Prune (_assume_exp, _loc, _, _) ->
        astate
    (* Call to a function/method not statically known, eg a function pointer. This should never
       happen in Java; fail if it does. *)
    | Call (_return, call_exp, _actuals, loc, _) ->
        L.die InternalError "Unexpected indirect call %a at %a" Exp.pp call_exp Location.pp loc
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "resource leaks lab"
end

(** 5(a) Type of CFG to analyze: [Exceptional] to follow exceptional control-flow edges, [Normal] to
    ignore them *)
module CFG = ProcCfg.Normal

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} post =
  let change_me = false in
  if change_me then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "Leaked %a resource(s)" ResourceLeakDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc ResourceLeakLabExercise
      IssueType.lab_resource_leak message


(** Main function into the checker; registered in {!RegisterCheckers} *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result = Analyzer.compute_post analysis_data ~initial:ResourceLeakDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_leak analysis_data post) ;
  result
