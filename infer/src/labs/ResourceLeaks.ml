(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

module Domain = ResourceLeakDomain

(* Boilerplate to write/read our summaries alongside the summaries of other analyzers *)
module Summary = Summary.Make(struct
    type payload = Domain.astate

    let update_payload resources_payload (summary : Specs.summary) =
      { summary with payload = { summary.payload with resources = Some resources_payload }}

    let read_payload (summary : Specs.summary) =
      summary.payload.resources
  end)

type extras = FormalMap.t

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type nonrec extras = extras

  (* Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : Domain.astate) { ProcData.pdesc; tenv; } _ (instr : HilInstr.t) =
    let is_closeable procname tenv = match procname with
      | Typ.Procname.Java java_procname ->
          let is_closable_interface typename _ = match Typ.Name.name typename with
            | "java.io.AutoCloseable" | "java.io.Closeable" -> true
            | _ -> false in
          PatternMatch.supertype_exists
            tenv
            is_closable_interface
            (Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_procname))
      | _ ->
          false in
    (* We assume all constructors of a subclass of Closeable acquire a resource *)
    let acquires_resource procname tenv =
      Typ.Procname.is_constructor procname && is_closeable procname tenv in
    (* We assume the close method of a Closeable releases all of its resources *)
    let releases_resource procname tenv = match Typ.Procname.get_method procname with
      | "close" -> is_closeable procname tenv
      | _ -> false in
    match instr with
    | Call (_return_opt, Direct callee_procname, _actuals, _, _loc) ->
        (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
        (* 1(e) *)
        let astate' =
          if acquires_resource callee_procname tenv
          then astate + 1 (* 2(a) *)
          else if releases_resource callee_procname tenv
          then astate - 1 (* 2(a) *)
          else astate in
        begin
          match Summary.read_summary pdesc callee_procname with
          | Some _summary ->
              (* Looked up the summary for callee_procname... do something with it *)
              (* 4(a) *)
              (* 5(b) *)
              astate'
          | None ->
              (* No summary for callee_procname; it's native code or missing for some reason *)
              astate'
        end
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assigment [lhs_access_path] := [rhs_exp] *)
        astate
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        failwithf "Unexpected indirect call %a" HilInstr.pp instr
end

(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer =
  AbstractInterpreter.Make
    (* Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
       ignore them *)
    (ProcCfg.Normal) (* 5(a) *)
    (LowerHil.Make(TransferFunctions))

(* Callback for invoking the checker from the outside--registered in RegisterCheckers *)
let checker { Callbacks.summary; proc_desc; tenv; } : Specs.summary =
  (* Report an error when we have acquired more resources than we have released *)
  let report leak_count (proc_data : extras ProcData.t) =
    if leak_count > 0 (* 2(a) *)
    then
      let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_data.pdesc) in
      let issue_kind = Localise.to_issue_id Localise.resource_leak in
      let message = F.asprintf "Leaked %d resource(s)" leak_count in
      let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
      Reporting.log_error summary ~loc:last_loc exn in

  (* Convert the abstract state to a summary. for now, just the identity function *)
  let convert_to_summary (post : Domain.astate) : Domain.summary =
    (* 4(a) *)
    post in
  let extras = FormalMap.make proc_desc in
  let proc_data = ProcData.make proc_desc tenv extras in
  let initial = ResourceLeakDomain.initial, IdAccessPathMapDomain.empty in
  match Analyzer.compute_post proc_data ~initial ~debug:false with
  | Some (post, _) ->
      (* 1(f) *)
      report post proc_data;
      Summary.update_summary (convert_to_summary post) summary
  | None ->
      failwithf
        "Analyzer failed to compute post for %a"
        Typ.Procname.pp (Procdesc.get_proc_name proc_data.pdesc)
