(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

(** find transitive procedure calls for each procedure *)

module ProcnameSet = PrettyPrintable.MakePPSet(struct
    type t = Procname.t
    let compare = Procname.compare
    let pp_element = Procname.pp
  end)

module Domain = AbstractDomain.FiniteSet(ProcnameSet)

(* Store a single stacktree frame per method. That is, callees is
   always []. Instead, the expanded per-method summaries are directly stored
   in the output directory as JSON files and *only* for those methods that
   will be part of the final crashcontext.json. *)
module SpecSummary = Summary.Make (struct
    type summary = Stacktree_j.stacktree option

    let update_payload frame payload =
      { payload with Specs.crashcontext_frame = frame }

    let read_from_payload payload =
      payload.Specs.crashcontext_frame
  end)

type extras_t = {
  get_proc_desc : Procname.t -> Cfg.Procdesc.t option;
  stacktrace : Stacktrace.t;
}

let line_range_of_pdesc pdesc =
  let ploc = Cfg.Procdesc.get_loc pdesc in
  let start_line = ploc.Location.line in
  let end_line = Cfg.Procdesc.fold_instrs
      (fun acc _ instr ->
         let new_loc = Sil.instr_get_loc instr in
         max acc new_loc.Location.line)
      start_line
      pdesc in
  { Stacktree_j.start_line; end_line }

let stacktree_of_pdesc
    pdesc
    ?(loc=Cfg.Procdesc.get_loc pdesc)
    ?(callees=[])
    location_type =
  let procname = Cfg.Procdesc.get_proc_name pdesc in
  let frame_loc =
    Some { Stacktree_j.location_type = location_type;
           file = DB.source_file_to_string loc.Location.file;
           line = loc.Location.line;
           blame_range = [line_range_of_pdesc pdesc] } in
  { Stacktree_j.method_name = Procname.to_unique_id procname;
    location = frame_loc;
    callees = callees }

let stacktree_stub_of_procname procname =
  { Stacktree_j.method_name = Procname.to_unique_id procname;
    location = None;
    callees = [] }

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = extras_t

  let stacktree_of_astate pdesc astate loc location_type get_proc_desc =
    let procs = Domain.elements astate in
    let callees = IList.map
        (fun pn ->
           match SpecSummary.read_summary pdesc pn with
           | None | Some None -> (match get_proc_desc pn with
               | None -> stacktree_stub_of_procname pn
               (* This can happen when the callee is in the same cluster/ buck
                  target, but it hasn't been checked yet. So we need both the
                  inter-target lookup (SpecSummary) and the intra-target
                  lookup (using get_proc_desc). *)
               | Some callee_pdesc ->
                   stacktree_of_pdesc callee_pdesc "proc_start")
           | Some (Some stracktree) -> stracktree )
        procs in
    stacktree_of_pdesc pdesc ~loc ~callees location_type

  let output_json_summary pdesc astate loc location_type get_proc_desc =
    let caller = Cfg.Procdesc.get_proc_name pdesc in
    let stacktree =
      stacktree_of_astate pdesc astate loc location_type get_proc_desc in
    let dir = Filename.concat Config.results_dir "crashcontext" in
    let suffix = F.sprintf "%s_%d" location_type loc.Location.line in
    let fname = F.sprintf "%s.%s.json"
        (Procname.to_filename caller)
        suffix in
    let fpath = Filename.concat dir fname in
    DB.create_dir dir;
    Ag_util.Json.to_file Stacktree_j.write_stacktree fpath stacktree

  let exec_instr astate proc_data _ = function
    | Sil.Call (_, Const (Const.Cfun pn), _, loc, _) ->
        let get_proc_desc = proc_data.ProcData.extras.get_proc_desc in
        let trace = proc_data.ProcData.extras.stacktrace in
        let caller = Cfg.Procdesc.get_proc_name proc_data.ProcData.pdesc in
        let matches_proc frame =
          let matches_class pname = match pname with
            | Procname.Java java_proc ->
                string_equal
                  frame.Stacktrace.class_str
                  (Procname.java_get_class_name java_proc)
            | Procname.ObjC_Cpp objc_cpp_prod ->
                string_equal
                  frame.Stacktrace.class_str
                  (Procname.objc_cpp_get_class_name objc_cpp_prod)
            | Procname.C _ -> true (* Needed for test code. *)
            | Procname.Block _ ->
                failwith "Proc type not supported by crashcontext: block" in
          frame.Stacktrace.method_str = (Procname.get_method caller) &&
          matches_class caller in
        let proc_in_trace = IList.exists matches_proc trace.Stacktrace.frames in
        if proc_in_trace then begin
          let frame = IList.find matches_proc trace.Stacktrace.frames in
          let new_astate = Domain.add pn astate in
          if Stacktrace.frame_matches_location frame loc then begin
            let pdesc = proc_data.ProcData.pdesc in
            output_json_summary pdesc new_astate loc "call_site" get_proc_desc
          end;
          new_astate
        end
        else
          astate
    | Sil.Call _ ->
        (* We currently ignore calls through function pointers in C and
           other potential special kinds of procedure calls to be added later,
           e.g. Java reflection. *)
        astate
    | Sil.Letderef _ | Set _ | Prune _ | Declare_locals _
    | Stackop _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

(* Stacktrace lookup:
   1) Check if trace_ref is already set and use that.
   2) If not, load trace from the file specified in Config.stacktrace. *)
let trace_ref = ref None

let load_trace () =
  (* Check Config.stacktrace is set and points to a file,
     call Stacktrace.of_json_file  *)
  let filename = match Config.stacktrace with
    | None -> failwith "Missing command line option: '--stacktrace stack.json' \
                        must be used when running '-a crashcontext'. This \
                        option expects a JSON formated stack trace. See \
                        tests/codetoanalyze/java/crashcontext/*.json for \
                        examples of the expected format."
    | Some fname -> fname in
  let new_trace = Stacktrace.of_json_file filename in
  trace_ref := Some new_trace;
  new_trace

let checker { Callbacks.proc_desc; tenv; get_proc_desc; } =
  let stacktrace = match !trace_ref with
    | None -> load_trace ()
    | Some t -> t in
  let extras = { get_proc_desc; stacktrace; } in
  SpecSummary.write_summary
    (Cfg.Procdesc.get_proc_name proc_desc)
    (Some (stacktree_of_pdesc proc_desc "proc_start"));
  ignore(Analyzer.exec_pdesc (ProcData.make proc_desc tenv extras))
