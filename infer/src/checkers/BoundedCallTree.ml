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

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = Stacktrace.t

  let stacktree_of_summary caller astate loc location_type =
    let procs = Domain.elements astate in
    let method_name = Procname.to_unique_id caller in
    let file = DB.source_file_to_string loc.Location.file in
    let line = loc.Location.line in
    let location = Some { Stacktree_j.location_type ; file ; line } in
    let callees = IList.map
        (fun pn ->
           { Stacktree_j.method_name = Procname.to_unique_id pn;
             location = None;
             callees = [] } )
        procs in
    { Stacktree_j.method_name; location; callees }

  let output_summary caller astate loc loc_type =
    let stacktree = stacktree_of_summary caller astate loc loc_type in
    let dir = Filename.concat Config.results_dir "crashcontext" in
    let suffix = F.sprintf "%s_%d" loc_type loc.Location.line in
    let fname = F.sprintf "%s.%s.json"
        (Procname.to_filename caller)
        suffix in
    let fpath = Filename.concat dir fname in
    DB.create_dir dir;
    Ag_util.Json.to_file Stacktree_j.write_stacktree fpath stacktree

  let exec_instr astate proc_data _ = function
    | Sil.Call (_, Const (Const.Cfun pn), _, loc, _) ->
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
        let proc_in_trace = IList.exists
            matches_proc
            proc_data.ProcData.extras.Stacktrace.frames in
        if proc_in_trace then begin
          let frame = IList.find
              matches_proc
              proc_data.ProcData.extras.Stacktrace.frames in
          let new_astate = Domain.add pn astate in
          if Stacktrace.frame_matches_location frame loc then begin
            output_summary caller new_astate loc "call_site"
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

(** Stacktrace lookup:
    1) Check if trace_ref is already set and use that.
    2) If not, load trace from the file specified in Config.stacktrace. *)
let trace_ref = ref None

let load_trace () =
  (* Check Config.stacktrace is set and points to a file, call Stacktrace.of_json_file  *)
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

let checker { Callbacks.proc_desc; tenv; } =
  let trace = match !trace_ref with
    | None -> load_trace ()
    | Some t -> t in
  ignore(Analyzer.exec_pdesc (ProcData.make proc_desc tenv trace))
