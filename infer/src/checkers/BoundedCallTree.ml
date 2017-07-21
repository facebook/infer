(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** find transitive procedure calls for each procedure *)

module Domain = AbstractDomain.FiniteSet (Typ.Procname)

(* Store a single stacktree frame per method. That is, callees is
   always []. Instead, the expanded per-method summaries are directly stored
   in the output directory as JSON files and *only* for those methods that
   will be part of the final crashcontext.json. *)
module SpecSummary = Summary.Make (struct
  type payload = Stacktree_j.stacktree

  let update_payload frame (summary: Specs.summary) =
    let payload = {summary.payload with Specs.crashcontext_frame= Some frame} in
    {summary with payload}

  let read_payload (summary: Specs.summary) = summary.payload.crashcontext_frame
end)

type extras_t = {get_proc_desc: Typ.Procname.t -> Procdesc.t option; stacktraces: Stacktrace.t list}

let line_range_of_pdesc pdesc =
  let ploc = Procdesc.get_loc pdesc in
  let start_line = ploc.Location.line in
  let end_line =
    Procdesc.fold_instrs
      (fun acc _ instr ->
        let new_loc = Sil.instr_get_loc instr in
        max acc new_loc.Location.line)
      start_line pdesc
  in
  {Stacktree_j.start_line= start_line; end_line}

let stacktree_of_pdesc pdesc ?(loc= Procdesc.get_loc pdesc) ?(callees= []) location_type =
  let procname = Procdesc.get_proc_name pdesc in
  let frame_loc =
    Some
      { Stacktree_j.location_type= location_type
      ; file= SourceFile.to_string loc.Location.file
      ; line= Some loc.Location.line
      ; blame_range= [line_range_of_pdesc pdesc] }
  in
  {Stacktree_j.method_name= Typ.Procname.to_unique_id procname; location= frame_loc; callees}

let stacktree_stub_of_procname procname =
  {Stacktree_j.method_name= Typ.Procname.to_unique_id procname; location= None; callees= []}

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = extras_t

  let stacktree_of_astate pdesc astate loc location_type get_proc_desc =
    let procs = Domain.elements astate in
    let callees =
      List.map
        ~f:(fun pn ->
          match SpecSummary.read_summary pdesc pn with
          | None -> (
            match get_proc_desc pn with
            | None
             -> stacktree_stub_of_procname pn
            (* This can happen when the callee is in the same cluster/ buck
                    target, but it hasn't been checked yet. So we need both the
                    inter-target lookup (SpecSummary) and the intra-target
                    lookup (using get_proc_desc). *)
            | Some callee_pdesc
             -> stacktree_of_pdesc callee_pdesc "proc_start" )
          | Some stracktree
           -> stracktree)
        procs
    in
    stacktree_of_pdesc pdesc ~loc ~callees location_type

  let output_json_summary pdesc astate loc location_type get_proc_desc =
    let caller = Procdesc.get_proc_name pdesc in
    let stacktree = stacktree_of_astate pdesc astate loc location_type get_proc_desc in
    let dir = Filename.concat Config.results_dir "crashcontext" in
    let suffix = F.sprintf "%s_%d" location_type loc.Location.line in
    let fname = F.sprintf "%s.%s.json" (Typ.Procname.to_filename caller) suffix in
    let fpath = Filename.concat dir fname in
    Utils.create_dir dir ; Ag_util.Json.to_file Stacktree_j.write_stacktree fpath stacktree

  let exec_instr astate proc_data _ = function
    | Sil.Call (_, Const Const.Cfun pn, _, loc, _)
     -> (
        let get_proc_desc = proc_data.ProcData.extras.get_proc_desc in
        let traces = proc_data.ProcData.extras.stacktraces in
        let caller = Procdesc.get_proc_name proc_data.ProcData.pdesc in
        let matches_proc frame =
          let matches_class pname =
            match pname with
            | Typ.Procname.Java java_proc
             -> String.equal frame.Stacktrace.class_str
                  (Typ.Procname.java_get_class_name java_proc)
            | Typ.Procname.ObjC_Cpp objc_cpp_prod
             -> String.equal frame.Stacktrace.class_str
                  (Typ.Procname.objc_cpp_get_class_name objc_cpp_prod)
            | Typ.Procname.C _
             -> true (* Needed for test code. *)
            | Typ.Procname.Block _ | Typ.Procname.Linters_dummy_method
             -> failwith "Proc type not supported by crashcontext: block"
          in
          String.equal frame.Stacktrace.method_str (Typ.Procname.get_method caller)
          && matches_class caller
        in
        let all_frames = List.concat (List.map ~f:(fun trace -> trace.Stacktrace.frames) traces) in
        match List.find ~f:matches_proc all_frames with
        | Some frame
         -> let new_astate = Domain.add pn astate in
            ( if Stacktrace.frame_matches_location frame loc then
                let pdesc = proc_data.ProcData.pdesc in
                output_json_summary pdesc new_astate loc "call_site" get_proc_desc ) ;
            new_astate
        | None
         -> astate )
    | Sil.Call _
     -> (* We currently ignore calls through function pointers in C and
           other potential special kinds of procedure calls to be added later,
           e.g. Java reflection. *)
        astate
    | Sil.Load _ | Store _ | Prune _ | Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _
     -> astate
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Exceptional) (TransferFunctions)

let loaded_stacktraces =
  (* Load all stacktraces defined in either Config.stacktrace or
     Config.stacktraces_dir.  *)
  let json_files_in_dir dir =
    let stacktrace_path_regexp = Str.regexp ".*\\.json" in
    let path_matcher path = Str.string_match stacktrace_path_regexp path 0 in
    DB.paths_matching dir path_matcher
  in
  let filenames =
    match (Config.stacktrace, Config.stacktraces_dir) with
    | None, None
     -> None
    | Some fname, None
     -> Some [fname]
    | None, Some dir
     -> Some (json_files_in_dir dir)
    | Some fname, Some dir
     -> Some (fname :: json_files_in_dir dir)
  in
  match filenames with
  | None
   -> None
  | Some files
   -> Some (List.map ~f:Stacktrace.of_json_file files)

let checker {Callbacks.proc_desc; tenv; get_proc_desc; summary} : Specs.summary =
  ( match loaded_stacktraces with
  | None
   -> failwith
        "Missing command line option. Either '--stacktrace stack.json' or '--stacktrace-dir ./dir' must be used when running '-a crashcontext'. This options expects a JSON formated stack trace or a directory containing multiple such traces, respectively. See tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."
  | Some stacktraces
   -> let extras = {get_proc_desc; stacktraces} in
      ignore (Analyzer.exec_pdesc (ProcData.make proc_desc tenv extras) ~initial:Domain.empty) ) ;
  summary
