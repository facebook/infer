(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for on-demand analysis. *)

module L = Logging
module F = Format
open Utils

let trace = false

let enabled () = false

let across_files () = true

type analyze_proc = Procname.t -> unit

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

let analyze_proc_fun = ref None

let set_analyze_proc (analyze_proc : analyze_proc) =
  analyze_proc_fun := Some analyze_proc

let unset_analyze_prop () =
  analyze_proc_fun := None

let nesting = ref 0

let do_analysis (get_proc_desc : get_proc_desc) curr_pname proc_name =

  let really_do_analysis analyze_proc proc_desc =
    if trace then L.stderr "[%d] really_do_analysis %a -> %a@."
        !nesting
        Procname.pp curr_pname
        Procname.pp proc_name;

    let preprocess () =
      incr nesting;
      let attributes_opt =
        Some (Cfg.Procdesc.get_attributes proc_desc) in
      let call_graph =
        let cg = Cg.create () in
        Cg.add_node cg proc_name;
        cg in
      Specs.reset_summary call_graph proc_name attributes_opt;
      Specs.set_status proc_name Specs.ACTIVE in

    let postprocess () =
      decr nesting;
      let summary = Specs.get_summary_unsafe "ondemand" proc_name in
      let summary' =
        { summary with
          Specs.status = Specs.INACTIVE;
          timestamp = summary.Specs.timestamp + 1 } in
      Specs.add_summary proc_name summary';
      Checkers.ST.store_summary proc_name in

    try
      preprocess ();
      analyze_proc proc_name;
      postprocess ()
    with e ->
      L.stderr "ONDEMAND EXCEPTION %a %s %s@."
        Procname.pp proc_name
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      raise e in

  let currently_analyzed =
    Specs.summary_exists proc_name &&
    Specs.is_active proc_name in
  let already_analyzed = match Specs.get_summary proc_name with
    | Some summary ->
        Specs.get_timestamp summary > 0
    | None ->
        false in
  (* The procedure to be analyzed is in the same file as the current one. *)
  let same_file proc_attributes =
    match get_proc_desc curr_pname with
    | Some curr_pdesc ->
        (Cfg.Procdesc.get_loc curr_pdesc).Location.file
        =
        proc_attributes.ProcAttributes.loc.Location.file
    | None -> false in

  match !analyze_proc_fun, AttributesTable.load_attributes proc_name with
  | Some analyze_proc, Some proc_attributes
    when enabled () &&
         proc_attributes.ProcAttributes.is_defined && (* we have the implementation *)
         not currently_analyzed && (* avoid infinite loops *)
         not already_analyzed && (* avoid re-analysis of the same procedure *)
         (across_files () (* whether to push the analysis into other files *)
          || same_file proc_attributes) ->
      begin
        match get_proc_desc proc_name with
        | Some proc_desc ->
            really_do_analysis analyze_proc proc_desc
        | None -> ()
      end
  | _ ->
      () (* skipping *)


(** Mark the return type @Nullable by modifying the spec. *)
let proc_add_return_nullable curr_pname =
  match Specs.get_summary curr_pname with
  | Some summary ->
      let proc_attributes = Specs.get_attributes summary in
      let method_annotation = proc_attributes.ProcAttributes.method_annotation in
      let method_annotation' = Annotations.method_annotation_mark_return
          Annotations.Nullable method_annotation in
      let proc_attributes' =
        { proc_attributes with
          ProcAttributes.method_annotation = method_annotation' } in
      let summary' =
        { summary with
          Specs.attributes = proc_attributes' } in
      Specs.add_summary curr_pname summary'
  | None -> ()
