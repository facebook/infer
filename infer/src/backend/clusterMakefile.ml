(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format
open Utils

(** Module to create a makefile with dependencies between clusters *)

(* this relies on the assumption that a source_file
   can be converted to a string, then pname, then back *)
let source_file_from_pname pname =
  DB.source_file_from_string (Procname.to_string pname)

let source_file_to_pname fname =
  Procname.from_string_c_fun (DB.source_file_to_string fname)

let pp_prolog fmt clusters =
  F.fprintf fmt "INFERANALYZE= %s $(INFER_OPTIONS) -results_dir '%s'\n@."
    Sys.executable_name
    (Escape.escape_map (fun c -> if c = '#' then Some "\\#" else None) !Config.results_dir);
  F.fprintf fmt "OBJECTS=";
  let filter cl = match cl with
      [ce] ->
        begin
          match Cluster.get_ondemand_info ce with
          | Some source_dir ->
              let in_ondemand_config = match Ondemand.read_dirs_to_analyze () with
                | None ->
                    true
                | Some set ->
                    StringSet.mem (DB.source_dir_to_string source_dir) set in
              in_ondemand_config
          | None ->
              true
        end
    | _ ->
        true in
  IList.iteri
    (fun i cl ->
       if filter cl then F.fprintf fmt "%a " Cluster.pp_cl (i+1))
    clusters;
  F.fprintf fmt "@.@.default: test@.@.all: test@.@.";
  F.fprintf fmt "test: $(OBJECTS)@.";
  if !Config.show_progress_bar then F.fprintf fmt "\techo \"\"@."

let pp_epilog fmt () =
  F.fprintf fmt "@.clean:@.\trm -f $(OBJECTS)@."

let create_cluster_makefile_and_exit
    (clusters: Cluster.t list) (file_cg: Cg.t) (fname: string) (print_files: bool) =
  let outc = open_out fname in
  let fmt = Format.formatter_of_out_channel outc in
  let file_to_cluster = ref DB.SourceFileMap.empty in
  let cluster_nr = ref 0 in
  let tot_clusters_nr = IList.length clusters in
  let do_cluster cluster =
    incr cluster_nr;
    let dependent_clusters = ref IntSet.empty in
    let add_dependent file_as_pname =
      let source_file = source_file_from_pname file_as_pname in
      try
        let num = DB.SourceFileMap.find source_file !file_to_cluster in
        if num < !cluster_nr then
          dependent_clusters := IntSet.add num !dependent_clusters
      with Not_found ->
        F.fprintf fmt "#[%a] missing dependency to %s@."
          Cluster.pp_cl !cluster_nr
          (DB.source_file_to_string source_file) in
    let do_file ce = match Cluster.get_ondemand_info ce with
      | Some source_dir ->
          (* add comment to makefile to correlate source file and cluster number. *)
          let pname_str = match ce.Cluster.ce_active_procs with
            | [pname] -> Procname.to_string pname
            | _ -> "" in
          F.fprintf fmt "#%s %s@\n" (DB.source_dir_to_string source_dir) pname_str
      | None ->
          let source_file = ce.Cluster.ce_file in
          let children =
            try Cg.get_defined_children file_cg (source_file_to_pname source_file) with
            | Not_found -> Procname.Set.empty in
          Procname.Set.iter add_dependent children;
          file_to_cluster :=
            DB.SourceFileMap.add source_file !cluster_nr !file_to_cluster;
          () (* L.err "file %s has %d children@." file (StringSet.cardinal children) *) in
    IList.iter do_file cluster;
    Cluster.pp_cluster_dependency
      !cluster_nr tot_clusters_nr cluster print_files fmt (IntSet.elements !dependent_clusters);
    (* L.err "cluster %d has %d dependencies@."
       !cluster_nr (IntSet.cardinal !dependent_clusters) *) in
  pp_prolog fmt clusters;
  IList.iter do_cluster clusters;
  pp_epilog fmt ();
  exit 0
