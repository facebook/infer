(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

(** Module to create a makefile with dependencies between clusters *)

(* this relies on the assumption that a source_file
   can be converted to a string, then pname, then back *)
let source_file_from_pname pname =
  DB.source_file_from_string (Procname.to_string pname)

let source_file_to_pname fname =
  Procname.from_string_c_fun (DB.source_file_to_string fname)

let cluster_should_be_analyzed cluster =
  let fname = DB.source_dir_to_string cluster in
  let in_ondemand_config =
    match Lazy.force Ondemand.dirs_to_analyze with
    | None ->
        None
    | Some set ->
        Some (StringSet.mem fname set) in
  let check_modified () =
    let modified =
      DB.file_was_updated_after_start (DB.filename_from_string fname) in
    if modified &&
       !Config.developer_mode
    then L.stdout "Modified: %s@." fname;
    modified in
  begin
    match in_ondemand_config with
    | Some b -> (* ondemand config file is specified *)
        b
    | None when !Config.reactive_mode  ->
        check_modified ()
    | None ->
        true
  end


let pp_prolog fmt clusters =
  F.fprintf fmt "INFERANALYZE= %s $(INFER_OPTIONS) -results_dir '%s'\n@."
    Sys.executable_name
    (Escape.escape_map
       (fun c -> if c = '#' then Some "\\#" else None)
       !Config.results_dir);
  F.fprintf fmt "CLUSTERS=";

  IList.iteri
    (fun i cl ->
       if cluster_should_be_analyzed cl
       then F.fprintf fmt "%a " Cluster.pp_cluster_name (i+1))
    clusters;

  F.fprintf fmt "@.@.default: test@.@.all: test@.@.";
  F.fprintf fmt "test: $(CLUSTERS)@.";
  if !Config.show_progress_bar then F.fprintf fmt "\techo \"\"@."

let pp_epilog fmt () =
  F.fprintf fmt "@.clean:@.\trm -f $(CLUSTERS)@."

let create_cluster_makefile (clusters: Cluster.t list) (fname: string) =
  let outc = open_out fname in
  let fmt = Format.formatter_of_out_channel outc in
  let do_cluster cluster_nr cluster =
    F.fprintf fmt "#%s@\n" (DB.source_dir_to_string cluster);
    Cluster.pp_cluster fmt (cluster_nr + 1, cluster) in
  pp_prolog fmt clusters;
  IList.iteri do_cluster clusters;
  pp_epilog fmt ()
