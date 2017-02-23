(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format
module CLOpt = CommandLineOption

(** Module to create a makefile with dependencies between clusters *)

let cluster_should_be_analyzed cluster =
  let fname = DB.source_dir_to_string cluster in
  let in_ondemand_config =
    Option.map ~f:(fun dirs -> String.Set.mem dirs fname) Ondemand.dirs_to_analyze in
  let check_modified () =
    let modified =
      DB.file_was_updated_after_start (DB.filename_from_string fname) in
    if modified &&
       Config.developer_mode
    then L.stdout "Modified: %s@." fname;
    modified in
  begin
    match in_ondemand_config with
    | Some b -> (* ondemand config file is specified *)
        b
    | None when Config.reactive_mode  ->
        check_modified ()
    | None ->
        true
  end


let pp_prolog fmt clusters =
  let escape = Escape.escape_map (fun c -> if Char.equal c '#' then Some "\\#" else None) in
  let infer_flag_of_compilation_db = function
    | `Escaped f -> F.sprintf "--compilation-database-escaped '%s'" f
    | `Raw f -> F.sprintf "--compilation-database '%s'" f in
  let compilation_dbs_cmd =
    List.map ~f:infer_flag_of_compilation_db !Config.clang_compilation_dbs
    |> String.concat ~sep:" " |> escape in
  F.fprintf fmt "INFERANALYZE= %s --results-dir '%s' %s \n@."
    (Config.bin_dir ^/ (Config.exe_name Analyze))
    (escape Config.results_dir)
    compilation_dbs_cmd;
  F.fprintf fmt "CLUSTERS=";

  List.iteri
    ~f:(fun i cl ->
        if cluster_should_be_analyzed cl
        then F.fprintf fmt "%a " Cluster.pp_cluster_name (i+1))
    clusters;

  F.fprintf fmt "@.@.default: test@.@.all: test@.@.";
  F.fprintf fmt "test: $(CLUSTERS)@.";
  if Config.show_progress_bar then F.fprintf fmt "\t@@echo@\n@."

let pp_epilog fmt () =
  F.fprintf fmt "@.clean:@.\trm -f $(CLUSTERS)@."

let create_cluster_makefile (clusters: Cluster.t list) (fname: string) =
  let outc = open_out fname in
  let fmt = Format.formatter_of_out_channel outc in
  let do_cluster cluster_nr cluster =
    F.fprintf fmt "#%s@\n" (DB.source_dir_to_string cluster);
    Cluster.pp_cluster fmt (cluster_nr + 1, cluster) in
  pp_prolog fmt clusters;
  List.iteri ~f:do_cluster clusters;
  pp_epilog fmt () ;
  Out_channel.close outc
