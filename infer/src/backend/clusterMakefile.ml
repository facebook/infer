(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Module to create a makefile with dependencies between clusters *)

let pp_prolog fmt clusters =
  let escape = Escape.escape_map (fun c -> if Char.equal c '#' then Some "\\#" else None) in
  let infer_flag_of_compilation_db = function
    | `Escaped f ->
        F.sprintf "--compilation-database-escaped '%s'" f
    | `Raw f ->
        F.sprintf "--compilation-database '%s'" f
  in
  let compilation_dbs_cmd =
    List.map ~f:infer_flag_of_compilation_db !Config.clang_compilation_dbs
    |> String.concat ~sep:" " |> escape
  in
  F.fprintf fmt "INFERANALYZE = '%s' --no-report --results-dir '%s' %s@\n@\n"
    (Config.bin_dir ^/ InferCommand.(to_exe_name Analyze))
    (escape Config.results_dir) compilation_dbs_cmd ;
  F.fprintf fmt "CLUSTERS=" ;
  List.iteri ~f:(fun i _ -> F.fprintf fmt "%a " Cluster.pp_cluster_name (i + 1)) clusters ;
  F.fprintf fmt "@\n@\ndefault: test@\n@\nall: test@\n@\n" ;
  F.fprintf fmt "test: $(CLUSTERS)@\n" ;
  if Config.show_progress_bar then F.fprintf fmt "\t%@echo@\n@."


let pp_epilog fmt () = F.fprintf fmt "@.clean:@.\trm -f $(CLUSTERS)@."

let create_cluster_makefile (clusters: Cluster.t list) (fname: string) =
  let outc = Out_channel.create fname in
  let fmt = Format.formatter_of_out_channel outc in
  let do_cluster cluster_nr cluster =
    F.fprintf fmt "#%a@\n" SourceFile.pp cluster ;
    Cluster.pp_cluster fmt (cluster_nr + 1, cluster)
  in
  pp_prolog fmt clusters ;
  List.iteri ~f:do_cluster clusters ;
  pp_epilog fmt () ;
  Out_channel.close outc
