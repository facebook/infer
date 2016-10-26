(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack

module L = Logging

let () =
  match Config.models_file with
  | None when Config.models_mode ->
      ()
  | None ->
      failwith "Java model file is required"
  | Some _ when Config.models_mode ->
      failwith "Not expecting model file when analyzing the models";
  | Some file ->
      JClasspath.add_models file


let register_perf_stats_report source_file =
  let stats_dir = Filename.concat Config.results_dir Config.frontend_stats_dir_name in
  let abbrev_source_file = DB.source_file_encoding source_file in
  let stats_file = Config.perf_stats_prefix ^ "_" ^ abbrev_source_file ^ ".json" in
  create_dir Config.results_dir ;
  create_dir stats_dir ;
  PerfStats.register_report_at_exit (Filename.concat stats_dir stats_file)


let init_global_state source_file =
  register_perf_stats_report source_file ;
  Config.curr_language := Config.Java;
  DB.Results_dir.init source_file;
  Ident.NameGenerator.reset ();
  JContext.reset_exn_node_table ();
  let nLOC = FileLOC.file_get_loc (DB.source_file_to_string source_file) in
  Config.nLOC := nLOC


let store_icfg source_file tenv cg cfg =
  let source_dir = DB.source_dir_from_source_file source_file in
  let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
  if Config.create_harness then Harness.create_harness cfg cg tenv;
  Cg.store_to_file cg_file cg;
  Cfg.store_cfg_to_file ~source_file cfg_file cfg;
  if Config.debug_mode || Config.frontend_tests then
    begin
      Dotty.print_icfg_dotty source_file cfg;
      Cg.save_call_graph_dotty source_file Specs.get_specs cg
    end;
  (* NOTE: nothing should be written to source_dir after this *)
  DB.mark_file_updated (DB.source_dir_to_string source_dir)


(* Given a source file, its code is translated, and the call-graph, control-flow-graph and type *)
(* environment are obtained and saved. *)
let do_source_file
    linereader classes program tenv
    source_basename package_opt source_file =
  L.stdout "\nfilename: %s (%s)@."
    (DB.source_file_to_string source_file) source_basename;
  let call_graph, cfg =
    JFrontend.compute_source_icfg
      linereader classes program tenv
      source_basename package_opt source_file in
  store_icfg source_file tenv call_graph cfg


let capture_libs linereader program tenv =
  let capture_class tenv cn node =
    match node with
    | Javalib.JInterface _ -> ()
    | Javalib.JClass _ when JFrontend.is_classname_cached cn -> ()
    | Javalib.JClass _ ->
        begin
          let fake_source_file =
            JClasspath.java_source_file_from_path (JFrontend.path_of_cached_classname cn) in
          init_global_state fake_source_file;
          let call_graph, cfg =
            JFrontend.compute_class_icfg fake_source_file linereader program tenv node in
          store_icfg fake_source_file tenv call_graph cfg;
          JFrontend.cache_classname cn;
        end in
  JBasics.ClassMap.iter (capture_class tenv) (JClasspath.get_classmap program)


(* load a stored global tenv if the file is found, and create a new one otherwise *)
let load_tenv () =
  match Tenv.load_from_file DB.global_tenv_fname with
  | None ->
      Tenv.create ()
  | Some _ when Config.models_mode ->
      failwithf
        "Unexpected tenv file %s found while generating the models"
        (DB.filename_to_string DB.global_tenv_fname)
  | Some tenv ->
      tenv


(* Store to a file the type environment containing all the types required to perform the analysis *)
let save_tenv tenv =
  if not Config.models_mode then JTransType.add_models_types tenv;
  (* TODO: this prevents per compilation step incremental analysis at this stage *)
  if DB.file_exists DB.global_tenv_fname then DB.file_remove DB.global_tenv_fname;
  L.stdout "writing new tenv %s@." (DB.filename_to_string DB.global_tenv_fname);
  Tenv.store_to_file DB.global_tenv_fname tenv


(* The program is loaded and translated *)
let do_all_files classpath sources classes =
  L.stdout "Translating %d source files (%d classes)@."
    (StringMap.cardinal sources)
    (JBasics.ClassSet.cardinal classes);
  let program = JClasspath.load_program classpath classes in
  let tenv = load_tenv () in
  let linereader = Printer.LineReader.create () in
  let skip source_file =
    let is_path_matching path =
      IList.exists
        (fun pattern -> Str.string_match (Str.regexp pattern) path 0)
        Config.skip_analysis_in_path in
    is_path_matching (DB.source_file_to_rel_path source_file)
    || Inferconfig.skip_translation_matcher source_file Procname.empty_block in
  let translate_source_file basename (package_opt, _) source_file =
    init_global_state source_file;
    if not (skip source_file) then
      do_source_file linereader classes program tenv basename package_opt source_file in
  StringMap.iter
    (fun basename file_entry ->
       match file_entry with
       | JClasspath.Singleton source_file ->
           translate_source_file basename (None, source_file) source_file
       | JClasspath.Duplicate source_files ->
           IList.iter
             (fun (package, source_file) ->
                translate_source_file basename (Some package, source_file) source_file)
             source_files)
    sources;
  if Config.dependency_mode then
    capture_libs linereader program tenv;
  save_tenv tenv;
  JClasspath.cleanup program;
  L.stdout "done @."


(* loads the source files and translates them *)
let () =
  let classpath, sources, classes = JClasspath.load_sources_and_classes () in
  if StringMap.is_empty sources then
    failwith "Failed to load any Java source code"
  else
    do_all_files classpath sources classes
