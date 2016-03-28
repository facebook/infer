(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Javalib_pack

module L = Logging

let arg_desc =
  let options_to_keep = ["-results_dir"; "-project_root"] in
  let desc =
    (arg_desc_filter options_to_keep base_arg_desc) @
    [
      "-classpath",
      Arg.String (fun classpath -> JConfig.classpath := Some classpath),
      None,
      "set the classpath"
      ;
      "-class_source_map",
      Arg.String (fun filename -> JConfig.class_source_map := Some filename),
      None,
      "path to class -> source map in JSON format"
      ;
      "-models",
      Arg.String (fun filename -> JClasspath.add_models filename),
      Some "paths",
      "set the path to the jar containing the models"
      ;
      "-debug",
      Arg.Unit (fun () -> JConfig.debug_mode := true),
      None,
      "write extra translation information"
      ;
      "-dependencies",
      Arg.Unit (fun _ -> JConfig.dependency_mode := true),
      None,
      "translate all the dependencies during the capture"
      ;
      "-no-static_final",
      Arg.Unit (fun () -> JTrans.no_static_final := true),
      None,
      "no special treatment for static final fields"
      ;
      "-tracing",
      Arg.Unit (fun () -> JConfig.translate_checks := true),
      None,
      "Translate JVM checks"
      ;
      "-harness",
      Arg.Unit (fun () -> JConfig.create_harness := true),
      None,
      "Create Android lifecycle harness"
      ;
      "-verbose_out",
      Arg.String (fun path -> JClasspath.set_verbose_out path),
      None,
      "Set the path to the javac verbose output"
      ;
      "-suppress_warnings_out",
      Arg.String (fun s -> Config.suppress_warnings_annotations := Some s),
      Some "Path",
      "Path to list of collected @SuppressWarnings annotations"
      ;
    ] in
  Arg.create_options_desc false "Parsing Options" desc

let usage =
  "Usage: InferJava -d compilation_dir -sources filename\n"

let print_usage_exit () =
  Arg.usage arg_desc usage;
  exit(1)

let () =
  Arg.parse "INFERJAVA_ARGS" arg_desc (fun _ -> ()) usage;
  if Config.analyze_models && !JClasspath.models_jar <> "" then
    failwith "Not expecting model file when analyzing the models";
  if not Config.analyze_models && !JClasspath.models_jar = "" then
    failwith "Java model file is required"


let init_global_state source_file =
  Config.curr_language := Config.Java;
  DB.current_source := source_file;
  DB.Results_dir.init ();
  Ident.NameGenerator.reset ();
  JContext.reset_exn_node_table ();
  let nLOC = FileLOC.file_get_loc (DB.source_file_to_string source_file) in
  Config.nLOC := nLOC


let store_icfg tenv cg cfg program =
  let f_translate_typ tenv typ_str =
    let cn = JBasics.make_cn typ_str in
    ignore (JTransType.get_class_type program tenv cn) in
  let source_dir = DB.source_dir_from_source_file !DB.current_source in
  begin
    let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
    let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
    if !JConfig.create_harness then Harness.create_harness cfg cg tenv;
    Preanal.doit ~f_translate_typ:(Some f_translate_typ) cfg cg tenv;
    Cg.store_to_file cg_file cg;
    Cfg.store_cfg_to_file cfg_file true cfg;
    if !JConfig.debug_mode then
      begin
        Config.write_dotty := true;
        Config.print_types := true;
        Dotty.print_icfg_dotty cfg [];
        Cg.save_call_graph_dotty None Specs.get_specs cg
      end
  end


(* Given a source file, its code is translated, and the call-graph, control-flow-graph and type *)
(* environment are obtained and saved. *)
let do_source_file
    never_null_matcher linereader classes program tenv
    source_basename (package_opt, source_file) =
  JUtils.log "\nfilename: %s (%s)@."
    (DB.source_file_to_string source_file) source_basename;
  let call_graph, cfg =
    JFrontend.compute_source_icfg
      never_null_matcher linereader classes program tenv
      source_basename package_opt in
  store_icfg tenv call_graph cfg program


let capture_libs never_null_matcher linereader program tenv =
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
            JFrontend.compute_class_icfg never_null_matcher linereader program tenv node in
          store_icfg tenv call_graph cfg program;
          JFrontend.cache_classname cn;
        end in
  JBasics.ClassMap.iter (capture_class tenv) (JClasspath.get_classmap program)


(* load a stored global tenv if the file is found, and create a new one otherwise *)
let load_tenv () =
  let tenv_filename = DB.global_tenv_fname () in
  let tenv =
    if DB.file_exists tenv_filename then
      begin
        match Tenv.load_from_file tenv_filename with
        | None ->
            Tenv.create ()
        | Some _ when Config.analyze_models ->
            let error_msg =
              "Unexpected tenv file "
              ^ (DB.filename_to_string tenv_filename)
              ^ " found while generating the models" in
            failwith error_msg
        | Some tenv ->
            tenv
      end
    else
      Tenv.create () in
  tenv


(* Store to a file the type environment containing all the types required to perform the analysis *)
let save_tenv tenv =
  if not Config.analyze_models then JTransType.add_models_types tenv;
  let tenv_filename = DB.global_tenv_fname () in
  (* TODO: this prevents per compilation step incremental analysis at this stage *)
  if DB.file_exists tenv_filename then DB.file_remove tenv_filename;
  JUtils.log "writing new tenv %s@." (DB.filename_to_string tenv_filename);
  Tenv.store_to_file tenv_filename tenv


(* The program is loaded and translated *)
let do_all_files classpath sources classes =
  JUtils.log "Translating %d source files (%d classes)@."
    (StringMap.cardinal sources)
    (JBasics.ClassSet.cardinal classes);
  let program = JClasspath.load_program classpath classes in
  let tenv = load_tenv () in
  let linereader = Printer.LineReader.create () in
  let skip_translation_matcher =
    Inferconfig.SkipTranslationMatcher.load_matcher (Inferconfig.inferconfig ()) in
  let never_null_matcher =
    Inferconfig.NeverReturnNull.load_matcher (Inferconfig.inferconfig ()) in
  let skip source_file =
    skip_translation_matcher source_file Procname.empty_block in
  let translate_source_file basename (package_opt, _) source_file =
    init_global_state source_file;
    if not (skip source_file) then
      do_source_file
        never_null_matcher linereader classes program tenv basename (package_opt, source_file) in
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
  if !JConfig.dependency_mode then
    capture_libs never_null_matcher linereader program tenv;
  save_tenv tenv;
  JClasspath.cleanup program;
  JUtils.log "done @."


(* loads the source files and translates them *)
let () =
  let classpath, sources, classes = JClasspath.load_sources_and_classes () in
  if StringMap.is_empty sources then
    failwith "Failed to load any Java source code"
  else
    do_all_files classpath sources classes
