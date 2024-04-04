(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
module L = Logging

let init_global_state source_file =
  Language.curr_language := Language.Java ;
  DB.Results_dir.init source_file ;
  Ident.NameGenerator.reset () ;
  JContext.reset_exn_node_table ()


let store_icfg source_file cfg =
  SourceFiles.add source_file cfg Tenv.Global None ;
  if Config.debug_mode || Config.frontend_tests then DotCfg.emit_frontend_cfg source_file cfg ;
  ()


(* Given a source file, its code is translated, and the call-graph, control-flow-graph and type *)
(* environment are obtained and saved. *)
let do_source_file program tenv source_basename package_opt source_file =
  L.(debug Capture Medium) "@\nfilename: %a (%s)@." SourceFile.pp source_file source_basename ;
  init_global_state source_file ;
  let cfg = JFrontend.compute_source_icfg program tenv source_basename package_opt source_file in
  ( if Config.dump_textual then
      let filename = Filename.chop_extension (SourceFile.to_abs_path source_file) ^ ".sil" in
      TextualSil.from_java ~filename tenv cfg ) ;
  store_icfg source_file cfg


let do_class tenv program cn node =
  let class_source_file = SourceFile.from_abs_path (JFrontend.classname_path cn) in
  L.(debug Capture Medium)
    "@\nfilename (class): %a (%s)@." SourceFile.pp class_source_file (JBasics.cn_name cn) ;
  init_global_state class_source_file ;
  let cfg = JFrontend.compute_class_icfg class_source_file program tenv node in
  store_icfg class_source_file cfg ;
  JFrontend.cache_classname cn


let capture_libs program tenv =
  let filter_and_capture_class_only tenv cn node =
    match node with
    | Javalib.JInterface _ ->
        ()
    | Javalib.JClass _ when JFrontend.is_classname_cached cn ->
        ()
    | Javalib.JClass _ ->
        do_class tenv program cn node
  in
  JProgramDesc.Classmap.iter
    (filter_and_capture_class_only tenv)
    (JProgramDesc.get_classmap program)


(* load a stored global tenv if the file is found, and create a new one otherwise *)
let load_tenv () =
  match Tenv.load_global () with
  | None ->
      Tenv.create ()
  | Some _ when Config.biabduction_models_mode ->
      L.die InternalError "Unexpected global tenv file found in '%s' while generating the models"
        (ResultsDir.get_path GlobalTypeEnvironment)
  | Some tenv ->
      tenv


(** Store to a file the type environment containing all the types required to perform the analysis *)
let save_tenv tenv =
  L.(debug Capture Medium) "writing new global tenv@." ;
  Tenv.store_global ~normalize:true tenv


let store_callee_attributes tenv program =
  let f proc_name cn ms =
    Option.iter
      ~f:(Attributes.store ~proc_desc:None ~analysis:false)
      (JTrans.create_callee_attributes tenv program cn ms proc_name)
  in
  JProgramDesc.iter_missing_callees program ~f


(* The program is loaded and translated *)
let do_all_files classpath program =
  let {JClasspath.sources; JClasspath.classes; _} = classpath in
  let tenv = load_tenv () in
  let skip source_file =
    let is_path_matching path =
      Option.exists ~f:(fun re -> Str.string_match re path 0) Config.skip_analysis_in_path
    in
    is_path_matching (SourceFile.to_rel_path source_file)
    || Inferconfig.capture_block_list_file_matcher source_file
  in
  let translate_source_file basename package_opt source_file =
    if not (skip source_file) then do_source_file program tenv basename package_opt source_file
  in
  if String.Map.is_empty sources then (
    L.(debug Capture Medium) "no source files found, capturing class files directly@." ;
    JBasics.ClassSet.iter
      (fun cn ->
        if JFrontend.is_classname_cached cn then ()
        else
          let class_source_file = SourceFile.from_abs_path (JFrontend.classname_path cn) in
          if not (skip class_source_file) then
            let node = JProgramDesc.lookup_node cn program in
            Option.iter ~f:(do_class tenv program cn) node )
      classes )
  else
    String.Map.iteri
      ~f:(fun ~key:basename ~data:file_entry ->
        match file_entry with
        | JClasspath.Singleton source_file ->
            translate_source_file basename None source_file
        | JClasspath.Duplicate source_files ->
            List.iter
              ~f:(fun (package, source_file) ->
                translate_source_file basename (Some package) source_file )
              source_files )
      sources ;
  if Config.dependency_mode then capture_libs program tenv ;
  store_callee_attributes tenv program ;
  save_tenv tenv ;
  L.(debug Capture Quiet) "done capturing all files@."


(* loads the source files and translates them *)
let main load_sources_and_classes =
  ( match (Config.biabduction_models_mode, ISys.file_exists Config.biabduction_models_jar) with
  | true, false ->
      ()
  | false, false ->
      L.(die UserError) "Java model file is required"
  | true, true ->
      L.(die UserError) "Not expecting model file when analyzing the models"
  | false, true ->
      JModels.load_models ~jar_filename:Config.biabduction_models_jar ) ;
  JBasics.set_permissive true ;
  JClasspath.with_classpath load_sources_and_classes ~f:(fun classpath ->
      let program = JProgramDesc.load classpath in
      do_all_files classpath program )


let from_arguments ~sources path = main (JClasspath.FromArguments {path; sources})

let from_verbose_out verbose_out_file = main (JClasspath.FromVerboseOut {verbose_out_file})
