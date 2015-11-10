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
open Sawja_pack
open Utils

module L = Logging

let models_specs_filenames = ref StringSet.empty

let arg_classpath = ref ""

let arg_jarfile = ref ""

let set_jarfile file =
  arg_jarfile := file

let javac_verbose_out = ref ""

let set_verbose_out path =
  javac_verbose_out := path

let models_jar = ref ""


let models_tenv = ref (Sil.create_tenv ())


let load_models_tenv zip_channel =
  let models_tenv_filename_in_jar =
    let root = Filename.concat Config.default_in_zip_results_dir Config.captured_dir_name in
    Filename.concat root Config.global_tenv_filename in
  let temp_tenv_filename =
    DB.filename_from_string (Filename.temp_file "tmp_" Config.global_tenv_filename) in
  let entry = Zip.find_entry zip_channel models_tenv_filename_in_jar in
  let temp_tenv_file = DB.filename_to_string temp_tenv_filename in
  let models_tenv =
    try
      Zip.copy_entry_to_file zip_channel entry temp_tenv_file;
      match Sil.load_tenv_from_file temp_tenv_filename with
      | None -> failwith "Models tenv file could not be loaded"
      | Some tenv -> tenv
    with
    | Not_found -> failwith "Models tenv not found in jar file"
    | Sys_error msg -> failwith ("Models jar could not be opened "^msg) in
  DB.file_remove temp_tenv_filename;
  models_tenv


let collect_specs_filenames jar_filename =
  let zip_channel = Zip.open_in jar_filename in
  let collect set e =
    let filename = e.Zip.filename in
    if not (Filename.check_suffix filename ".specs") then set
    else
      let proc_filename = (Filename.chop_extension (Filename.basename filename)) in
      StringSet.add proc_filename set in
  models_specs_filenames :=
    IList.fold_left collect !models_specs_filenames (Zip.entries zip_channel);
  models_tenv := load_models_tenv zip_channel;
  Zip.close_in zip_channel


let add_models jar_filename =
  models_jar := jar_filename;
  if Sys.file_exists !models_jar then
    collect_specs_filenames jar_filename
  else
    failwith "Java model file not found"


let is_model procname =
  StringSet.mem (Procname.to_filename procname) !models_specs_filenames


let split_classpath cp = Str.split (Str.regexp JFile.sep) cp


let java_source_file_from_path path =
  if Filename.is_relative path then
    failwith "Expect absolute path for java source files"
  else
    match !Config.project_root with
    | None -> DB.abs_source_file_from_path path
    | Some project_root -> DB.rel_source_file_from_abs_path project_root path


(** Add the android.jar containing bytecode at the beginning of the class path *)
let add_android_jar paths =
  AndroidFramework.non_stub_android_jar () :: paths


let append_path classpath path =
  if Sys.file_exists path then
    let full_path = filename_to_absolute path in
    if String.length classpath = 0 then
      full_path
    else
      classpath^JFile.sep^full_path
  else
    classpath


let load_sources_and_classes () =
  let file_in = open_in !javac_verbose_out in
  let cwd = Sys.getcwd () in
  let convert_filename fname =
    if Filename.is_relative fname then
      Filename.concat cwd fname
    else
      fname in
  let rec loop paths roots sources classes =
    try
      let lexbuf = Lexing.from_string (input_line file_in) in
      match JVerboseParser.line JVerboseLexer.token lexbuf with
      | JVerbose.Source fname ->
          let source_file = java_source_file_from_path (convert_filename fname) in
          loop paths roots (StringMap.add (Filename.basename fname) source_file sources) classes
      | JVerbose.Class fname ->
          let cn, root_info = Javalib.extract_class_name_from_file fname in
          let root_dir = if root_info = "" then Filename.current_dir_name else root_info in
          let updated_roots =
            if IList.exists (fun p -> p = root_dir) roots then roots
            else root_dir:: roots in
          loop paths updated_roots sources (JBasics.ClassSet.add cn classes)
      | JVerbose.Classpath parsed_paths ->
          loop parsed_paths roots sources classes
    with
    | JBasics.Class_structure_error _
    | Parsing.Parse_error
    | Invalid_argument _
    | Failure "lexing: empty token" -> loop paths roots sources classes
    | End_of_file ->
        close_in file_in;
        let classpath = IList.fold_left append_path "" (roots @ (add_android_jar paths)) in
        (classpath, sources, classes) in
  loop [] [] StringMap.empty JBasics.ClassSet.empty


type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t


type program = {
  classpath: Javalib.class_path;
  models: classmap;
  mutable classmap: classmap
}


let get_classmap program =
  program.classmap


let get_classpath program =
  program.classpath


let get_models program =
  program.models


let add_class cn jclass program =
  program.classmap <- JBasics.ClassMap.add cn jclass program.classmap


let lookup_node cn program =
  try
    Some (JBasics.ClassMap.find cn (get_classmap program))
  with Not_found ->
    try
      let jclass = Javalib.get_class (get_classpath program) cn in
      add_class cn jclass program;
      Some jclass
    with
    | JBasics.No_class_found _
    | JBasics.Class_structure_error _
    | Invalid_argument _ -> None


let classname_of_class_filename class_filename =
  let parts = Str.split (Str.regexp "/") class_filename in
  let classname_str =
    if IList.length parts > 1 then
      IList.fold_left (fun s p -> s^"."^p) (IList.hd parts) (IList.tl parts)
    else
      IList.hd parts in
  JBasics.make_cn classname_str


let extract_classnames classnames jar_filename =
  let file_in = Zip.open_in jar_filename in
  let collect classes entry =
    let class_filename = entry.Zip.filename in
    try
      let () = ignore (Str.search_forward (Str.regexp "class") class_filename 0) in
      (classname_of_class_filename (Filename.chop_extension class_filename):: classes)
    with Not_found -> classes in
  let classnames_after = IList.fold_left collect classnames (Zip.entries file_in) in
  Zip.close_in file_in;
  classnames_after


let collect_classes classmap jar_filename =
  let classpath = Javalib.class_path jar_filename in
  let collect classmap cn =
    JBasics.ClassMap.add cn (Javalib.get_class classpath cn) classmap in
  IList.fold_left collect classmap (extract_classnames [] jar_filename)


let classmap_of_classpath classpath =
  let jar_filenames =
    IList.filter (fun p -> not (Sys.is_directory p)) (split_classpath classpath) in
  IList.fold_left collect_classes JBasics.ClassMap.empty jar_filenames


let load_program classpath classes arg_source_files =
  JUtils.log "loading program ... %!";
  let models =
    if !models_jar = "" then JBasics.ClassMap.empty
    else collect_classes JBasics.ClassMap.empty !models_jar in
  let program = {
    classpath = Javalib.class_path classpath;
    models = models;
    classmap = JBasics.ClassMap.empty
  } in
  JBasics.ClassSet.iter
    (fun cn -> ignore (lookup_node cn program))
    classes;
  JUtils.log "done@.";
  program
