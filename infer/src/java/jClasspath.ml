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

let models_specs_filenames = ref StringSet.empty

let models_jar = ref ""


let models_tenv = ref (Tenv.create ())


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
      match Tenv.load_from_file temp_tenv_filename with
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
    if not (Filename.check_suffix filename Config.specs_files_suffix) then set
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
    DB.rel_source_file_from_abs_path Config.project_root path


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


type file_entry =
  | Singleton of DB.source_file
  | Duplicate of (string * DB.source_file) list


(* Open the source file and search for the package declaration.
   Only the case where the package is declared in a single line is supported *)
let read_package_declaration source_file =
  let path = DB.source_file_to_abs_path source_file in
  let file_in = open_in path in
  let remove_trailing_semicolon =
    Str.replace_first (Str.regexp ";") "" in
  let empty_package = "" in
  let rec loop () =
    try
      let line = remove_trailing_semicolon (input_line file_in) in
      match Str.split (Str.regexp "[ \t]+") line with
      | [] -> loop ()
      | hd::package::[] when hd = "package" -> package
      | _ -> loop ()
    with End_of_file ->
      close_in file_in;
      empty_package in
  loop ()


let add_source_file path map =
  let convert_to_absolute p =
    if Filename.is_relative p then
      Filename.concat (Sys.getcwd ()) p
    else
      p in
  let basename = Filename.basename path in
  let entry =
    let current_source_file =
      java_source_file_from_path (convert_to_absolute path) in
    try
      match StringMap.find basename map with
      | Singleton previous_source_file ->
          (* Another source file with the same base name has been found.
             Reading the package from the source file to resolve the ambiguity
             only happens in this case *)
          let previous_package = read_package_declaration previous_source_file
          and current_package = read_package_declaration current_source_file in
          let source_list = [
            (current_package, current_source_file);
            (previous_package, previous_source_file)] in
          Duplicate source_list
      | Duplicate previous_source_files ->
          (* Two or more source file with the same base name have been found *)
          let current_package = read_package_declaration current_source_file in
          Duplicate ((current_package, current_source_file) :: previous_source_files)
    with Not_found ->
      (* Most common case: there is no conflict with the base name of the source file *)
      Singleton current_source_file in
  StringMap.add basename entry map


let load_sources_and_classes () =
  let file_in = open_in Config.javac_verbose_out in
  let class_filename_re =
    Str.regexp
      "\\[wrote RegularFileObject\\[\\(.*\\)\\]\\]" in
  let source_filename_re =
    Str.regexp
      "\\[parsing started RegularFileObject\\[\\(.*\\)\\]\\]" in
  let classpath_re =
    Str.regexp
      "\\[search path for class files: \\(.*\\)\\]" in
  let rec loop paths roots sources classes =
    try
      let line = input_line file_in in
      if Str.string_match class_filename_re line 0 then
        let path = Str.matched_group 1 line in
        let cn, root_info = Javalib.extract_class_name_from_file path in
        let root_dir = if root_info = "" then Filename.current_dir_name else root_info in
        let updated_roots =
          if IList.exists (fun p -> p = root_dir) roots then roots
          else root_dir:: roots in
        loop paths updated_roots sources (JBasics.ClassSet.add cn classes)
      else if Str.string_match source_filename_re line 0 then
        let path = Str.matched_group 1 line in
        loop paths roots (add_source_file path sources) classes
      else if Str.string_match classpath_re line 0 then
        let classpath = Str.matched_group 1 line in
        let parsed_paths = Str.split (Str.regexp_string ",") classpath in
        loop parsed_paths roots sources classes
      else
        (* skip this line *)
        loop paths roots sources classes
    with
    | JBasics.Class_structure_error _
    | Invalid_argument _ -> loop paths roots sources classes
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

let cleanup program =
  Javalib.close_class_path program.classpath

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


let collect_classes start_classmap jar_filename =
  let classpath = Javalib.class_path jar_filename in
  let collect classmap cn =
    try
      JBasics.ClassMap.add cn (Javalib.get_class classpath cn) classmap
    with JBasics.Class_structure_error _ ->
      classmap in
  let classmap =
    IList.fold_left
      collect
      start_classmap
      (extract_classnames [] jar_filename) in
  Javalib.close_class_path classpath;
  classmap


let load_program classpath classes =
  L.out_debug "loading program ... %!";
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
  L.out_debug "done@.";
  program
