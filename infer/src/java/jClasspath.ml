(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
open Javalib_pack
module L = Logging

(** version of Javalib.get_class that does not spam stderr *)
let javalib_get_class = Utils.suppress_stderr2 Javalib.get_class

let models_specs_filenames = ref String.Set.empty

let models_jar = ref ""

let collect_specs_filenames jar_filename =
  let zip_channel = Zip.open_in jar_filename in
  let collect set e =
    let filename = e.Zip.filename in
    if not (Filename.check_suffix filename Config.specs_files_suffix) then set
    else
      let proc_filename = Filename.chop_extension (Filename.basename filename) in
      String.Set.add set proc_filename
  in
  models_specs_filenames :=
    List.fold ~f:collect ~init:!models_specs_filenames (Zip.entries zip_channel) ;
  Zip.close_in zip_channel


let add_models jar_filename =
  models_jar := jar_filename ;
  if Sys.file_exists !models_jar = `Yes then collect_specs_filenames jar_filename
  else L.(die InternalError) "Java model file not found"


let is_model procname = String.Set.mem !models_specs_filenames (Typ.Procname.to_filename procname)

let split_classpath cp = Str.split (Str.regexp JFile.sep) cp

let append_path classpath path =
  let full_path = Utils.filename_to_absolute ~root:Config.project_root path in
  if Sys.file_exists full_path = `Yes then
    if Int.equal (String.length classpath) 0 then full_path else classpath ^ JFile.sep ^ full_path
  else (
    L.debug Capture Medium "Path %s not found" full_path ;
    classpath )


type file_entry = Singleton of SourceFile.t | Duplicate of (string * SourceFile.t) list

type t = string * file_entry String.Map.t * JBasics.ClassSet.t

(* Open the source file and search for the package declaration.
   Only the case where the package is declared in a single line is supported *)
let read_package_declaration source_file =
  let path = SourceFile.to_abs_path source_file in
  let file_in = In_channel.create path in
  let remove_trailing_semicolon = Str.replace_first (Str.regexp ";") "" in
  let empty_package = "" in
  let rec loop () =
    try
      let line = remove_trailing_semicolon (In_channel.input_line_exn file_in) in
      match Str.split (Str.regexp "[ \t]+") line with
      | [] ->
          loop ()
      | [hd; package] when String.equal hd "package" ->
          package
      | _ ->
          loop ()
    with End_of_file -> In_channel.close file_in ; empty_package
  in
  loop ()


let add_source_file path map =
  let convert_to_absolute p =
    if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p
  in
  let basename = Filename.basename path in
  let entry =
    let current_source_file = SourceFile.from_abs_path (convert_to_absolute path) in
    try
      match String.Map.find_exn map basename with
      | Singleton previous_source_file ->
          (* Another source file with the same base name has been found.
             Reading the package from the source file to resolve the ambiguity
             only happens in this case *)
          let previous_package = read_package_declaration previous_source_file
          and current_package = read_package_declaration current_source_file in
          let source_list =
            [(current_package, current_source_file); (previous_package, previous_source_file)]
          in
          Duplicate source_list
      | Duplicate previous_source_files ->
          (* Two or more source file with the same base name have been found *)
          let current_package = read_package_declaration current_source_file in
          Duplicate ((current_package, current_source_file) :: previous_source_files)
    with
    | Not_found_s _ | Caml.Not_found ->
        (* Most common case: there is no conflict with the base name of the source file *)
        Singleton current_source_file
  in
  String.Map.set ~key:basename ~data:entry map


let add_root_path path roots = String.Set.add roots path

let load_from_verbose_output javac_verbose_out =
  let file_in = In_channel.create javac_verbose_out in
  let class_filename_re =
    Str.regexp
      (Printf.sprintf
         (* the unreadable regexp below captures 3 possible forms:
            1. [wrote DirectoryFileObject[/path/to/classes_out:path/to/File.java]], leaves `path/to/File.java` in match group 2
            2. [wrote RegularFileObject[path/to/File.java]], leaves `path/to/File.java` in match group 5
            3. [wrote SimpleFileObject[path/to/File.java]], also leaves `path/to/File.java` in match group 5 *)
         "\\[wrote \
          \\(DirectoryFileObject\\[%s:\\(.*\\)\\|\\(\\(Regular\\|Simple\\)FileObject\\[\\(.*\\)\\)\\)\\]\\]"
         Config.javac_classes_out)
  in
  let source_filename_re =
    Str.regexp "\\[parsing started \\(Regular\\|Simple\\)FileObject\\[\\(.*\\)\\]\\]"
  in
  let classpath_re = Str.regexp "\\[search path for class files: \\(.*\\)\\]" in
  let rec loop paths roots sources classes =
    try
      let line = In_channel.input_line_exn file_in in
      if Str.string_match class_filename_re line 0 then
        let path =
          try Str.matched_group 5 line with Caml.Not_found ->
            (* either matched group 5 is found, or matched group 2 is found, see doc for [class_filename_re] above *)
            Config.javac_classes_out ^/ Str.matched_group 2 line
        in
        let cn, root_info = Javalib.extract_class_name_from_file path in
        let root_dir =
          if String.equal root_info "" then Filename.current_dir_name else root_info
        in
        loop paths (add_root_path root_dir roots) sources (JBasics.ClassSet.add cn classes)
      else if Str.string_match source_filename_re line 0 then
        let path = Str.matched_group 2 line in
        loop paths roots (add_source_file path sources) classes
      else if Str.string_match classpath_re line 0 then
        let classpath = Str.matched_group 1 line in
        let parsed_paths = String.split ~on:',' classpath in
        loop parsed_paths roots sources classes
      else (* skip this line *)
        loop paths roots sources classes
    with
    | JBasics.Class_structure_error _ | Invalid_argument _ ->
        loop paths roots sources classes
    | End_of_file ->
        In_channel.close file_in ;
        let classpath = List.fold ~f:append_path ~init:"" (String.Set.elements roots @ paths) in
        (classpath, sources, classes)
  in
  loop [] String.Set.empty String.Map.empty JBasics.ClassSet.empty


let classname_of_class_filename class_filename =
  JBasics.make_cn (String.map ~f:(function '/' -> '.' | c -> c) class_filename)


let extract_classnames classnames jar_filename =
  let file_in = Zip.open_in jar_filename in
  let collect classes entry =
    let class_filename = entry.Zip.filename in
    match Filename.split_extension class_filename with
    | basename, Some "class" ->
        classname_of_class_filename basename :: classes
    | _ ->
        classes
  in
  let classnames_after = List.fold ~f:collect ~init:classnames (Zip.entries file_in) in
  Zip.close_in file_in ; classnames_after


let collect_classnames start_classmap jar_filename =
  List.fold
    ~f:(fun map cn -> JBasics.ClassSet.add cn map)
    ~init:start_classmap
    (extract_classnames [] jar_filename)


let search_classes path =
  let add_class roots classes class_filename =
    let cn, root_dir = Javalib.extract_class_name_from_file class_filename in
    (add_root_path root_dir roots, JBasics.ClassSet.add cn classes)
  in
  Utils.directory_fold
    (fun accu p ->
      let paths, classes = accu in
      if Filename.check_suffix p "class" then add_class paths classes p
      else if Filename.check_suffix p "jar" then
        (add_root_path p paths, collect_classnames classes p)
      else accu )
    (String.Set.empty, JBasics.ClassSet.empty)
    path


let search_sources () =
  let initial_map =
    List.fold ~f:(fun map path -> add_source_file path map) ~init:String.Map.empty Config.sources
  in
  match Config.sourcepath with
  | None ->
      initial_map
  | Some sourcepath ->
      Utils.directory_fold
        (fun map p -> if Filename.check_suffix p "java" then add_source_file p map else map)
        initial_map sourcepath


let load_from_arguments classes_out_path =
  let roots, classes = search_classes classes_out_path in
  let split cp_option = Option.value_map ~f:split_classpath ~default:[] cp_option in
  let combine path_list classpath =
    List.fold ~f:append_path ~init:classpath (List.rev path_list)
  in
  let classpath =
    combine (split Config.classpath) ""
    |> combine (String.Set.elements roots)
    |> combine (split Config.bootclasspath)
  in
  (classpath, search_sources (), classes)


type callee_status = Translated | Missing of JBasics.class_name * JBasics.method_signature

type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t

type classpath = {path: string; channel: Javalib.class_path}

type program =
  { classpath: classpath
  ; models: classmap
  ; mutable classmap: classmap
  ; callees: callee_status Typ.Procname.Hash.t }

let get_classmap program = program.classmap

let get_classpath_channel program = program.classpath.channel

let get_models program = program.models

let add_class cn jclass program =
  program.classmap <- JBasics.ClassMap.add cn jclass program.classmap


let set_callee_translated program pname =
  Typ.Procname.Hash.replace program.callees pname Translated


let add_missing_callee program pname cn ms =
  if not (Typ.Procname.Hash.mem program.callees pname) then
    Typ.Procname.Hash.add program.callees pname (Missing (cn, ms))


let iter_missing_callees program ~f =
  let select proc_name = function Translated -> () | Missing (cn, ms) -> f proc_name cn ms in
  Typ.Procname.Hash.iter select program.callees


let cleanup program = Javalib.close_class_path program.classpath.channel

let lookup_node cn program =
  try Some (JBasics.ClassMap.find cn (get_classmap program)) with Caml.Not_found -> (
    try
      let jclass = javalib_get_class (get_classpath_channel program) cn in
      add_class cn jclass program ; Some jclass
    with
    | JBasics.No_class_found _ ->
        (* TODO T28155039 Figure out when and what to log *)
        None
    | (JBasics.Class_structure_error _ | Invalid_argument _) as exn ->
        L.internal_error "ERROR: %s@." (Exn.to_string exn) ;
        None )


let collect_classes start_classmap jar_filename =
  let classpath = Javalib.class_path jar_filename in
  let collect classmap cn =
    try JBasics.ClassMap.add cn (javalib_get_class classpath cn) classmap
    with JBasics.Class_structure_error _ -> classmap
  in
  let classmap = List.fold ~f:collect ~init:start_classmap (extract_classnames [] jar_filename) in
  Javalib.close_class_path classpath ;
  classmap


let load_program classpath classes =
  L.(debug Capture Medium) "loading program ... %!" ;
  let models =
    if String.equal !models_jar "" then JBasics.ClassMap.empty
    else collect_classes JBasics.ClassMap.empty !models_jar
  in
  let program =
    { classpath= {path= classpath; channel= Javalib.class_path classpath}
    ; models
    ; classmap= JBasics.ClassMap.empty
    ; callees= Typ.Procname.Hash.create 128 }
  in
  JBasics.ClassSet.iter (fun cn -> ignore (lookup_node cn program)) classes ;
  L.(debug Capture Medium) "done@." ;
  program
