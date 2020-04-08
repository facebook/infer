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

(** version of Javalib.get_class that does not spam stderr *)
let javalib_get_class = Utils.suppress_stderr2 Javalib.get_class

let split_classpath = String.split ~on:JFile.sep

let classpath_of_paths paths =
  let of_path path =
    let full_path = Utils.filename_to_absolute ~root:Config.project_root path in
    match Sys.file_exists full_path with
    | `Yes ->
        Some full_path
    | _ ->
        L.debug Capture Medium "Path %s not found" full_path ;
        None
  in
  let string_sep = Char.to_string JFile.sep in
  List.filter_map paths ~f:of_path |> String.concat ~sep:string_sep


type file_entry = Singleton of SourceFile.t | Duplicate of (string * SourceFile.t) list

type t = {classpath: string; sources: file_entry String.Map.t; classes: JBasics.ClassSet.t}

(* Open the source file and search for the package declaration.
   Only the case where the package is declared in a single line is supported *)
let read_package_declaration source_file =
  let path = SourceFile.to_abs_path source_file in
  let process_line line =
    String.strip line |> String.lsplit2 ~on:';' |> Option.map ~f:fst
    |> Option.bind ~f:(String.chop_prefix ~prefix:"package")
    |> Option.map ~f:String.strip
  in
  let rec loop file_in =
    match In_channel.input_line file_in with
    | None ->
        None
    | Some line -> (
      match process_line line with Some package -> Some package | None -> loop file_in )
  in
  Utils.with_file_in path ~f:loop |> Option.value ~default:""


let add_source_file =
  let cwd = lazy (Sys.getcwd ()) in
  let convert_to_absolute p =
    if Filename.is_relative p then Filename.concat (Lazy.force cwd) p else p
  in
  fun path map ->
    let basename = Filename.basename path in
    let entry =
      let current_source_file = SourceFile.from_abs_path (convert_to_absolute path) in
      match String.Map.find map basename with
      | None ->
          (* Most common case: there is no conflict with the base name of the source file *)
          Singleton current_source_file
      | Some (Singleton previous_source_file) ->
          (* Another source file with the same base name has been found.
             Reading the package from the source file to resolve the ambiguity
             only happens in this case *)
          let previous_package = read_package_declaration previous_source_file
          and current_package = read_package_declaration current_source_file in
          let source_list =
            [(current_package, current_source_file); (previous_package, previous_source_file)]
          in
          Duplicate source_list
      | Some (Duplicate previous_source_files) ->
          (* Two or more source file with the same base name have been found *)
          let current_package = read_package_declaration current_source_file in
          Duplicate ((current_package, current_source_file) :: previous_source_files)
    in
    String.Map.set ~key:basename ~data:entry map


let add_root_path path roots = String.Set.add roots path

let load_from_verbose_output =
  let class_filename_re =
    Printf.sprintf
      (* the unreadable regexp below captures 3 possible forms:
         1. [wrote DirectoryFileObject[/path/to/classes_out:path/to/File.java]], leaves `path/to/File.java` in match group 2
         2. [wrote RegularFileObject[path/to/File.java]], leaves `path/to/File.java` in match group 5
         3. [wrote SimpleFileObject[path/to/File.java]], also leaves `path/to/File.java` in match group 5
         4. [wrote path/to/File.java] leaves `path/to/File.java` in match group 6 (from java 11)*)
      "\\[wrote \
       \\(DirectoryFileObject\\[%s:\\(.*\\)\\|\\(\\(Regular\\|Simple\\)FileObject\\[\\(.*\\)\\)\\]\\|\\(.*\\)\\)\\]"
      Config.javac_classes_out
    |> Str.regexp
  in
  let source_filename_re =
    Str.regexp "\\[parsing started \\(Regular\\|Simple\\)FileObject\\[\\(.*\\)\\]\\]"
  in
  let classpath_re = Str.regexp "\\[search path for class files: \\(.*\\)\\]" in
  let rec loop paths roots sources classes file_in =
    match In_channel.input_line file_in with
    | None ->
        let classpath = classpath_of_paths (String.Set.elements roots @ paths) in
        {classpath; sources; classes}
    | Some line when Str.string_match class_filename_re line 0 -> (
        let path =
          try Str.matched_group 5 line
          with Caml.Not_found -> (
            try Config.javac_classes_out ^/ Str.matched_group 2 line
            with Caml.Not_found ->
              (* either matched group 5, 6, or 2 is found, see doc for [class_filename_re] above *)
              Str.matched_group 6 line )
        in
        match Javalib.extract_class_name_from_file path with
        | exception (JBasics.Class_structure_error _ | Invalid_argument _) ->
            loop paths roots sources classes file_in
        | cn, root_info ->
            let root_dir =
              if String.equal root_info "" then Filename.current_dir_name else root_info
            in
            loop paths (add_root_path root_dir roots) sources (JBasics.ClassSet.add cn classes)
              file_in )
    | Some line when Str.string_match source_filename_re line 0 ->
        let path = Str.matched_group 2 line in
        loop paths roots (add_source_file path sources) classes file_in
    | Some line when Str.string_match classpath_re line 0 ->
        let classpath = Str.matched_group 1 line in
        let parsed_paths = String.split ~on:',' classpath in
        loop parsed_paths roots sources classes file_in
    | _ ->
        (* skip this line *)
        loop paths roots sources classes file_in
  in
  fun javac_verbose_out ->
    Utils.with_file_in javac_verbose_out
      ~f:(loop [] String.Set.empty String.Map.empty JBasics.ClassSet.empty)


let collect_classnames init jar_filename =
  let f acc filename_with_extension =
    match Filename.split_extension filename_with_extension with
    | class_filename, Some extension when String.equal extension "class" ->
        let classname =
          JBasics.make_cn (String.map ~f:(function '/' -> '.' | c -> c) class_filename)
        in
        JBasics.ClassSet.add classname acc
    | _ ->
        acc
  in
  Utils.zip_fold_filenames ~init ~f ~zip_filename:jar_filename


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
  let classpath =
    (* order follows https://docs.oracle.com/javase/7/docs/technotes/tools/windows/classpath.html *)
    split Config.bootclasspath @ split Config.classpath @ String.Set.elements roots
    |> classpath_of_paths
  in
  {classpath; sources= search_sources (); classes}


type callee_status = Translated | Missing of JBasics.class_name * JBasics.method_signature

type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t

(** We store for each classname the location of its declaration. This map is filled during
    JFrontend.compute_source_icfg and then it is used in JTransType.get_class_struct_typ before we
    lose access to JClasspath.program. At the end, the information seats in each Struct.t (stored in
    Tenv.t) *)
type java_location_map = Location.t JBasics.ClassMap.t

type program =
  { classpath_channel: Javalib.class_path
  ; mutable classmap: classmap
  ; mutable java_location_map: java_location_map
  ; callees: callee_status Procname.Hash.t }

let get_classmap program = program.classmap

let set_java_location program cn loc =
  program.java_location_map <- JBasics.ClassMap.add cn loc program.java_location_map


let get_java_location program cn =
  try Some (JBasics.ClassMap.find cn program.java_location_map) with Caml.Not_found -> None


let mem_classmap cn program = JBasics.ClassMap.mem cn program.classmap

let get_classpath_channel program = program.classpath_channel

let add_class cn jclass program =
  (* [prefix] must be a fresh class name *)
  let prefix = JBasics.cn_name cn ^ Config.java_lambda_marker_infix in
  (* we rewrite each class to replace invokedynamic (closure construction)
     with equivalent old-style Java code that implements a suitable Java interface *)
  let rewritten_jclass, new_classes = Javalib.remove_invokedynamics jclass ~prefix in
  program.classmap <- JBasics.ClassMap.add cn rewritten_jclass program.classmap ;
  (* the rewrite will generate new classes and we add them to the program *)
  JBasics.ClassMap.iter
    (fun cn jcl -> program.classmap <- JBasics.ClassMap.add cn jcl program.classmap)
    new_classes ;
  rewritten_jclass


let set_callee_translated program pname = Procname.Hash.replace program.callees pname Translated

let add_missing_callee program pname cn ms =
  if not (Procname.Hash.mem program.callees pname) then
    Procname.Hash.add program.callees pname (Missing (cn, ms))


let iter_missing_callees program ~f =
  let select proc_name = function Translated -> () | Missing (cn, ms) -> f proc_name cn ms in
  Procname.Hash.iter select program.callees


let cleanup program = Javalib.close_class_path program.classpath_channel

let lookup_node cn program =
  try Some (JBasics.ClassMap.find cn (get_classmap program))
  with Caml.Not_found -> (
    try
      let jclass = javalib_get_class (get_classpath_channel program) cn in
      Some (add_class cn jclass program)
    with
    | JBasics.No_class_found _ ->
        (* TODO T28155039 Figure out when and what to log *)
        None
    | (JBasics.Class_structure_error _ | Invalid_argument _ | Failure _) as exn ->
        L.internal_error "ERROR: %s@." (Exn.to_string exn) ;
        None )


let load_program ~classpath classes =
  L.(debug Capture Medium) "loading program ... %!" ;
  let program =
    { classpath_channel= Javalib.class_path classpath
    ; classmap= JBasics.ClassMap.empty
    ; java_location_map= JBasics.ClassMap.empty
    ; callees= Procname.Hash.create 128 }
  in
  JBasics.ClassSet.iter (fun cn -> ignore (lookup_node cn program)) classes ;
  L.(debug Capture Medium) "done@." ;
  program
