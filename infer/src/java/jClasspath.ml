(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
module F = Format
module L = Logging

let split_classpath = String.split ~on:JFile.sep

let classpath_of_paths paths =
  let of_path path =
    let full_path = Utils.filename_to_absolute ~root:Config.project_root path in
    if ISys.file_exists full_path then Some full_path
    else (
      L.debug Capture Medium "Path %s not found" full_path ;
      None )
  in
  let string_sep = Char.to_string JFile.sep in
  List.filter_map paths ~f:of_path |> String.concat ~sep:string_sep


type file_entry = Singleton of SourceFile.t | Duplicate of (string list * SourceFile.t) list

type t =
  { classpath_channel: Javalib.class_path
  ; sources: file_entry String.Map.t
  ; classes: JBasics.ClassSet.t }

(* Open the source file and search for the package declaration.
   Only the case where the package is declared in a single line is supported *)
let read_package_declaration source_file =
  let path = SourceFile.to_abs_path source_file in
  let process_line line =
    String.strip line |> String.lsplit2 ~on:';' |> Option.map ~f:fst
    |> Option.bind ~f:(String.chop_prefix ~prefix:"package")
    |> Option.map ~f:String.strip
    |> Option.map ~f:(String.split ~on:'.')
  in
  let rec loop file_in =
    match In_channel.input_line file_in with
    | None ->
        None
    | Some line -> (
      match process_line line with Some package -> Some package | None -> loop file_in )
  in
  Utils.with_file_in path ~f:loop |> Option.value ~default:[]


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

let read_modules_1 path =
  let temp_dir = Filename.temp_dir "java_modules_lib" "" in
  Epilogues.register
    ~f:(fun () -> Utils.rmtree temp_dir)
    ~description:("Remove the temp dir for java modules: " ^ temp_dir) ;
  let jimage_cmd =
    ["jimage"; "extract"; F.sprintf "--dir=%s" temp_dir; path]
    |> List.map ~f:Escape.escape_shell |> String.concat ~sep:" "
  in
  L.debug Capture Medium "reading Java modules with: %s@\n" jimage_cmd ;
  let jimage_ret = Sys.command jimage_cmd in
  let root_paths = ref [] in
  if Int.equal jimage_ret 0 then
    Utils.iter_dir temp_dir ~f:(fun path -> root_paths := path :: !root_paths)
  else L.debug Capture Medium "Failed to run jimage for reading java modules.@\n" ;
  !root_paths


let read_modules paths =
  let suffix = Filename.dir_sep ^ "modules" in
  List.concat_map paths ~f:(fun path ->
      if String.is_suffix path ~suffix then read_modules_1 path else [path] )


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
        let paths = if Config.java_read_modules then read_modules paths else paths in
        let classpath = classpath_of_paths (String.Set.elements roots @ paths) in
        {classpath_channel= Javalib.class_path classpath; sources; classes}
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
  let f acc _ zip_entry =
    match Filename.split_extension zip_entry.Zip.filename with
    | class_filename, Some "class" ->
        let classname =
          JBasics.make_cn (String.map ~f:(function '/' -> '.' | c -> c) class_filename)
        in
        JBasics.ClassSet.add classname acc
    | _ ->
        acc
  in
  Utils.zip_fold ~init ~f ~zip_filename:jar_filename


let search_classes path =
  let add_class roots classes class_filename =
    let cn, root_dir = Javalib.extract_class_name_from_file class_filename in
    (add_root_path root_dir roots, JBasics.ClassSet.add cn classes)
  in
  Utils.directory_fold
    (fun accu p ->
      let paths, classes = accu in
      match Filename.split_extension p with
      | _, Some "class" ->
          add_class paths classes p
      | _, Some ("jar" | "war") ->
          (add_root_path p paths, collect_classnames classes p)
      | _ ->
          accu )
    (String.Set.empty, JBasics.ClassSet.empty)
    path


let is_valid_source_file path =
  ( Filename.check_suffix path ".java"
  || (Config.kotlin_capture && Filename.check_suffix path Config.kotlin_source_extension) )
  && PolyVariantEqual.(Sys.is_file path <> `No)


let search_sources sources =
  let initial_map =
    List.fold sources ~init:String.Map.empty ~f:(fun map path ->
        if is_valid_source_file path then add_source_file path map
        else (
          L.external_warning "'%s' does not appear to be a valid source file, skipping@\n" path ;
          map ) )
  in
  match Config.sourcepath with
  | None ->
      initial_map
  | Some sourcepath ->
      Utils.directory_fold
        (fun map path -> if is_valid_source_file path then add_source_file path map else map)
        initial_map sourcepath


let load_from_arguments classes_out_path sources =
  let roots, classes = search_classes classes_out_path in
  let split cp_option = Option.value_map ~f:split_classpath ~default:[] cp_option in
  let classpath =
    (* order follows https://docs.oracle.com/javase/7/docs/technotes/tools/windows/classpath.html *)
    split Config.bootclasspath @ split Config.classpath @ String.Set.elements roots
    |> classpath_of_paths
  in
  {classpath_channel= Javalib.class_path classpath; sources= search_sources sources; classes}


type source =
  | FromVerboseOut of {verbose_out_file: string}
  | FromArguments of {path: string; sources: string list}

let with_classpath ~f source =
  let classpath =
    match source with
    | FromVerboseOut {verbose_out_file} ->
        load_from_verbose_output verbose_out_file
    | FromArguments {path; sources} ->
        load_from_arguments path sources
  in
  if String.Map.is_empty classpath.sources then
    L.user_warning "No Java source code loaded. Analyzing JAR/WAR files directly" ;
  if String.Map.is_empty classpath.sources && JBasics.ClassSet.is_empty classpath.classes then
    L.(die InternalError) "Failed to load any Java source or class files" ;
  L.(debug Capture Quiet)
    "Translating %d source files (%d classes)@."
    (String.Map.length classpath.sources)
    (JBasics.ClassSet.cardinal classpath.classes) ;
  f classpath ;
  Javalib.close_class_path classpath.classpath_channel
