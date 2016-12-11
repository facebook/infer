(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module CLOpt = CommandLineOption
module F = Format
module L = Logging

type path_filter = SourceFile.t -> bool
type error_filter = Localise.t -> bool
type proc_filter = Procname.t -> bool

type filters =
  {
    path_filter : path_filter;
    error_filter : error_filter;
    proc_filter : proc_filter;
  }

let default_path_filter : path_filter = function _ -> true
let default_error_filter : error_filter = function _ -> true
let default_proc_filter : proc_filter = function _ -> true

let do_not_filter : filters =
  {
    path_filter = default_path_filter;
    error_filter = default_error_filter;
    proc_filter = default_proc_filter;
  }

type filter_config =
  {
    whitelist: string list;
    blacklist: string list;
    blacklist_files_containing : string list;
    suppress_errors: string list;
  }

let is_matching patterns =
  fun source_file ->
    let path = SourceFile.to_rel_path source_file in
    IList.exists
      (fun pattern ->
         try
           (Str.search_forward pattern path 0) = 0
         with Not_found -> false)
      patterns


(** Check if a proc name is matching the name given as string. *)
let match_method language proc_name method_name =
  not (BuiltinDecl.is_declared proc_name) &&
  Procname.get_language proc_name = language &&
  Procname.get_method proc_name = method_name

(* Module to create matcher based on strings present in the source file *)
module FileContainsStringMatcher = struct
  type matcher = SourceFile.t -> bool

  let default_matcher : matcher = fun _ -> false

  let file_contains regexp file_in =
    let rec loop () =
      try
        (Str.search_forward regexp (input_line file_in) 0) >= 0
      with
      | Not_found -> loop ()
      | End_of_file -> false in
    loop ()

  let create_matcher s_patterns =
    if s_patterns = [] then
      default_matcher
    else
      let source_map = ref SourceFile.Map.empty in
      let regexp =
        Str.regexp (String.concat ~sep:"\\|" s_patterns) in
      fun source_file ->
        try
          SourceFile.Map.find source_file !source_map
        with Not_found ->
        try
          let file_in = open_in (SourceFile.to_abs_path source_file) in
          let pattern_found = file_contains regexp file_in in
          In_channel.close file_in;
          source_map := SourceFile.Map.add source_file pattern_found !source_map;
          pattern_found
        with Sys_error _ -> false
end

type method_pattern = {
  class_name : string;
  method_name : string option;
  parameters : (string list) option;
}

type pattern =
  | Method_pattern of Config.language * method_pattern
  | Source_contains of Config.language * string

(* Module to create matcher based on source file names or class names and method names *)
module FileOrProcMatcher = struct

  type matcher = SourceFile.t -> Procname.t -> bool

  let default_matcher : matcher =
    fun _ _ -> false

  let create_method_matcher m_patterns =
    if m_patterns = [] then
      default_matcher
    else
      let pattern_map =
        IList.fold_left
          (fun map pattern ->
             let previous =
               try
                 String.Map.find_exn map pattern.class_name
               with Not_found -> [] in
             String.Map.add ~key:pattern.class_name ~data:(pattern :: previous) map)
          String.Map.empty
          m_patterns in
      let do_java pname_java =
        let class_name = Procname.java_get_class_name pname_java
        and method_name = Procname.java_get_method pname_java in
        try
          let class_patterns = String.Map.find_exn pattern_map class_name in
          IList.exists
            (fun p ->
               match p.method_name with
               | None -> true
               | Some m -> String.equal m method_name)
            class_patterns
        with Not_found -> false in

      fun _ proc_name ->
        match proc_name with
        | Procname.Java pname_java ->
            do_java pname_java
        | _ ->
            false

  let create_file_matcher patterns =
    let s_patterns, m_patterns =
      let collect (s_patterns, m_patterns) = function
        | Source_contains (_, s) -> (s:: s_patterns, m_patterns)
        | Method_pattern (_, mp) -> (s_patterns, mp :: m_patterns) in
      IList.fold_left collect ([], []) patterns in
    let s_matcher =
      let matcher = FileContainsStringMatcher.create_matcher s_patterns in
      fun source_file _ -> matcher source_file
    and m_matcher = create_method_matcher m_patterns in
    fun source_file proc_name ->
      m_matcher source_file proc_name || s_matcher source_file proc_name

  let load_matcher = create_file_matcher

  let _pp_pattern fmt pattern =
    let pp_string fmt s =
      Format.fprintf fmt "%s" s in
    let pp_option pp_value fmt = function
      | None -> pp_string fmt "None"
      | Some value -> Format.fprintf fmt "%a" pp_value value in
    let pp_key_value pp_value fmt (key, value) =
      Format.fprintf fmt "  %s: %a,\n" key (pp_option pp_value) value in
    let pp_method_pattern fmt mp =
      let pp_params fmt l =
        Format.fprintf fmt "[%a]"
          (Pp.semicolon_seq_oneline Pp.text pp_string) l in
      Format.fprintf fmt "%a%a%a"
        (pp_key_value pp_string) ("class", Some mp.class_name)
        (pp_key_value pp_string) ("method", mp.method_name)
        (pp_key_value pp_params) ("parameters", mp.parameters)
    and pp_source_contains fmt sc =
      Format.fprintf fmt "  pattern: %s\n" sc in
    match pattern with
    | Method_pattern (language, mp) ->
        Format.fprintf fmt "Method pattern (%s) {\n%a}\n"
          (Config.string_of_language language) pp_method_pattern mp
    | Source_contains (language, sc) ->
        Format.fprintf fmt "Source contains (%s) {\n%a}\n"
          (Config.string_of_language language) pp_source_contains sc

end (* of module FileOrProcMatcher *)

(* Module to create patterns that will match all overriding methods in the pattern *)
module OverridesMatcher = struct

  let load_matcher patterns =
    fun is_subtype proc_name ->
      let is_matching = function
        | Method_pattern (language, mp) ->
            is_subtype mp.class_name
            && (Option.value_map ~f:(match_method language proc_name) ~default:false mp.method_name)
        | _ -> failwith "Expecting method pattern" in
      IList.exists is_matching patterns

end

let patterns_of_json_with_key (json_key, json) =
  let default_method_pattern = {
    class_name = "";
    method_name = None;
    parameters = None
  } in

  let default_source_contains = "" in

  let language_of_string = function
    | "Java" ->
        Ok Config.Java
    | l ->
        Error ("Inferconfig JSON key " ^ json_key ^ " not supported for language " ^ l) in

  let rec detect_language = function
    | [] ->
        Error ("No language found for " ^ json_key ^ " in " ^ Config.inferconfig_file)
    | ("language", `String s) :: _ ->
        language_of_string s
    | _:: tl ->
        detect_language tl in

  (* Detect the kind of pattern, method pattern or pattern based on the content of the source file.
     Detecting the kind of patterns in a first step makes it easier to parse the parts of the
     pattern in a second step *)
  let detect_pattern assoc =
    match detect_language assoc with
    | Ok language ->
        let is_method_pattern key = IList.exists (String.equal key) ["class"; "method"]
        and is_source_contains key = IList.exists (String.equal key) ["source_contains"] in
        let rec loop = function
          | [] ->
              Error ("Unknown pattern for " ^ json_key ^ " in " ^ Config.inferconfig_file)
          | (key, _) :: _ when is_method_pattern key ->
              Ok (Method_pattern (language, default_method_pattern))
          | (key, _) :: _ when is_source_contains key ->
              Ok (Source_contains (language, default_source_contains))
          | _:: tl -> loop tl in
        loop assoc
    | Error _ as error ->
        error in

  (* Translate a JSON entry into a matching pattern *)
  let create_pattern (assoc : (string * Yojson.Basic.json) list) =
    let collect_params l =
      let collect accu = function
        | `String s -> s:: accu
        | _ -> failwith ("Unrecognised parameters in " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.rev (IList.fold_left collect [] l) in
    let create_method_pattern assoc =
      let loop mp = function
        | (key, `String s) when key = "class" ->
            { mp with class_name = s }
        | (key, `String s) when key = "method" ->
            { mp with method_name = Some s }
        | (key, `List l) when key = "parameters" ->
            { mp with parameters = Some (collect_params l) }
        | (key, _) when key = "language" -> mp
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.fold_left loop default_method_pattern assoc
    and create_string_contains assoc =
      let loop sc = function
        | (key, `String pattern) when key = "source_contains" -> pattern
        | (key, _) when key = "language" -> sc
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.fold_left loop default_source_contains assoc in
    match detect_pattern assoc with
    | Ok (Method_pattern (language, _)) ->
        Ok (Method_pattern (language, create_method_pattern assoc))
    | Ok (Source_contains (language, _)) ->
        Ok (Source_contains (language, create_string_contains assoc))
    | Error _ as error ->
        error in

  let warn_user msg =
    F.eprintf "WARNING: in file %s: error parsing option %s@\n%s"
      Config.inferconfig_file json_key msg in

  (* Translate all the JSON entries into matching patterns *)
  let rec translate accu = function
    | `Assoc l -> (
        match create_pattern l with
        | Ok pattern ->
            pattern :: accu
        | Error msg ->
            warn_user msg;
            accu)
    | `List l ->
        IList.fold_left translate accu l
    | json ->
        warn_user (Printf.sprintf "expected list or assoc json type, but got value %s"
                     (Yojson.Basic.to_string json));
        accu in

  translate [] json

let modeled_expensive_matcher =
  OverridesMatcher.load_matcher (patterns_of_json_with_key Config.patterns_modeled_expensive)

let never_return_null_matcher =
  FileOrProcMatcher.load_matcher (patterns_of_json_with_key Config.patterns_never_returning_null)

let skip_translation_matcher =
  FileOrProcMatcher.load_matcher (patterns_of_json_with_key Config.patterns_skip_translation)

let suppress_warnings_matcher =
  let error msg =
    F.eprintf
      "There was an issue reading the option %s.@\n\
       If you did not call %s directly, this is likely a bug in Infer.@\n\
       %s@."
      Config.suppress_warnings_annotations_long
      (Filename.basename Sys.executable_name)
      msg ;
    [] in
  let patterns =
    match Config.suppress_warnings_out with
    | Some path -> (
        match Utils.read_optional_json_file path with
        | Ok json -> (
            let json_key = "suppress_warnings" in
            match Yojson.Basic.Util.member json_key json with
            | `Null -> []
            | json -> patterns_of_json_with_key (json_key, json))
        | Error msg -> error ("Could not read or parse the supplied " ^ path ^ ":\n" ^ msg)
      )
    | None when Config.current_exe <> CLOpt.Java -> []
    | None when Option.is_some Config.generated_classes -> []
    | None ->
        error ("The option " ^ Config.suppress_warnings_annotations_long ^ " was not provided") in
  FileOrProcMatcher.load_matcher patterns

let load_filters analyzer =
  {
    whitelist = Config.analysis_path_regex_whitelist analyzer;
    blacklist = Config.analysis_path_regex_blacklist analyzer;
    blacklist_files_containing = Config.analysis_blacklist_files_containing analyzer;
    suppress_errors = Config.analysis_suppress_errors analyzer;
  }

let filters_from_inferconfig inferconfig : filters =
  let path_filter =
    let whitelist_filter : path_filter =
      if inferconfig.whitelist = [] then default_path_filter
      else is_matching (IList.map Str.regexp inferconfig.whitelist) in
    let blacklist_filter : path_filter =
      is_matching (IList.map Str.regexp inferconfig.blacklist) in
    let blacklist_files_containing_filter : path_filter =
      FileContainsStringMatcher.create_matcher inferconfig.blacklist_files_containing in
    function source_file ->
      whitelist_filter source_file &&
      not (blacklist_filter source_file) &&
      not (blacklist_files_containing_filter source_file) in
  let error_filter =
    function error_name ->
      let error_str = Localise.to_string error_name in
      not (IList.exists (String.equal error_str) inferconfig.suppress_errors) in
  {
    path_filter = path_filter;
    error_filter = error_filter;
    proc_filter = default_proc_filter;
  }

(* Create filters based on .inferconfig *)
let create_filters analyzer =
  if not Config.filter_paths then do_not_filter
  else filters_from_inferconfig (load_filters analyzer)

(* Decide whether a checker or error type is enabled or disabled based on*)
(* white/black listing in .inferconfig and the default value *)
let is_checker_enabled checker_name =
  match IList.mem (=) checker_name Config.disable_checks,
        IList.mem (=) checker_name Config.enable_checks with
  | false, false -> (* if it's not amond white/black listed then we use default value *)
      not (IList.mem (=) checker_name Config.checks_disabled_by_default)
  | true, false -> (* if it's blacklisted and not whitelisted then it should be disabled *)
      false
  | false, true -> (* if it is not blacklisted and it is whitelisted then it should be enabled *)
      true
  | true, true -> (* if it's both blacklisted and whitelisted then we flag error *)
      failwith ("Inconsistent setting in .inferconfig: checker" ^ checker_name ^ " is both blacklisted and whitelisted.")

(* This function loads and list the path that are being filtered by the analyzer. The results *)
(* are of the form: path/to/file.java -> {infer, eradicate} meaning that analysis results will *)
(* be reported on path/to/file.java both for infer and for eradicate *)
let test () =
  let filters =
    IList.map
      (fun (name, analyzer) -> (name, analyzer, create_filters analyzer))
      Config.string_to_analyzer in
  let matching_analyzers path =
    IList.fold_left
      (fun l (n, a, f) -> if f.path_filter path then (n,a) :: l else l)
      [] filters in
  Utils.directory_iter
    (fun path ->
       if DB.is_source_file path then
         let source_file = SourceFile.from_abs_path path in
         let matching = matching_analyzers source_file in
         if matching <> [] then
           let matching_s = String.concat ~sep:", " (IList.map fst matching) in
           L.stderr "%s -> {%s}@."
             (SourceFile.to_rel_path source_file)
             matching_s)
    (Sys.getcwd ())
