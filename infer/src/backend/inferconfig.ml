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
type proc_filter = Typ.Procname.t -> bool

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
    List.exists
      ~f:(fun pattern ->
          try
            Int.equal (Str.search_forward pattern path 0) 0
          with Not_found -> false)
      patterns


(** Check if a proc name is matching the name given as string. *)
let match_method language proc_name method_name =
  not (BuiltinDecl.is_declared proc_name) &&
  Config.equal_language (Typ.Procname.get_language proc_name) language &&
  String.equal (Typ.Procname.get_method proc_name) method_name

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
    if List.is_empty s_patterns then
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

  type matcher = SourceFile.t -> Typ.Procname.t -> bool

  let default_matcher : matcher =
    fun _ _ -> false

  let create_method_matcher m_patterns =
    if List.is_empty m_patterns then
      default_matcher
    else
      let pattern_map =
        List.fold
          ~f:(fun map pattern ->
              let previous =
                try
                  String.Map.find_exn map pattern.class_name
                with Not_found -> [] in
              String.Map.add ~key:pattern.class_name ~data:(pattern :: previous) map)
          ~init:String.Map.empty
          m_patterns in
      let do_java pname_java =
        let class_name = Typ.Procname.java_get_class_name pname_java
        and method_name = Typ.Procname.java_get_method pname_java in
        try
          let class_patterns = String.Map.find_exn pattern_map class_name in
          List.exists
            ~f:(fun p ->
                match p.method_name with
                | None -> true
                | Some m -> String.equal m method_name)
            class_patterns
        with Not_found -> false in

      fun _ proc_name ->
        match proc_name with
        | Typ.Procname.Java pname_java ->
            do_java pname_java
        | _ ->
            false

  let create_file_matcher patterns =
    let s_patterns, m_patterns =
      let collect (s_patterns, m_patterns) = function
        | Source_contains (_, s) -> (s:: s_patterns, m_patterns)
        | Method_pattern (_, mp) -> (s_patterns, mp :: m_patterns) in
      List.fold ~f:collect ~init:([], []) patterns in
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
      List.exists ~f:is_matching patterns

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
        Error ("JSON key " ^ json_key ^ " not supported for language " ^ l) in

  let rec detect_language = function
    | [] ->
        Error ("No language found for " ^ json_key)
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
        let is_method_pattern key = List.exists ~f:(String.equal key) ["class"; "method"]
        and is_source_contains key = List.exists ~f:(String.equal key) ["source_contains"] in
        let rec loop = function
          | [] ->
              Error ("Unknown pattern for " ^ json_key)
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
      List.rev (List.fold ~f:collect ~init:[] l) in
    let create_method_pattern assoc =
      let loop mp = function
        | (key, `String s) when String.equal key "class" ->
            { mp with class_name = s }
        | (key, `String s) when String.equal key "method" ->
            { mp with method_name = Some s }
        | (key, `List l) when String.equal key "parameters" ->
            { mp with parameters = Some (collect_params l) }
        | (key, _) when String.equal key "language" -> mp
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      List.fold ~f:loop ~init:default_method_pattern assoc
    and create_string_contains assoc =
      let loop sc = function
        | (key, `String pattern) when String.equal key "source_contains" -> pattern
        | (key, _) when String.equal key "language" -> sc
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      List.fold ~f:loop ~init:default_source_contains assoc in
    match detect_pattern assoc with
    | Ok (Method_pattern (language, _)) ->
        Ok (Method_pattern (language, create_method_pattern assoc))
    | Ok (Source_contains (language, _)) ->
        Ok (Source_contains (language, create_string_contains assoc))
    | Error _ as error ->
        error in

  let warn_user msg =
    CLOpt.warnf "WARNING: error parsing option %s@\n%s@." json_key msg in

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
        List.fold ~f:translate ~init:accu l
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
      if List.is_empty inferconfig.whitelist then default_path_filter
      else is_matching (List.map ~f:Str.regexp inferconfig.whitelist) in
    let blacklist_filter : path_filter =
      is_matching (List.map ~f:Str.regexp inferconfig.blacklist) in
    let blacklist_files_containing_filter : path_filter =
      FileContainsStringMatcher.create_matcher inferconfig.blacklist_files_containing in
    function source_file ->
      whitelist_filter source_file &&
      not (blacklist_filter source_file) &&
      not (blacklist_files_containing_filter source_file) in
  let error_filter =
    function error_name ->
      let error_str = Localise.to_issue_id error_name in
      not (List.exists ~f:(String.equal error_str) inferconfig.suppress_errors) in
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
  match List.mem ~equal:String.(=) Config.disable_checks checker_name,
        List.mem ~equal:String.(=) Config.enable_checks checker_name with
  | false, false -> (* if it's not amond white/black listed then we use default value *)
      not (List.mem ~equal:String.(=) Config.checks_disabled_by_default checker_name)
  | true, false -> (* if it's blacklisted and not whitelisted then it should be disabled *)
      false
  | false, true -> (* if it is not blacklisted and it is whitelisted then it should be enabled *)
      true
  | true, true -> (* if it's both blacklisted and whitelisted then we flag error *)
      failwithf "Inconsistent settings: checker %s is both blacklisted and whitelisted."
        checker_name

(* This function loads and list the path that are being filtered by the analyzer. The results *)
(* are of the form: path/to/file.java -> {infer, eradicate} meaning that analysis results will *)
(* be reported on path/to/file.java both for infer and for eradicate *)
let test () =
  let filters =
    List.map
      ~f:(fun (name, analyzer) -> (name, analyzer, create_filters analyzer))
      Config.string_to_analyzer in
  let matching_analyzers path =
    List.fold
      ~f:(fun l (n, a, f) -> if f.path_filter path then (n,a) :: l else l)
      ~init:[]
      filters in
  Utils.directory_iter
    (fun path ->
       if DB.is_source_file path then
         let source_file = SourceFile.from_abs_path path in
         let matching = matching_analyzers source_file in
         if matching <> [] then
           let matching_s = String.concat ~sep:", " (List.map ~f:fst matching) in
           L.stderr "%s -> {%s}@."
             (SourceFile.to_rel_path source_file)
             matching_s)
    (Sys.getcwd ())
