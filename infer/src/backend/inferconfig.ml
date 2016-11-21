(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging

type path_filter = DB.source_file -> bool
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
    let path = DB.source_file_to_rel_path source_file in
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
  type matcher = DB.source_file -> bool

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
      let source_map = ref DB.SourceFileMap.empty in
      let regexp =
        Str.regexp (join_strings "\\|" s_patterns) in
      fun source_file ->
        try
          DB.SourceFileMap.find source_file !source_map
        with Not_found ->
        try
          let file_in = open_in (DB.source_file_to_abs_path source_file) in
          let pattern_found = file_contains regexp file_in in
          close_in file_in;
          source_map := DB.SourceFileMap.add source_file pattern_found !source_map;
          pattern_found
        with Sys_error _ -> false
end

(* Module to create matcher based on source file names or class names and method names *)
module FileOrProcMatcher = struct

  type matcher = DB.source_file -> Procname.t -> bool

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
                 StringMap.find pattern.Config.class_name map
               with Not_found -> [] in
             StringMap.add pattern.Config.class_name (pattern:: previous) map)
          StringMap.empty
          m_patterns in
      let do_java pname_java =
        let class_name = Procname.java_get_class_name pname_java
        and method_name = Procname.java_get_method pname_java in
        try
          let class_patterns = StringMap.find class_name pattern_map in
          IList.exists
            (fun p ->
               match p.Config.method_name with
               | None -> true
               | Some m -> string_equal m method_name)
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
        | Config.Source_contains (_, s) -> (s:: s_patterns, m_patterns)
        | Config.Method_pattern (_, mp) -> (s_patterns, mp :: m_patterns) in
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
          (pp_semicolon_seq_oneline pe_text pp_string) l in
      Format.fprintf fmt "%a%a%a"
        (pp_key_value pp_string) ("class", Some mp.Config.class_name)
        (pp_key_value pp_string) ("method", mp.Config.method_name)
        (pp_key_value pp_params) ("parameters", mp.Config.parameters)
    and pp_source_contains fmt sc =
      Format.fprintf fmt "  pattern: %s\n" sc in
    match pattern with
    | Config.Method_pattern (language, mp) ->
        Format.fprintf fmt "Method pattern (%s) {\n%a}\n"
          (Config.string_of_language language) pp_method_pattern mp
    | Config.Source_contains (language, sc) ->
        Format.fprintf fmt "Source contains (%s) {\n%a}\n"
          (Config.string_of_language language) pp_source_contains sc

end (* of module FileOrProcMatcher *)

(* Module to create patterns that will match all overriding methods in the pattern *)
module OverridesMatcher = struct

  let load_matcher patterns =
    fun is_subtype proc_name ->
      let is_matching = function
        | Config.Method_pattern (language, mp) ->
            is_subtype mp.Config.class_name
            && Option.map_default (match_method language proc_name) false mp.Config.method_name
        | _ -> failwith "Expecting method pattern" in
      IList.exists is_matching patterns

end

let never_return_null_matcher = FileOrProcMatcher.load_matcher Config.patterns_never_returning_null
let skip_translation_matcher = FileOrProcMatcher.load_matcher Config.patterns_skip_translation
let suppress_warnings_matcher = FileOrProcMatcher.load_matcher Config.patterns_suppress_warnings
let modeled_expensive_matcher = OverridesMatcher.load_matcher Config.patterns_modeled_expensive

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
      not (IList.exists (string_equal error_str) inferconfig.suppress_errors) in
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
  directory_iter
    (fun path ->
       if DB.is_source_file path then
         let source_file = (DB.source_file_from_string path) in
         let matching = matching_analyzers source_file in
         if matching <> [] then
           let matching_s = join_strings ", " (IList.map fst matching) in
           L.stderr "%s -> {%s}@."
             (DB.source_file_to_rel_path source_file)
             matching_s)
    (Sys.getcwd ())
