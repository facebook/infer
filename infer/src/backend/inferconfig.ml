(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

(** Name of the infer configuration file *)
let inferconfig_file = ".inferconfig"

let inferconfig_home = ref None

let local_config = ref None

(** Look up a key in a json file containing a list of strings *)
let lookup_string_list key json =
  Yojson.Basic.Util.filter_member key [json]
  |> Yojson.Basic.Util.flatten
  |> Yojson.Basic.Util.filter_string

type path_filter = DB.source_file -> bool
type error_filter = Localise.t -> bool

type filters =
  {
    path_filter : path_filter;
    error_filter: error_filter;
  }

let default_path_filter : path_filter = function path -> true
let default_error_filter : error_filter = function error_name -> true

let do_not_filter : filters =
  {
    path_filter = default_path_filter;
    error_filter = default_error_filter;
  }

type filter_config =
  {
    whitelist: string list;
    blacklist: string list;
    blacklist_files_containing : string list;
    suppress_errors: string list;
  }

let load_filters analyzer =
  try
    let json =
      match !inferconfig_home with
      | Some dir ->
          Yojson.Basic.from_file (Filename.concat dir inferconfig_file)
      | None -> Yojson.Basic.from_file inferconfig_file in
    let inferconfig =
      {
        whitelist = lookup_string_list (analyzer ^ "_whitelist") json;
        blacklist = lookup_string_list (analyzer ^ "_blacklist") json;
        blacklist_files_containing =
          lookup_string_list (analyzer ^ "_blacklist_files_containing") json;
        suppress_errors = lookup_string_list (analyzer ^ "_suppress_errors") json;
      } in
    Some inferconfig
  with Sys_error _ -> None

let is_matching patterns =
  fun source_file ->
    let path = DB.source_file_to_rel_path source_file in
    Utils.list_exists
      (fun pattern ->
         try
           (Str.search_forward pattern path 0) = 0
         with Not_found -> false)
      patterns

module FileContainsStringMatcher = struct
  type matcher = DB.source_file -> bool

  let default_matcher : matcher = fun fname -> false

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
            let file_in = open_in (DB.source_file_to_string source_file) in
            let pattern_found = file_contains regexp file_in in
            close_in file_in;
            source_map := DB.SourceFileMap.add source_file pattern_found !source_map;
            pattern_found
          with Sys_error _ -> false
end

let filters_from_inferconfig inferconfig : filters =
  let path_filter =
    let whitelist_filter : path_filter =
      if inferconfig.whitelist = [] then default_path_filter
      else is_matching (list_map Str.regexp inferconfig.whitelist) in
    let blacklist_filter : path_filter =
      is_matching (list_map Str.regexp inferconfig.blacklist) in
    let blacklist_files_containing_filter : path_filter =
      FileContainsStringMatcher.create_matcher inferconfig.blacklist_files_containing in
    function source_file ->
      whitelist_filter source_file &&
      not (blacklist_filter source_file) &&
      not (blacklist_files_containing_filter source_file) in
  let error_filter =
    function error_name ->
      let error_str = Localise.to_string error_name in
      not (list_exists (string_equal error_str) inferconfig.suppress_errors) in
  {
    path_filter = path_filter;
    error_filter = error_filter;
  }

(* Create filters based on .inferconfig.*)
(* The environment varialble NO_PATH_FILTERING disables path filtering. *)
let create_filters analyzer =
  Config.project_root := Some (Sys.getcwd ());
  if Config.from_env_variable "NO_PATH_FILTERING" then
    do_not_filter
  else
    match load_filters (Utils.string_of_analyzer analyzer) with
    | None -> do_not_filter
    | Some inferconfig ->
        filters_from_inferconfig inferconfig

module NeverReturnNull = struct

  let never_returning_null_key = "never_returning_null"

  type matcher = DB.source_file -> Procname.t -> bool

  let default_matcher : matcher =
    fun source_file proc_name -> false

  type method_pattern = {
    class_name : string;
    method_name : string option;
    parameters : (string list) option
  }

  let default_method_pattern = {
    class_name = "";
    method_name = None;
    parameters = None
  }

  let default_source_contains = ""

  type pattern =
    | Method_pattern of Sil.language * method_pattern
    | Source_contains of Sil.language * string

  let language_of_string = function
    | "Java" -> Sil.Java
    | "C_CPP" -> Sil.C_CPP
    | _ -> failwith ("Unknown language found in " ^ inferconfig_file)

  let pp_pattern fmt pattern =
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
        (pp_key_value pp_string) ("class", Some mp.class_name)
        (pp_key_value pp_string) ("method", mp.method_name)
        (pp_key_value pp_params) ("parameters", mp.parameters)
    and pp_source_contains fmt sc =
      Format.fprintf fmt "  pattern: %s\n" sc in
    match pattern with
    | Method_pattern (language, mp) ->
        Format.fprintf fmt "Method pattern (%s) {\n%a}\n"
          (Sil.string_of_language language) pp_method_pattern mp
    | Source_contains (language, sc) ->
        Format.fprintf fmt "Source contains (%s) {\n%a}\n"
          (Sil.string_of_language language) pp_source_contains sc

  let detect_language assoc =
    let rec loop = function
      | [] ->
          failwith
            ("No language found for " ^ never_returning_null_key ^ " in " ^ inferconfig_file)
      | (key, `String s) :: _ when key = "language" ->
          language_of_string s
      | _:: tl -> loop tl in
    loop assoc

  let detect_pattern assoc =
    let language = detect_language assoc in
    let is_method_pattern key = list_exists (string_equal key) ["class"; "method"]
    and is_source_contains key = list_exists (string_equal key) ["source_contains"] in
    let rec loop = function
      | [] ->
          failwith ("Unknown pattern for " ^ never_returning_null_key ^ " in " ^ inferconfig_file)
      | (key, _) :: _ when is_method_pattern key ->
          Method_pattern (language, default_method_pattern)
      | (key, _) :: _ when is_source_contains key ->
          Source_contains (language, default_source_contains)
      | _:: tl -> loop tl in
    loop assoc

  let create_pattern (assoc : (string * Yojson.Basic.json) list) =
    let collect_params l =
      let collect accu = function
        | `String s -> s:: accu
        | _ -> failwith ("Unrecognised parameters in " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      list_rev (list_fold_left collect [] l) in
    let create_method_pattern mp assoc =
      let loop mp = function
        | (key, `String s) when key = "class" ->
            { mp with class_name = s }
        | (key, `String s) when key = "method" ->
            { mp with method_name = Some s }
        | (key, `List l) when key = "parameters" ->
            { mp with parameters = Some (collect_params l) }
        | (key, _) when key = "language" -> mp
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      list_fold_left loop default_method_pattern assoc
    and create_string_contains sc assoc =
      let loop sc = function
        | (key, `String pattern) when key = "source_contains" -> pattern
        | (key, _) when key = "language" -> sc
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      list_fold_left loop default_source_contains assoc in
    match detect_pattern assoc with
    | Method_pattern (language, mp) ->
        Method_pattern (language, create_method_pattern mp assoc)
    | Source_contains (language, sc) ->
        Source_contains (language, create_string_contains sc assoc)

  let rec translate accu (json : Yojson.Basic.json) : pattern list =
    match json with
    | `Assoc l -> (create_pattern l):: accu
    | `List l -> list_fold_left translate accu l
    | _ -> assert false

  let create_method_matcher language m_patterns =
    if language <> Sil.Java then assert false
    else
    if m_patterns = [] then
      default_matcher
    else
      let pattern_map =
        list_fold_left
          (fun map pattern ->
             let previous =
               try
                 StringMap.find pattern.class_name map
               with Not_found -> [] in
             StringMap.add pattern.class_name (pattern:: previous) map)
          StringMap.empty
          m_patterns in
      fun source_file proc_name ->
        let class_name = Procname.java_get_class proc_name
        and method_name = Procname.java_get_method proc_name in
        try
          let class_patterns = StringMap.find class_name pattern_map in
          list_exists
            (fun p ->
               match p.method_name with
               | None -> true
               | Some m -> string_equal m method_name)
            class_patterns
        with Not_found -> false

  let create_file_matcher language patterns =
    let s_patterns, m_patterns =
      let collect (s_patterns, m_patterns) = function
        | Source_contains (lang, s) when lang = language -> (s:: s_patterns, m_patterns)
        | Method_pattern (lang, mp) when lang = language ->
            (s_patterns, mp :: m_patterns)
        | _ -> (s_patterns, m_patterns) in
      list_fold_left collect ([], []) patterns in
    let s_matcher =
      let matcher = FileContainsStringMatcher.create_matcher s_patterns in
      fun source_file proc_name -> matcher source_file
    and m_matcher = create_method_matcher language m_patterns in
    fun source_file proc_name ->
      m_matcher source_file proc_name || s_matcher source_file proc_name

  let load_matcher language =
    try
      let patterns =
        let found =
          Yojson.Basic.Util.filter_member
            never_returning_null_key
            [Yojson.Basic.from_file inferconfig_file] in
        list_fold_left translate [] found in
      create_file_matcher language patterns
    with Sys_error _ ->
      default_matcher

end (* of module NeverReturnNull *)

(* This function loads and list the path that are being filtered by the analyzer. The results *)
(* are of the form: path/to/file.java -> {infer, eradicate} meaning that analysis results will *)
(* be reported on path/to/file.java both for infer and for eradicate *)
let test () =
  Config.project_root := Some (Sys.getcwd ());
  let filters =
    Utils.list_map (fun analyzer -> (analyzer, create_filters analyzer)) Utils.analyzers in
  let matching_analyzers path =
    Utils.list_fold_left
      (fun l (a, f) -> if f.path_filter path then a:: l else l)
      [] filters in
  Utils.directory_iter
    (fun path ->
       if DB.is_source_file path then
         let source_file = (DB.source_file_from_string path) in
         let matching = matching_analyzers source_file in
         if matching <> [] then
           let matching_s =
             Utils.join_strings ", "
               (Utils.list_map Utils.string_of_analyzer matching) in
           Logging.stderr "%s -> {%s}@."
             (DB.source_file_to_rel_path source_file)
             matching_s)
    (Sys.getcwd ())
