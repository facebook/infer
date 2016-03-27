(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging

(** Look up a key in a json file containing a list of strings *)
let lookup_string_list key json =
  Yojson.Basic.Util.filter_member key [json]
  |> Yojson.Basic.Util.flatten
  |> Yojson.Basic.Util.filter_string

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
  | Method_pattern of Config.language * method_pattern
  | Source_contains of Config.language * string

let language_of_string json_key = function
  | "Java" -> Config.Java
  | l -> failwith ("Inferconfig JSON key " ^ json_key ^ " not supported for language " ^ l)

let detect_language json_key assoc =
  let rec loop = function
    | [] ->
        failwith
          ("No language found for " ^ json_key ^ " in " ^ Config.inferconfig_file)
    | (key, `String s) :: _ when key = "language" ->
        language_of_string json_key s
    | _:: tl -> loop tl in
  loop assoc

(* Detect the kind of pattern, method pattern or pattern based on the content of the source file.
   Detecting the kind of patterns in a first step makes it easier to parse the parts of the
   pattern in a second step *)
let detect_pattern json_key assoc =
  let language = detect_language json_key assoc in
  let is_method_pattern key = IList.exists (string_equal key) ["class"; "method"]
  and is_source_contains key = IList.exists (string_equal key) ["source_contains"] in
  let rec loop = function
    | [] ->
        failwith ("Unknown pattern for " ^ json_key ^ " in " ^ Config.inferconfig_file)
    | (key, _) :: _ when is_method_pattern key ->
        Method_pattern (language, default_method_pattern)
    | (key, _) :: _ when is_source_contains key ->
        Source_contains (language, default_source_contains)
    | _:: tl -> loop tl in
  loop assoc

(* Translate a JSON entry into a matching pattern *)
let create_pattern json_key (assoc : (string * Yojson.Basic.json) list) =
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
  match detect_pattern json_key assoc with
  | Method_pattern (language, _) ->
      Method_pattern (language, create_method_pattern assoc)
  | Source_contains (language, _) ->
      Source_contains (language, create_string_contains assoc)

(* Translate all the JSON entries into matching patterns *)
let rec translate json_key accu (json : Yojson.Basic.json) : pattern list =
  match json with
  | `Assoc l -> (create_pattern json_key l):: accu
  | `List l -> IList.fold_left (translate json_key) accu l
  | _ -> assert false

(* Creates a list of matching patterns for the given inferconfig file *)
let load_patterns json_key inferconfig =
  let found =
    Yojson.Basic.Util.filter_member
      json_key
      [Yojson.Basic.from_file inferconfig] in
  IList.fold_left (translate json_key) [] found


(** Check if a proc name is matching the name given as string. *)
let match_method language proc_name method_name =
  not (Builtin.is_registered proc_name) &&
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
          let file_in = open_in (DB.source_file_to_string source_file) in
          let pattern_found = file_contains regexp file_in in
          close_in file_in;
          source_map := DB.SourceFileMap.add source_file pattern_found !source_map;
          pattern_found
        with Sys_error _ -> false
end

module type MATCHABLE_JSON = sig
  val json_key : string
end

module type Matcher = sig
  type matcher = DB.source_file -> Procname.t -> bool
  val load_matcher : string -> matcher
end

(* Module to create matcher based on source file names or class names and method names *)
module FileOrProcMatcher = functor (M : MATCHABLE_JSON) ->
struct

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
                 StringMap.find pattern.class_name map
               with Not_found -> [] in
             StringMap.add pattern.class_name (pattern:: previous) map)
          StringMap.empty
          m_patterns in
      let do_java pname_java =
        let class_name = Procname.java_get_class_name pname_java
        and method_name = Procname.java_get_method pname_java in
        try
          let class_patterns = StringMap.find class_name pattern_map in
          IList.exists
            (fun p ->
               match p.method_name with
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
        | Source_contains (_, s) -> (s:: s_patterns, m_patterns)
        | Method_pattern (_, mp) -> (s_patterns, mp :: m_patterns) in
      IList.fold_left collect ([], []) patterns in
    let s_matcher =
      let matcher = FileContainsStringMatcher.create_matcher s_patterns in
      fun source_file _ -> matcher source_file
    and m_matcher = create_method_matcher m_patterns in
    fun source_file proc_name ->
      m_matcher source_file proc_name || s_matcher source_file proc_name

  let load_matcher inferconfig =
    if Sys.file_exists inferconfig then
      create_file_matcher (load_patterns M.json_key inferconfig)
    else
      default_matcher


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
module OverridesMatcher = functor (M : MATCHABLE_JSON) ->
struct

  type matcher = (string -> bool) -> Procname.t -> bool

  let default_matcher _ _ = false

  let load_matcher inferconfig =
    if Sys.file_exists inferconfig then
      fun is_subtype proc_name ->
        let is_matching = function
          | Method_pattern (language, mp) ->
              is_subtype mp.class_name
              && Option.map_default (match_method language proc_name) false mp.method_name
          | _ -> failwith "Expecting method pattern" in
        IList.exists is_matching (load_patterns M.json_key inferconfig)
    else
      default_matcher

end

module NeverReturnNull = FileOrProcMatcher(struct
    let json_key = "never_returning_null"
  end)

module SuppressWarningsMatcher = FileOrProcMatcher(struct
    let json_key = "suppress_warnings"
  end)

module SkipTranslationMatcher = FileOrProcMatcher(struct
    let json_key = "skip_translation"
  end)

module ModeledExpensiveMatcher = OverridesMatcher(struct
    let json_key = "modeled_expensive"
  end)


let inferconfig () =
  match !Config.inferconfig_home with
  | Some dir -> Filename.concat dir Config.inferconfig_file
  | None -> Config.inferconfig_file

let load_filters analyzer =
  let inferconfig_file = inferconfig () in
  if Sys.file_exists inferconfig_file then
    try
      let json = Yojson.Basic.from_file inferconfig_file in
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
  else None


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
(* The environment varialble NO_PATH_FILTERING disables path filtering. *)
let create_filters analyzer =
  Config.project_root := Some (Sys.getcwd ());
  if Config.from_env_variable "NO_PATH_FILTERING" then do_not_filter
  else
    match load_filters (Utils.string_of_analyzer analyzer) with
    | None -> do_not_filter
    | Some inferconfig -> filters_from_inferconfig inferconfig

(* This function loads and list the path that are being filtered by the analyzer. The results *)
(* are of the form: path/to/file.java -> {infer, eradicate} meaning that analysis results will *)
(* be reported on path/to/file.java both for infer and for eradicate *)
let test () =
  Config.project_root := Some (Sys.getcwd ());
  let filters =
    IList.map (fun analyzer -> (analyzer, create_filters analyzer)) analyzers in
  let matching_analyzers path =
    IList.fold_left
      (fun l (a, f) -> if f.path_filter path then a:: l else l)
      [] filters in
  directory_iter
    (fun path ->
       if DB.is_source_file path then
         let source_file = (DB.source_file_from_string path) in
         let matching = matching_analyzers source_file in
         if matching <> [] then
           let matching_s =
             join_strings ", "
               (IList.map string_of_analyzer matching) in
           L.stderr "%s -> {%s}@."
             (DB.source_file_to_rel_path source_file)
             matching_s)
    (Sys.getcwd ())
