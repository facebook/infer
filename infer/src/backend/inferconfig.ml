(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CLOpt = CommandLineOption
module L = Logging

type path_filter = SourceFile.t -> bool

type error_filter = IssueType.t -> bool

type proc_filter = Typ.Procname.t -> bool

type filters = {path_filter: path_filter; error_filter: error_filter; proc_filter: proc_filter}

let default_path_filter : path_filter = function _ -> true

let default_error_filter : error_filter = function _ -> true

let default_proc_filter : proc_filter = function _ -> true

let do_not_filter : filters =
  { path_filter= default_path_filter
  ; error_filter= default_error_filter
  ; proc_filter= default_proc_filter }


type filter_config =
  { whitelist: string list
  ; blacklist: string list
  ; blacklist_files_containing: string list
  ; suppress_errors: string list }

let is_matching patterns source_file =
  let path = SourceFile.to_rel_path source_file in
  List.exists
    ~f:(fun pattern ->
      try Int.equal (Str.search_forward pattern path 0) 0 with Caml.Not_found -> false )
    patterns


(** Check if a proc name is matching the name given as string. *)
let match_method language proc_name method_name =
  (not (BuiltinDecl.is_declared proc_name))
  && Language.equal (Typ.Procname.get_language proc_name) language
  && String.equal (Typ.Procname.get_method proc_name) method_name


(* Module to create matcher based on strings present in the source file *)
module FileContainsStringMatcher = struct
  type matcher = SourceFile.t -> bool

  let default_matcher : matcher = fun _ -> false

  let file_contains regexp file_in =
    let rec loop () =
      try Str.search_forward regexp (In_channel.input_line_exn file_in) 0 >= 0 with
      | Caml.Not_found ->
          loop ()
      | End_of_file ->
          false
    in
    loop ()


  let create_matcher s_patterns =
    if List.is_empty s_patterns then default_matcher
    else
      let source_map = ref SourceFile.Map.empty in
      let regexp = Str.regexp (String.concat ~sep:"\\|" s_patterns) in
      fun source_file ->
        try SourceFile.Map.find source_file !source_map with Caml.Not_found -> (
          try
            let file_in = In_channel.create (SourceFile.to_abs_path source_file) in
            let pattern_found = file_contains regexp file_in in
            In_channel.close file_in ;
            source_map := SourceFile.Map.add source_file pattern_found !source_map ;
            pattern_found
          with Sys_error _ -> false )
end

type method_pattern = {class_name: string; method_name: string option}

type pattern =
  | Method_pattern of Language.t * method_pattern
  | Source_contains of Language.t * string

(* Module to create matcher based on source file names or class names and method names *)
module FileOrProcMatcher = struct
  type matcher = SourceFile.t -> Typ.Procname.t -> bool

  let default_matcher : matcher = fun _ _ -> false

  let create_method_matcher m_patterns =
    if List.is_empty m_patterns then default_matcher
    else
      let pattern_map =
        List.fold
          ~f:(fun map pattern ->
            let previous =
              try String.Map.find_exn map pattern.class_name with
              | Not_found_s _ | Caml.Not_found ->
                  []
            in
            String.Map.set ~key:pattern.class_name ~data:(pattern :: previous) map )
          ~init:String.Map.empty m_patterns
      in
      let do_java pname_java =
        let class_name = Typ.Procname.Java.get_class_name pname_java
        and method_name = Typ.Procname.Java.get_method pname_java in
        try
          let class_patterns = String.Map.find_exn pattern_map class_name in
          List.exists
            ~f:(fun p ->
              match p.method_name with None -> true | Some m -> String.equal m method_name )
            class_patterns
        with
        | Not_found_s _ | Caml.Not_found ->
            false
      in
      fun _ proc_name ->
        match proc_name with Typ.Procname.Java pname_java -> do_java pname_java | _ -> false


  let create_file_matcher patterns =
    let s_patterns, m_patterns =
      let collect (s_patterns, m_patterns) = function
        | Source_contains (_, s) ->
            (s :: s_patterns, m_patterns)
        | Method_pattern (_, mp) ->
            (s_patterns, mp :: m_patterns)
      in
      List.fold ~f:collect ~init:([], []) patterns
    in
    let s_matcher =
      let matcher = FileContainsStringMatcher.create_matcher s_patterns in
      fun source_file _ -> matcher source_file
    and m_matcher = create_method_matcher m_patterns in
    fun source_file proc_name -> m_matcher source_file proc_name || s_matcher source_file proc_name


  let load_matcher = create_file_matcher

  let _pp_pattern fmt pattern =
    let pp_key_value pp_value fmt (key, value) =
      Format.fprintf fmt "  %s: %a,@\n" key (Pp.option pp_value) value
    in
    let pp_method_pattern fmt mp =
      Format.fprintf fmt "%a%a"
        (pp_key_value Format.pp_print_string)
        ("class", Some mp.class_name)
        (pp_key_value Format.pp_print_string)
        ("method", mp.method_name)
    and pp_source_contains fmt sc = Format.fprintf fmt "  pattern: %s@\n" sc in
    match pattern with
    | Method_pattern (language, mp) ->
        Format.fprintf fmt "Method pattern (%s) {@\n%a}@\n" (Language.to_string language)
          pp_method_pattern mp
    | Source_contains (language, sc) ->
        Format.fprintf fmt "Source contains (%s) {@\n%a}@\n" (Language.to_string language)
          pp_source_contains sc
end

(* of module FileOrProcMatcher *)
(* Module to create patterns that will match all overriding methods in the pattern *)
module OverridesMatcher = struct
  let load_matcher patterns is_subtype proc_name =
    let is_matching = function
      | Method_pattern (language, mp) ->
          is_subtype mp.class_name
          && Option.value_map ~f:(match_method language proc_name) ~default:false mp.method_name
      | _ ->
          L.(die UserError) "Expecting method pattern"
    in
    List.exists ~f:is_matching patterns
end

let patterns_of_json_with_key (json_key, json) =
  let default_method_pattern = {class_name= ""; method_name= None} in
  let default_source_contains = "" in
  let language_of_string s =
    match Language.of_string s with
    | Some Language.Java ->
        Ok Language.Java
    | _ ->
        Error ("JSON key " ^ json_key ^ " not supported for language " ^ s)
  in
  let rec detect_language = function
    | [] ->
        Error ("No language found for " ^ json_key)
    | ("language", `String s) :: _ ->
        language_of_string s
    | _ :: tl ->
        detect_language tl
  in
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
          | _ :: tl ->
              loop tl
        in
        loop assoc
    | Error _ as error ->
        error
  in
  (* Translate a JSON entry into a matching pattern *)
  let create_pattern (assoc : (string * Yojson.Basic.json) list) =
    let create_method_pattern assoc =
      let loop mp = function
        | key, `String s when String.equal key "class" ->
            {mp with class_name= s}
        | key, `String s when String.equal key "method" ->
            {mp with method_name= Some s}
        | key, _ when String.equal key "language" ->
            mp
        | _ ->
            L.(die UserError) "Failed to parse %s" (Yojson.Basic.to_string (`Assoc assoc))
      in
      List.fold ~f:loop ~init:default_method_pattern assoc
    and create_string_contains assoc =
      let loop sc = function
        | key, `String pattern when String.equal key "source_contains" ->
            pattern
        | key, _ when String.equal key "language" ->
            sc
        | _ ->
            L.(die UserError) "Failed to parse %s" (Yojson.Basic.to_string (`Assoc assoc))
      in
      List.fold ~f:loop ~init:default_source_contains assoc
    in
    match detect_pattern assoc with
    | Ok (Method_pattern (language, _)) ->
        Ok (Method_pattern (language, create_method_pattern assoc))
    | Ok (Source_contains (language, _)) ->
        Ok (Source_contains (language, create_string_contains assoc))
    | Error _ as error ->
        error
  in
  let warn_user msg = CLOpt.warnf "WARNING: error parsing option %s@\n%s@." json_key msg in
  (* Translate all the JSON entries into matching patterns *)
  let rec translate accu = function
    | `Assoc l -> (
      match create_pattern l with
      | Ok pattern ->
          pattern :: accu
      | Error msg ->
          warn_user msg ; accu )
    | `List l ->
        List.fold ~f:translate ~init:accu l
    | json ->
        warn_user
          (Printf.sprintf "expected list or assoc json type, but got value %s"
             (Yojson.Basic.to_string json)) ;
        accu
  in
  translate [] json


let modeled_expensive_matcher =
  OverridesMatcher.load_matcher (patterns_of_json_with_key Config.patterns_modeled_expensive)


let never_return_null_matcher =
  FileOrProcMatcher.load_matcher (patterns_of_json_with_key Config.patterns_never_returning_null)


let skip_translation_matcher =
  FileOrProcMatcher.load_matcher (patterns_of_json_with_key Config.patterns_skip_translation)


let skip_implementation_matcher =
  FileOrProcMatcher.load_matcher (patterns_of_json_with_key Config.patterns_skip_implementation)


let load_filters () =
  { whitelist= Config.analysis_path_regex_whitelist
  ; blacklist= Config.analysis_path_regex_blacklist
  ; blacklist_files_containing= Config.analysis_blacklist_files_containing
  ; suppress_errors= Config.analysis_suppress_errors }


let filters_from_inferconfig inferconfig : filters =
  let path_filter =
    let whitelist_filter : path_filter =
      if List.is_empty inferconfig.whitelist then default_path_filter
      else is_matching (List.map ~f:Str.regexp inferconfig.whitelist)
    in
    let blacklist_filter : path_filter =
      is_matching (List.map ~f:Str.regexp inferconfig.blacklist)
    in
    let blacklist_files_containing_filter : path_filter =
      FileContainsStringMatcher.create_matcher inferconfig.blacklist_files_containing
    in
    function
    | source_file ->
        whitelist_filter source_file
        && (not (blacklist_filter source_file))
        && not (blacklist_files_containing_filter source_file)
  in
  let error_filter = function
    | error_name ->
        let error_str = error_name.IssueType.unique_id in
        not (List.exists ~f:(String.equal error_str) inferconfig.suppress_errors)
  in
  {path_filter; error_filter; proc_filter= default_proc_filter}


(* Create filters based on configuration options *)
let create_filters () =
  if not Config.filter_paths then do_not_filter else filters_from_inferconfig (load_filters ())


(** This function loads and list the path that are being filtered by the analyzer. The results are
   of the form: path/to/file.java -> true/false meaning that analysis results will be reported on
   path/to/file.java or not *)
let test () =
  let filters = create_filters () in
  let matches path = filters.path_filter path in
  Sys.getcwd ()
  |> Utils.directory_iter (fun path ->
         if DB.is_source_file path then
           let source_file = SourceFile.from_abs_path path in
           let matching = matches source_file in
           L.result "%s -> %b@." (SourceFile.to_rel_path source_file) matching )
