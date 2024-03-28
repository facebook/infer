(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CLOpt = CommandLineOption
module L = Logging

type path_filter = SourceFile.t -> bool

type error_filter = IssueType.t -> bool

type proc_filter = Procname.t -> bool

type filters = {path_filter: path_filter; error_filter: error_filter; proc_filter: proc_filter}

let default_path_filter : path_filter = function _ -> true

let default_error_filter : error_filter = function _ -> true

let default_proc_filter : proc_filter = function _ -> true

let do_not_filter : filters =
  { path_filter= default_path_filter
  ; error_filter= default_error_filter
  ; proc_filter= default_proc_filter }


type filter_config =
  { allow_list: string list
  ; block_list: string list
  ; block_list_files_containing: string list
  ; suppress_errors: string list }

(** Check if a proc name is matching the name given as string. *)
let match_method proc_name method_name =
  (not (BuiltinDecl.is_declared proc_name))
  && String.equal (Procname.get_method proc_name) method_name


type contains_pattern = {contains: string; not_contains: string option}

(* Module to create matcher based on strings present in the source file *)
module FileContainsStringMatcher = struct
  type matcher = SourceFile.t -> bool

  let default_matcher : matcher = fun _ -> false

  (* check if the file contains the regexp but not regexp_not *)
  let file_contains regexp regexp_not_opt source_file =
    let rec loop regexp file_in =
      try Str.search_forward regexp (In_channel.input_line_exn file_in) 0 >= 0 with
      | Caml.Not_found ->
          loop regexp file_in
      | End_of_file ->
          false
    in
    let path = SourceFile.to_abs_path source_file in
    let contains_regexp = Utils.with_file_in path ~f:(fun file_in -> loop regexp file_in) in
    contains_regexp
    && (* [loop] leaves the read position where it found the match,
          hence we read the file twice to check if it doesn't contain
          regexp_not *)
    Option.value_map regexp_not_opt ~default:true ~f:(fun regexp_not ->
        Utils.with_file_in path ~f:(fun file_in -> not (loop regexp_not file_in)) )


  let create_matcher (s_patterns : contains_pattern list) =
    if List.is_empty s_patterns then default_matcher
    else
      let source_map = ref SourceFile.Map.empty in
      let not_contains_patterns =
        List.exists ~f:(fun {not_contains} -> Option.is_some not_contains) s_patterns
      in
      let disjunctive_regexp =
        Str.regexp (String.concat ~sep:"\\|" (List.map ~f:(fun {contains} -> contains) s_patterns))
      in
      let cond check_regexp =
        if not_contains_patterns then
          List.exists
            ~f:(fun {contains; not_contains} ->
              check_regexp (Str.regexp contains) (Option.map not_contains ~f:Str.regexp) )
            s_patterns
        else check_regexp disjunctive_regexp None
      in
      function
      | source_file ->
          let check_regexp regexp regexp_not_opt =
            match SourceFile.Map.find_opt source_file !source_map with
            | Some result ->
                result
            | None -> (
              try
                let pattern_found = file_contains regexp regexp_not_opt source_file in
                source_map := SourceFile.Map.add source_file pattern_found !source_map ;
                pattern_found
              with Sys_error _ -> false )
          in
          cond check_regexp
end

type method_pattern = {class_name: string; method_name: string option}

type pattern = Method_pattern of method_pattern | Source_pattern of contains_pattern

(* Module to create matcher based on source file names or class names and method names *)
module FileOrProcMatcher = struct
  type matcher = SourceFile.t -> Procname.t -> bool

  let default_matcher : matcher = fun _ _ -> false

  let create_method_matcher m_patterns =
    if List.is_empty m_patterns then default_matcher
    else
      let pattern_map =
        List.fold
          ~f:(fun map pattern ->
            let previous =
              try String.Map.find_exn map pattern.class_name
              with Not_found_s _ | Caml.Not_found -> []
            in
            String.Map.set ~key:pattern.class_name ~data:(pattern :: previous) map )
          ~init:String.Map.empty m_patterns
      in
      let do_java pname_java =
        let class_name = Procname.Java.get_class_name pname_java
        and method_name = Procname.Java.get_method pname_java in
        try
          let class_patterns = String.Map.find_exn pattern_map class_name in
          List.exists
            ~f:(fun p ->
              match p.method_name with None -> true | Some m -> String.equal m method_name )
            class_patterns
        with Not_found_s _ | Caml.Not_found -> false
      in
      fun _ proc_name ->
        match proc_name with Procname.Java pname_java -> do_java pname_java | _ -> false


  let load_matchers patterns =
    let s_patterns, m_patterns =
      let collect (s_patterns, m_patterns) = function
        | Source_pattern s ->
            (RevList.cons s s_patterns, m_patterns)
        | Method_pattern mp ->
            (s_patterns, RevList.cons mp m_patterns)
      in
      List.fold ~f:collect ~init:(RevList.empty, RevList.empty) patterns
    in
    let s_patterns, m_patterns = (RevList.to_list s_patterns, RevList.to_list m_patterns) in
    (FileContainsStringMatcher.create_matcher s_patterns, create_method_matcher m_patterns)


  let matches (s_matcher, m_matcher) source_file proc_name =
    m_matcher source_file proc_name || s_matcher source_file


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
    | Method_pattern mp ->
        Format.fprintf fmt "Method pattern {@\n%a}@\n" pp_method_pattern mp
    | Source_pattern {contains= sc; not_contains= None} ->
        Format.fprintf fmt "Source contains {@\n%a}@\n" pp_source_contains sc
    | Source_pattern {contains= sc; not_contains= Some snc} ->
        Format.fprintf fmt "Source contains {@\n%a} and not contains  {@\n%a} @\n"
          pp_source_contains sc pp_source_contains snc
end

(* of module FileOrProcMatcher *)
(* Module to create patterns that will match all overriding methods in the pattern *)
module OverridesMatcher = struct
  let load_matcher patterns is_subtype proc_name =
    let is_matching = function
      | Method_pattern mp ->
          is_subtype mp.class_name && Option.exists ~f:(match_method proc_name) mp.method_name
      | _ ->
          L.(die UserError) "Expecting method pattern"
    in
    List.exists ~f:is_matching patterns
end

let patterns_of_json_with_key (json_key, json) =
  let default_method_pattern = {class_name= ""; method_name= None} in
  let default_source_contains = "" in
  let default_not_contains = {contains= default_source_contains; not_contains= None} in
  (* Detect the kind of pattern, method pattern or pattern based on the content of the source file.
     Detecting the kind of patterns in a first step makes it easier to parse the parts of the
     pattern in a second step *)
  let detect_pattern assoc =
    let rec loop = function
      | [] ->
          Error ("Unknown pattern for " ^ json_key)
      | (("class" | "method"), _) :: _ ->
          Ok (Method_pattern default_method_pattern)
      | ("source_contains", _) :: _ ->
          Ok (Source_pattern default_not_contains)
      | _ :: tl ->
          loop tl
    in
    loop assoc
  in
  (* Translate a JSON entry into a matching pattern *)
  let create_pattern (assoc : (string * Yojson.Safe.t) list) =
    let deprecated_language_key () =
      CLOpt.warnf "\"language\" in matchers is deprecated and ignored. Offending config: %s@\n"
        (Yojson.Safe.to_string (`Assoc assoc))
    in
    let create_method_pattern assoc =
      let loop mp = function
        | "class", `String s ->
            {mp with class_name= s}
        | "method", `String s ->
            {mp with method_name= Some s}
        | "language", _ ->
            deprecated_language_key () ;
            mp
        | _ ->
            L.die UserError "Failed to parse %s" (Yojson.Safe.to_string (`Assoc assoc))
      in
      List.fold ~f:loop ~init:default_method_pattern assoc
    and create_string_contains assoc =
      let loop cp = function
        | "source_contains", `String pattern ->
            {cp with contains= pattern}
        | "source_not_contains", `String pattern ->
            {cp with not_contains= Some pattern}
        | "language", _ ->
            deprecated_language_key () ;
            cp
        | _ ->
            L.die UserError "Failed to parse here %s" (Yojson.Safe.to_string (`Assoc assoc))
      in
      List.fold ~f:loop ~init:default_not_contains assoc
    in
    match detect_pattern assoc with
    | Ok (Method_pattern _) ->
        Ok (Method_pattern (create_method_pattern assoc))
    | Ok (Source_pattern _) ->
        Ok (Source_pattern (create_string_contains assoc))
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
          warn_user msg ;
          accu )
    | `List l ->
        List.fold ~f:translate ~init:accu l
    | json ->
        warn_user
          (Printf.sprintf "expected list or assoc json type, but got value %s"
             (Yojson.Safe.to_string json) ) ;
        accu
  in
  translate [] json


let modeled_expensive_matcher =
  OverridesMatcher.load_matcher (patterns_of_json_with_key Config.modeled_expensive)


let never_return_null_matcher =
  let matchers =
    FileOrProcMatcher.load_matchers (patterns_of_json_with_key Config.never_returning_null)
  in
  fun proc_name source_file -> FileOrProcMatcher.matches matchers proc_name source_file


let capture_block_list_file_matcher =
  FileOrProcMatcher.load_matchers (patterns_of_json_with_key Config.capture_block_list) |> fst


let load_filters () =
  { allow_list= Config.report_path_regex_allow_list
  ; block_list= Config.report_path_regex_block_list
  ; block_list_files_containing= Config.report_block_list_files_containing
  ; suppress_errors= Config.report_suppress_errors }


let filters_from_inferconfig inferconfig : filters =
  let path_filter =
    let allow_list_filter : path_filter =
      SourceFile.is_matching (List.map ~f:Str.regexp inferconfig.allow_list)
    in
    let block_list_filter : path_filter =
      SourceFile.is_matching (List.map ~f:Str.regexp inferconfig.block_list)
    in
    let block_list_files_containing_filter : path_filter =
      FileContainsStringMatcher.create_matcher
        (List.map
           ~f:(fun s -> {contains= s; not_contains= None})
           inferconfig.block_list_files_containing )
    in
    function
    | source_file ->
        allow_list_filter source_file
        || (not (block_list_filter source_file))
           && not (block_list_files_containing_filter source_file)
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
