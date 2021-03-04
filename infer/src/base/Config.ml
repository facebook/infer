(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Configuration values: either constant, determined at compile time, or set at startup time by
    system calls, environment variables, or command line options *)

module F = Format
module CLOpt = CommandLineOption
module L = Die

type analyzer = Checkers | Linters [@@deriving compare]

let equal_analyzer = [%compare.equal: analyzer]

let string_to_analyzer = [("checkers", Checkers); ("linters", Linters)]

let ml_bucket_symbols =
  [ ("all", `MLeak_all)
  ; ("cf", `MLeak_cf)
  ; ("arc", `MLeak_arc)
  ; ("narc", `MLeak_no_arc)
  ; ("cpp", `MLeak_cpp)
  ; ("unknown_origin", `MLeak_unknown) ]


type os_type = Unix | Win32 | Cygwin

type build_system =
  | BAnt
  | BBuck
  | BClang
  | BGradle
  | BJava
  | BJavac
  | BMake
  | BMvn
  | BNdk
  | BXcode
[@@deriving compare]

type scheduler = File | Restart | SyntacticCallGraph [@@deriving equal]

let equal_build_system = [%compare.equal: build_system]

(* List of ([build system], [executable name]). Several executables may map to the same build
   system. In that case, the first one in the list will be used for printing, eg, in which mode
   infer is running. *)
let build_system_exe_assoc =
  [ (BAnt, "ant")
  ; (BBuck, "buck")
  ; (BGradle, "gradle")
  ; (BGradle, "gradlew")
  ; (BJava, "java")
  ; (BJavac, "javac")
  ; (BClang, "cc")
  ; (BClang, "clang")
  ; (BClang, "gcc")
  ; (BClang, "clang++")
  ; (BClang, "c++")
  ; (BClang, "g++")
  ; (BMake, "make")
  ; (BMake, "configure")
  ; (BMake, "cmake")
  ; (BMake, "waf")
  ; (BMvn, "mvn")
  ; (BMvn, "mvnw")
  ; (BNdk, "ndk-build")
  ; (BXcode, "xcodebuild") ]


let string_of_build_system build_system =
  List.Assoc.find_exn ~equal:equal_build_system build_system_exe_assoc build_system


let build_system_of_exe_name name =
  try List.Assoc.find_exn ~equal:String.equal (List.Assoc.inverse build_system_exe_assoc) name
  with Not_found_s _ | Caml.Not_found ->
    L.(die UserError)
      "Unsupported build command '%s'.@\n\
       If this is an alias for another build system that infer supports, you can use@\n\
       `--force-integration <command>` where <command> is one of the following supported build \
       systems:@\n\
       @[<v2>  %a@]" name
      (Pp.seq ~print_env:Pp.text_break ~sep:"" F.pp_print_string)
      ( List.map ~f:fst build_system_exe_assoc
      |> List.map ~f:string_of_build_system
      |> List.dedup_and_sort ~compare:String.compare )


(** Constant configuration values *)

let anonymous_block_num_sep = "_"

let anonymous_block_prefix = "objc_block"

let assign = "<\"Assign\">"

(** If true, a procedure call succeeds even when there is a bound error this mimics what happens
    with a direct array access where an error is produced and the analysis continues *)
let bound_error_allowed_in_procedure_call = true

let buck_out_gen = "buck-out" ^/ "gen"

let buck_results_dir_name = "infer"

let clang_initializer_prefix = "__infer_globals_initializer_"

let clang_inner_destructor_prefix = "__infer_inner_destructor_"

let default_failure_name = "ASSERTION_FAILURE"

(** Dotty output filename **)
let dotty_frontend_output = "proc_cfgs_frontend.dot"

(** exit code to use for the --fail-on-issue option *)
let fail_on_issue_exit_code = 2

(** If true, treat calls to no-arg getters as idempotent w.r.t non-nullness *)
let idempotent_getters = true

let ivar_attributes = "ivar_attributes"

let java_lambda_marker_infix = "$Lambda$"

let manual_buck = "BUCK OPTIONS"

let manual_buffer_overrun = "BUFFER OVERRUN OPTIONS"

let manual_clang = "CLANG OPTIONS"

let manual_clang_linters = "CLANG LINTERS OPTIONS"

let manual_explore_bugs = "EXPLORE BUGS"

let manual_debug_procedures = "DEBUG PROCEDURES"

let manual_debug_source_files = "DEBUG SOURCE FILES"

let manual_debug_global_tenv = "DEBUG GLOBAL TYPE ENVIRONMENT"

let manual_generic = Cmdliner.Manpage.s_options

let manual_hoisting = "HOISTING OPTIONS"

let manual_internal = "INTERNAL OPTIONS"

let manual_java = "JAVA OPTIONS"

let manual_quandary = "QUANDARY CHECKER OPTIONS"

let manual_racerd = "RACERD CHECKER OPTIONS"

let manual_siof = "SIOF CHECKER OPTIONS"

let max_narrows = 5

(** Maximum number of widens that can be performed before the analysis will intentionally crash.
    Used to guard against divergence in the case that someone has implemented a bad widening
    operator *)
let max_widens = 10000

(** Flag to tune the level of applying the meet operator for preconditions during the footprint
    analysis: 0 = do not use the meet 1 = use the meet to generate new preconditions *)
let meet_level = 1

let nsnotification_center_checker_backend = false

let property_attributes = "property_attributes"

(** If true, sanity-check inferred preconditions against Nullable annotations and report
    inconsistencies *)
let report_nullable_inconsistency = true

(** If true, compact summaries before saving *)
let save_compact_summaries = true

(** If true enables printing proposition compatible for the SMT project *)
let smt_output = false

let source_file_extentions = [".java"; ".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"]

let kotlin_source_extension = ".kt"

(** Enable detailed tracing information during array abstraction *)
let trace_absarray = false

let unsafe_unret = "<\"Unsafe_unretained\">"

let weak = "<\"Weak\">"

(* Whitelists for C++ library functions *)

let std_whitelisted_cpp_methods =
  [ "std::back_inserter"
  ; "std::forward"
  ; "std::front_inserter"
  ; "std::get"
  ; "std::inserter"
  ; "std::make_move_iterator"
  ; "std::make_pair"
  ; "std::max"
  ; "std::min"
  ; "std::move"
  ; "std::operator!="
  ; "std::operator<"
  ; "std::operator<="
  ; "std::operator=="
  ; "std::operator>"
  ; "std::operator>="
  ; "std::swap" ]


let libstdcxx_whitelisted_cpp_methods =
  [ "__gnu_cxx::operator!="
  ; "__gnu_cxx::operator<"
  ; "__gnu_cxx::operator<="
  ; "__gnu_cxx::operator=="
  ; "__gnu_cxx::operator>"
  ; "__gnu_cxx::operator>="
  ; "__gnu_cxx::operator+"
  ; "__gnu_cxx::operator-" ]


let libcxx_whitelisted_cpp_methods = []

let other_whitelisted_cpp_methods = ["google::CheckNotNull"]

let whitelisted_cpp_methods =
  List.concat
    [ std_whitelisted_cpp_methods
    ; libstdcxx_whitelisted_cpp_methods
    ; libcxx_whitelisted_cpp_methods
    ; other_whitelisted_cpp_methods ]


(* Whitelists for C++ library classes *)

let std_whitelisted_cpp_classes =
  [ "std::back_insert_iterator"
  ; "std::equal_to"
  ; "std::front_insert_iterator"
  ; "std::greater"
  ; "std::greater_equal"
  ; "std::insert_iterator"
  ; "std::less"
  ; "std::less_equal"
  ; "std::move_iterator"
  ; "std::not_equal_to"
  ; "std::pair"
  ; "std::reverse_iterator" ]


let libstdcxx_whitelisted_cpp_classes =
  (* libstdc++ internal support class for std::get<std::pair> *)
  [ "__gnu_cxx::__normal_iterator" (* libstdc++ internal name of vector iterator *)
  ; "std::__pair_get" ]


let libcxx_whitelisted_cpp_classes =
  (* libc++ internal support class for std::get<std::pair> *)
  [ "std::__less"
  ; "std::__wrap_iter" (* libc++ internal name of vector iterator *)
  ; "std::__get_pair" ]


let other_whitelisted_cpp_classes = []

let whitelisted_cpp_classes =
  List.concat
    [ std_whitelisted_cpp_classes
    ; libstdcxx_whitelisted_cpp_classes
    ; libcxx_whitelisted_cpp_classes
    ; other_whitelisted_cpp_classes ]


let pp_version fmt () =
  F.fprintf fmt "Infer version %s@\nCopyright 2009 - present Facebook. All Rights Reserved."
    Version.versionString


let version_string = F.asprintf "%a" pp_version ()

(** System call configuration values *)

(** Initial time of the analysis, i.e. when this module is loaded, gotten from Unix.time *)
let initial_analysis_time = Unix.time ()

let clang_exe_aliases =
  [ (* this must be kept in sync with the clang-like symlinks in [wrappers_dir] (see below) *)
    "c++"
  ; "cc"
  ; "clang"
  ; "clang++"
  ; "g++"
  ; "gcc" ]


let exe_basename =
  (* Sys.executable_name tries to do clever things which we must avoid, use argv[0] instead *)
  Filename.basename (Sys.get_argv ()).(0)


let infer_is_clang = List.mem ~equal:String.equal clang_exe_aliases exe_basename

let initial_command =
  match InferCommand.of_exe_name exe_basename with Some _ as command -> command | None -> None


let bin_dir =
  (* Resolve symlinks to get to the real executable, which is located in [bin_dir]. *)
  Filename.dirname (Utils.realpath Sys.executable_name)


let fcp_dir =
  bin_dir ^/ Filename.parent_dir_name ^/ Filename.parent_dir_name ^/ "facebook-clang-plugins"


let clang_plugin_path = fcp_dir ^/ "libtooling" ^/ "build" ^/ "FacebookClangPlugin.dylib"

let lib_dir = bin_dir ^/ Filename.parent_dir_name ^/ "lib"

let etc_dir = bin_dir ^/ Filename.parent_dir_name ^/ "etc"

(** Path to the database dump with model summaries *)
let biabduction_models_sql = lib_dir ^/ "models.sql"

let biabduction_models_jar = lib_dir ^/ "java" ^/ "models.jar"

(* Normalize the path *)

let linters_def_dir = lib_dir ^/ "linter_rules"

let linters_def_default_file = linters_def_dir ^/ "linters.al"

let wrappers_dir = lib_dir ^/ "wrappers"

let ncpu = Utils.numcores

let os_type = match Sys.os_type with "Win32" -> Win32 | "Cygwin" -> Cygwin | _ -> Unix

(** Resolve relative paths passed as command line options, i.e., with respect to the working
    directory of the initial invocation of infer. *)
let resolve = Utils.filename_to_absolute ~root:CLOpt.init_work_dir

let infer_top_results_dir_env_var = "INFER_TOP_RESULTS_DIR"

let infer_inside_maven_env_var = "INFER_INSIDE_MAVEN"

let maven = CLOpt.is_env_var_set infer_inside_maven_env_var

let env_inside_maven = `Extend [(infer_inside_maven_env_var, "1")]

let infer_is_javac = maven

let locate_sdk_root () =
  match Version.build_platform with
  | Darwin -> (
      let cmd = "xcrun --show-sdk-path --sdk macosx 2> /dev/null" in
      try
        let path, _ = Utils.with_process_in cmd In_channel.input_line in
        path
      with Unix.Unix_error _ -> None )
  | _ ->
      None


let infer_sdkroot_env_var = "INFER_SDKROOT"

(** Try to locate current SDK root on MacOS *unless* [SDKROOT] is explicitly provided. The implicit
    SDK root is propagated to child processes using a custom [INFER_SDKROOT] env var. The reason for
    this is twofold:

    1. With make and buck integrations infer is exec'ed by make/buck for each source file. That's
    why we propagate the value by using an env var instead of calling [locate_sdk_root] each time.

    2. We don't use [SDKROOT] because it can mess up with other parts of the toolchain not owned by
    infer. *)
let implicit_sdk_root =
  match Sys.getenv "SDKROOT" with
  | Some _ ->
      None
  | None -> (
    match Sys.getenv infer_sdkroot_env_var with
    | Some _ as path ->
        path
    | None ->
        let maybe_root = locate_sdk_root () in
        let putenv x = Unix.putenv ~key:infer_sdkroot_env_var ~data:x in
        Option.iter ~f:putenv maybe_root ;
        maybe_root )


let startup_action =
  let open CLOpt in
  if infer_is_javac then Javac
  else if
    !Sys.interactive
    || String.is_substring ~substring:"inline_test_runner" exe_basename
    || String.is_substring ~substring:"inferunit" exe_basename
    || String.equal "run.exe" exe_basename
    || String.equal "run.bc" exe_basename
  then NoParse
  else if infer_is_clang then NoParse
  else InferCommand


let exe_usage =
  let exe_command_name =
    match initial_command with
    | Some command ->
        Some (InferCommand.to_string command)
    | None ->
        None
  in
  Printf.sprintf "%s\nUsage: infer %s [options]\nSee `infer%s --help` for more information."
    version_string
    (Option.value ~default:"command" exe_command_name)
    (Option.value_map ~default:"" ~f:(( ^ ) " ") exe_command_name)


let get_symbol_string json_obj =
  match json_obj with
  | `String sym_regexp_str ->
      sym_regexp_str
  | _ ->
      L.(die UserError) "each --custom-symbols element should be list of symbol *strings*"


let get_symbols_regexp json_obj =
  let sym_regexp_strs =
    match json_obj with
    | `List json_objs ->
        List.map ~f:get_symbol_string json_objs
    | _ ->
        L.(die UserError) "each --custom-symbols element should be a *list* of strings"
  in
  Str.regexp ("\\(" ^ String.concat ~sep:"\\|" sym_regexp_strs ^ "\\)")


(** Command Line options *)

(* HOWTO define a new command line and config file option.

   1. Add an entry in the following let...and...and... binding.  See the documentation in
      [CommandLineOption.mli], and use the existing options as a guide.  Preferably the identifer
      and long option name are the same modulo underscores versus hyphens.

      E.g. [and new_option = CLOpt.mk_bool ~long:"new-option"]

   2. Add a line to the [Freeze initialized configuration values] section below.

      E.g. [and new_option = !new_option]

   3. Add a line to [config.mli].

      E.g. [val new_option : bool]

   These are all in alphabetical order as much as possible.

   The references representing the command line options are defined in a single
   simultaneous let...and...and... binding in order to allow the type-checker to catch
   uses of one reference in code for another. This avoids being sensitive to
   initialization-order and unintended dependence on the order in which options appear on
   the command line. For cases where order-dependence is desired, the interacting options
   can be defined together sharing a reference. See debug and specs_library below for two
   different examples. *)

let all_checkers = ref []

let disable_all_checkers () = List.iter !all_checkers ~f:(fun (_, _, var) -> var := false)

let enable_checker c =
  List.iter !all_checkers ~f:(fun (checker, _, var) -> if Checker.equal checker c then var := true)


let () =
  let on_unknown_arg_from_command (cmd : InferCommand.t) =
    match cmd with
    | Report ->
        `Add
    | Analyze | AnalyzeJson | Capture | Compile | Debug | Explore | Help | ReportDiff | Run ->
        `Reject
  in
  (* make sure we generate doc for all the commands we know about *)
  List.iter InferCommand.all_commands ~f:(fun cmd ->
      let {CommandDoc.name; command_doc} = CommandDoc.data_of_command cmd in
      let on_unknown_arg = on_unknown_arg_from_command cmd in
      let deprecated_long = if InferCommand.(equal ReportDiff) cmd then Some "diff" else None in
      CLOpt.mk_subcommand cmd ~name ?deprecated_long ~on_unknown_arg (Some command_doc) )


let abs_struct =
  CLOpt.mk_int ~deprecated:["absstruct"] ~long:"abs-struct" ~default:1 ~meta:"int"
    {|Specify abstraction level for fields of structs:
- 0 = no
- 1 = forget some fields during matching (and so lseg abstraction)
|}


and abs_val =
  CLOpt.mk_int ~deprecated:["absval"] ~long:"abs-val" ~default:2 ~meta:"int"
    {|Specify abstraction level for expressions:
- 0 = no abstraction
- 1 = evaluate all expressions abstractly
- 2 = 1 + abstract constant integer values during join
|}


and allow_leak =
  CLOpt.mk_bool ~deprecated:["leak"] ~long:"allow-leak" "Forget leaked memory during abstraction"


and analyzer =
  CLOpt.mk_symbol ~deprecated:["analyzer"; "-analyzer"; "a"] ~long:"" ~default:Checkers
    ~eq:equal_analyzer ~symbols:string_to_analyzer
    "DEPRECATED: To enable and disable individual analyses, use the various checkers options. For \
     instance, to enable only the biabduction analysis, run with $(b,--biabduction-only)."


(* checkers *)
and () =
  let open Checker in
  let in_analyze_help = InferCommand.[(Analyze, manual_generic)] in
  let mk_checker ?f checker =
    let config = Checker.config checker in
    let var =
      match config.cli_flags with
      | None ->
          (* HACK: return a constant ref if the checker cannot be enabled/disabled from the command line *)
          ref config.enabled_by_default
      | Some {deprecated; show_in_help} ->
          let in_help = if show_in_help then in_analyze_help else [] in
          CLOpt.mk_bool ?f ~long:config.id ~in_help ~default:config.enabled_by_default ~deprecated
            (Printf.sprintf "checker %s: %s" config.id config.short_documentation)
    in
    all_checkers := (checker, config, var) :: !all_checkers
  in
  List.iter Checker.all ~f:mk_checker ;
  let mk_only (_checker, config, var) =
    Option.iter config.cli_flags ~f:(fun {show_in_help} ->
        let (_ : bool ref) =
          CLOpt.mk_bool_group ~long:(config.id ^ "-only")
            ~in_help:InferCommand.[(Analyze, manual_generic)]
            ~f:(fun b ->
              disable_all_checkers () ;
              var := b ;
              b )
            ( if show_in_help then
              Printf.sprintf "Enable %s and disable all other checkers" config.id
            else "" )
            [] (* do all the work in ~f *) []
          (* do all the work in ~f *)
        in
        () )
  in
  List.iter ~f:mk_only !all_checkers ;
  let _default_checkers : bool ref =
    CLOpt.mk_bool_group ~long:"default-checkers"
      ~in_help:InferCommand.[(Analyze, manual_generic)]
      ~default:true
      ( "Default checkers: "
      ^ ( List.rev_filter_map
            ~f:(fun (_, config, _) ->
              match config.cli_flags with
              | Some _ when config.enabled_by_default ->
                  Some (Printf.sprintf "$(b,--%s)" config.id)
              | _ ->
                  None )
            !all_checkers
        |> String.concat ~sep:", " ) )
      ~f:(fun b ->
        List.iter
          ~f:(fun (_, config, var) ->
            var :=
              if b then config.enabled_by_default || !var
              else (not config.enabled_by_default) && !var )
          !all_checkers ;
        b )
      [] (* do all the work in ~f *) []
    (* do all the work in ~f *)
  in
  ()


and annotation_reachability_cxx =
  CLOpt.mk_json ~long:"annotation-reachability-cxx"
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    ( "Specify annotation reachability analyses to be performed on C/C++/ObjC code. Each entry is \
       a JSON object whose key is the issue name. \"sources\" and \"sinks\" can be specified \
       either by symbol (including regexps) or path prefix.  \"sinks\" optionally can specify \
       \"overrides\" (by symbol or path prefix) that block the reachability analysis when hit.  \
       Example:\n"
    ^ {|{
    "ISOLATED_REACHING_CONNECT": {
      "doc_url": "http:://example.com/issue/doc/optional_link.html",
      "sources": {
        "desc": "Code that should not call connect [optional]",
        "paths": [ "isolated/" ]
      },
      "sinks": {
        "symbols": [ "connect" ],
        "overrides": { "symbol_regexps": [ ".*::Trusted::.*" ] }
      }
    }
  }
|}
    ^ "\n\
       This will cause us to create a new ISOLATED_REACHING_CONNECT issue for every function whose \
       source path starts with \"isolated/\" that may reach the function named \"connect\", \
       ignoring paths that go through a symbol matching the OCaml regexp \".*::Trusted::.*\"." )


and annotation_reachability_cxx_sources =
  CLOpt.mk_json ~long:"annotation-reachability-cxx-sources"
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    {|Override sources in all cxx annotation reachability specs with the given sources spec|}


and annotation_reachability_custom_pairs =
  CLOpt.mk_json ~long:"annotation-reachability-custom-pairs"
    ~in_help:InferCommand.[(Analyze, manual_java)]
    {|Specify custom sources/sink for the annotation reachability checker
Example format: for custom annotations com.my.annotation.{Source1,Source2,Sink1}
{ "sources" : ["Source1", "Source2"], "sink" : "Sink1" }|}


and append_buck_flavors =
  CLOpt.mk_string_list ~long:"append-buck-flavors"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Additional Buck flavors to append to targets discovered by the \
     $(b,--buck-compilation-database) option."


and array_level =
  CLOpt.mk_int ~deprecated:["arraylevel"] ~long:"array-level" ~default:0 ~meta:"int"
    {|Level of treating the array indexing and pointer arithmetic:
- 0 = treats both features soundly
- 1 = assumes that the size of every array is infinite
- 2 = assumes that all heap dereferences via array indexing and pointer arithmetic are correct
|}


and biabduction_models_mode =
  CLOpt.mk_bool ~long:"biabduction-models-mode" "Analysis of the biabduction models"


and bo_field_depth_limit =
  CLOpt.mk_int_opt ~long:"bo-field-depth-limit"
    ~in_help:InferCommand.[(Analyze, manual_buffer_overrun)]
    "Limit of field depth of abstract location in buffer-overrun checker"


and bootclasspath =
  CLOpt.mk_string_opt ~long:"bootclasspath"
    ~in_help:InferCommand.[(Capture, manual_java)]
    "Specify the Java bootclasspath"


(** Automatically set when running from within Buck *)
and buck = CLOpt.mk_bool ~long:"buck" ""

and buck_blacklist =
  CLOpt.mk_string_list
    ~deprecated:["-blacklist-regex"; "-blacklist"]
    ~long:"buck-blacklist"
    ~in_help:InferCommand.[(Run, manual_buck); (Capture, manual_buck)]
    ~meta:"regex"
    "Skip capture of files matched by the specified regular expression. Only the clang, \
     non-compilation-database\n\
    \     Buck integration is supported, not Java."


and buck_build_args =
  CLOpt.mk_string_list ~long:"Xbuck"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Pass values as command-line arguments to invocations of $(i,`buck build`). Only valid for \
     $(b,--buck-clang)."


and buck_build_args_no_inline_rev =
  CLOpt.mk_string_list ~long:"Xbuck-no-inline"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Pass values as command-line arguments to invocations of $(i,`buck build`), don't inline any \
     args starting with '@'. Only valid for $(b,--buck-clang)."


and buck_compilation_database_depth =
  CLOpt.mk_int_opt ~long:"buck-compilation-database-depth"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Depth of dependencies used by the $(b,--buck-compilation-database deps) option. By default, \
     all recursive dependencies are captured."
    ~meta:"int"


and buck_java_heap_size_gb =
  CLOpt.mk_int_opt ~long:"buck-java-heap-size-gb"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Explicitly set the size of the Java heap of Buck processes, in gigabytes." ~meta:"int"


and buck_java_flavor_suppress_config =
  CLOpt.mk_bool ~long:"buck-java-flavor-suppress-config" ~default:false
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Suppress setting buck config values for the infer binary and its version in the \
     buck-java-flavor integration."


and buck_merge_all_deps =
  CLOpt.mk_bool ~long:"buck-merge-all-deps" ~default:false
    ~in_help:InferCommand.[(Capture, manual_buck)]
    "Find and merge all infer dependencies produced by buck. Use this flag if infer doesn't find \
     any files to analyze after a successful capture. Only valid for $(b,--buck-clang)."


and buck_mode =
  let buck_mode = ref `None in
  let set_mode mode b =
    if b then buck_mode := mode else buck_mode := `None ;
    b
  in
  CLOpt.mk_bool ~deprecated:["-flavors"; "-use-flavors"] ~long:"buck-clang"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    ~f:(set_mode `ClangFlavors)
    "Buck integration for clang-based targets (C/C++/Objective-C/Objective-C++)."
  |> ignore ;
  CLOpt.mk_symbol_opt ~long:"buck-compilation-database" ~deprecated:["-use-compilation-database"]
    ~in_help:InferCommand.[(Capture, manual_buck)]
    ~f:(fun s ->
      buck_mode := `ClangCompilationDB s ;
      s )
    "Buck integration using the compilation database, with or without dependencies. Only includes \
     clang targets, as per Buck's $(i,#compilation-database) flavor."
    ~symbols:[("no-deps", `NoDeps); ("deps", `DepsTmp)]
  |> ignore ;
  CLOpt.mk_bool ~long:"buck-java-flavor"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    ~f:(set_mode `JavaFlavor)
    "Buck integration for Java which uses the buck flavor #infer-java-capture instead of genrules \
     like buck-java."
  |> ignore ;
  buck_mode


and _buck_out =
  CLOpt.mk_path_opt ~deprecated:["-buck-out"] ~long:"" ~meta:"dir"
    "[DOES NOTHING] Specify the root directory of buck-out. Only valid for $(b,--buck-java)."


and buck_targets_blacklist =
  CLOpt.mk_string_list ~long:"buck-targets-blacklist"
    ~in_help:InferCommand.[(Run, manual_buck); (Capture, manual_buck)]
    ~meta:"regex"
    "Skip capture of buck targets matched by the specified regular expression. Only valid for \
     $(b,--buck-compilation-database)."


and call_graph_schedule =
  CLOpt.mk_bool ~long:"call-graph-schedule" ~default:false "use call graph for scheduling analysis"


and capture =
  CLOpt.mk_bool ~long:"capture" ~default:true
    "capture and translate source files into infer's intermediate language for analysis"


and capture_blacklist =
  CLOpt.mk_string_opt ~long:"capture-blacklist"
    ~in_help:InferCommand.[(Run, manual_java); (Capture, manual_java)]
    ~meta:"regex"
    "Skip capture of files matched by the specified OCaml regular expression (only supported by \
     the javac integration for now)."


and cfg_json =
  CLOpt.mk_path_opt ~long:"cfg-json"
    ~in_help:InferCommand.[(AnalyzeJson, manual_generic)]
    ~meta:"file" "Path to CFG json file"


and censor_report =
  CLOpt.mk_string_list ~long:"censor-report" ~deprecated:["-filter-report"]
    ~in_help:InferCommand.[(Report, manual_generic); (Run, manual_generic)]
    "Specify a filter for issues to be censored by adding a 'censored_reason' field in the json \
     report. Infer will not report censored issues on the console output and in report.txt, but \
     tools that post-process the json report can take them into account. If multiple filters are \
     specified, they are applied in the order in which they are specified. Each filter is applied \
     to each issue detected, and only issues which are accepted by all filters are reported. Each \
     filter is of the form: `<issue_type_regex>:<filename_regex>:<reason_string>`. The first two \
     components are OCaml Str regular expressions, with an optional `!` character prefix. If a \
     regex has a `!` prefix, the polarity is inverted, and the filter becomes a \"blacklist\" \
     instead of a \"whitelist\". Each filter is interpreted as an implication: an issue matches if \
     it does not match the `issue_type_regex` or if it does match the `filename_regex`. The \
     filenames that are tested by the regex are relative to the `--project-root` directory. The \
     `<reason_string>` is a non-empty string used to explain why the issue was filtered."


and changed_files_index =
  CLOpt.mk_path_opt ~long:"changed-files-index"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    ~meta:"file"
    "Specify the file containing the list of source files from which reactive analysis should \
     start. Source files should be specified relative to project root or be absolute"


and check_version =
  CLOpt.mk_string_opt ~long:"check-version" ~meta:"version"
    "Verify that the Infer version is equal to the provided argument"


and clang_biniou_file =
  CLOpt.mk_path_opt ~long:"clang-biniou-file"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    ~meta:"file"
    "Specify a file containing the AST of the program, in biniou format. Please note you still \
     need to provide a compilation command."


and clang_compound_literal_init_limit =
  CLOpt.mk_int ~default:5 ~long:"clang-compound-literal-init-limit"
    ~in_help:InferCommand.[(Analyze, manual_clang); (Capture, manual_clang)]
    "Limit after which initialization of compound types (structs and arrays) is not done element \
     by element but using a builtin function that each analysis has to model."


and clang_extra_flags =
  CLOpt.mk_string_list ~long:"Xclang"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Pass values as command-line arguments to invocations of clang"


and clang_blacklisted_flags =
  CLOpt.mk_string_list ~long:"clang-blacklisted-flags"
    ~default:
      [ "--expt-relaxed-constexpr"
      ; "-fembed-bitcode-marker"
      ; "-fno-absolute-module-directory"
      ; "-fno-canonical-system-headers" ]
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Clang flags to filter out"


and clang_blacklisted_flags_with_arg =
  CLOpt.mk_string_list ~long:"clang-blacklisted-flags-with-arg"
    ~default:["-index-store-path"; "-mllvm"]
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Clang flags (taking args) to filter out"


and clang_compilation_dbs = ref []

and clang_ignore_regex =
  CLOpt.mk_string_opt ~long:"clang-ignore-regex" ~meta:"dir_OCaml_regex"
    "The files in this regex will be ignored in the compilation process and an empty file will be \
     passed to clang instead. This is to be used with the buck flavour infer-capture-all to work \
     around missing generated files."


and clang_idirafter_to_override_regex =
  CLOpt.mk_string_opt ~long:"clang-idirafter-to-override-regex" ~meta:"dir_OCaml_regex"
    "Use this option in the uncommon case where the normal compilation process overrides the \
     location of internal compiler headers. This option should specify regular expression with the \
     path to those headers so that infer can use its own clang internal headers instead. \
     Concretely, this will replace $(b,-idirafter <path matching the regex>) with $(b,-idirafter \
     /path/to/infer/facebook-clang-plugins/clang/install/lib/clang/<version>/include)."


and clang_isystem_to_override_regex =
  CLOpt.mk_string_opt ~long:"clang-isystem-to-override-regex"
    ~deprecated:["-clang-include-to-override-regex"; "-clang-include-to-override"]
    ~meta:"dir_OCaml_regex"
    "Use this option in the uncommon case where the normal compilation process overrides the \
     location of internal compiler headers. This option should specify regular expression with the \
     path to those headers so that infer can use its own clang internal headers instead. \
     Concretely, this will replace $(b,-isystem <path matching the regex>) with $(b,-isystem \
     /path/to/infer/facebook-clang-plugins/clang/install/lib/clang/<version>/include)."


and clang_libcxx_include_to_override_regex =
  CLOpt.mk_string_opt ~long:"clang-libcxx-include-to-override-regex" ~meta:"dir_OCaml_regex"
    "Use this option in the uncommon case where the normal compilation process overrides the \
     location of libc++. Concretely, this will replace $(b,-I <path matching the regex>) with \
     $(b,-I /path/to/infer/facebook-clang-plugins/clang/install/include/c++/v1)."


and clang_yojson_file =
  CLOpt.mk_path_opt ~long:"clang-yojson-file"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    ~meta:"file"
    "Specify a file containing the AST of the program, in yojson format. Please note you still \
     need to provide a compilation command."


and classpath = CLOpt.mk_string_opt ~long:"classpath" "Specify the Java classpath"

and compilation_database =
  CLOpt.mk_path_list ~long:"compilation-database" ~deprecated:["-clang-compilation-db-files"]
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "File that contain compilation commands (can be specified multiple times)"


and compilation_database_escaped =
  CLOpt.mk_path_list ~long:"compilation-database-escaped"
    ~deprecated:["-clang-compilation-db-files-escaped"]
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "File that contain compilation commands where all entries are escaped for the shell, eg coming \
     from Xcode (can be specified multiple times)"


and config_impact_current =
  CLOpt.mk_path_opt ~long:"config-impact-current"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Config impact report of the latest revision"


and config_impact_data_file =
  CLOpt.mk_path_opt ~long:"config-impact-data-file"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"file" "[ConfigImpact] Specify the file containing the config data"


and config_impact_issues_tests =
  CLOpt.mk_path_opt ~long:"config-impact-issues-tests"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"file"
    "Write a list of config impact issues in a format suitable for config impact tests to $(i,file)"


and config_impact_max_callees_to_print =
  CLOpt.mk_int ~long:"config-impact-max-callees-to-print" ~default:3
    ~in_help:InferCommand.[(Report, manual_generic); (ReportDiff, manual_generic)]
    ~meta:"int"
    "Specify the maximum number of unchecked callees to print in the config impact checker"


and config_impact_previous =
  CLOpt.mk_path_opt ~long:"config-impact-previous"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Config impact report of the base revision to use for comparison"


(** Continue the capture for reactive mode: If a procedure was changed beforehand, keep the changed
    marking. *)
and continue =
  CLOpt.mk_bool ~deprecated:["continue"] ~long:"continue"
    ~in_help:InferCommand.[(Capture, manual_generic)]
    "Continue the capture for the reactive analysis, increasing the changed files/procedures. (If \
     a procedure was changed beforehand, keep the changed marking.)"


and continue_analysis =
  CLOpt.mk_bool ~long:"continue-analysis"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Continue the analysis after more targets are captured by $(b,--continue). The other analysis \
     options should be given the same before. Not compatible with $(b,--reanalyze) and \
     $(b,--incremental-analysis)."


and costs_current =
  CLOpt.mk_path_opt ~long:"costs-current"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Costs report of the latest revision"


and cost_issues_tests =
  CLOpt.mk_path_opt ~long:"cost-issues-tests"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"file" "Write a list of cost issues in a format suitable for cost tests to $(i,file)"


and costs_previous =
  CLOpt.mk_path_opt ~long:"costs-previous"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Costs report of the base revision to use for comparison"


and cost_suppress_func_ptr =
  CLOpt.mk_bool ~default:true ~long:"cost-suppress-func-ptr"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Suppress printing function pointers in cost reports"


and cost_tests_only_autoreleasepool =
  CLOpt.mk_bool ~long:"cost-tests-only-autoreleasepool"
    ~in_help:InferCommand.[(Report, manual_generic); (ReportDiff, manual_generic)]
    "[EXPERIMENTAL] Report only autoreleasepool size results in cost tests"


and siof_check_iostreams =
  CLOpt.mk_bool ~long:"siof-check-iostreams"
    ~in_help:InferCommand.[(Analyze, manual_siof)]
    "Do not assume that iostreams (cout, cerr, ...) are always initialized. The default is to \
     assume they are always initialized to avoid false positives. However, if your program \
     compiles against a recent libstdc++ then it is safe to turn this option on."


and cxx_scope_guards =
  CLOpt.mk_json ~long:"cxx-scope-guards"
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    "Specify scope guard classes that can be read only by destructors without being reported as \
     dead stores."


and cxx =
  CLOpt.mk_bool ~long:"cxx" ~default:true
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Analyze C++ methods"


and custom_symbols =
  CLOpt.mk_json ~long:"custom-symbols"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Specify named lists of symbols available to rules"


and ( bo_debug
    , deduplicate
    , developer_mode
    , debug
    , debug_exceptions
    , debug_level_analysis
    , debug_level_capture
    , debug_level_linters
    , debug_level_test_determinator
    , default_linters
    , filtering
    , frontend_tests
    , keep_going
    , linters_developer_mode
    , only_cheap_debug
    , print_buckets
    , print_jbir
    , print_logs
    , print_types
    , reports_include_ml_loc
    , trace_error
    , write_html
    , write_dotty ) =
  let all_generic_manuals =
    List.filter_map InferCommand.all_commands ~f:(fun (command : InferCommand.t) ->
        match command with
        | Debug | Explore | Help ->
            None
        | (Analyze | AnalyzeJson | Capture | Compile | Report | ReportDiff | Run) as command ->
            Some (command, manual_generic) )
  in
  let bo_debug =
    CLOpt.mk_int ~default:0 ~long:"bo-debug"
      ~in_help:InferCommand.[(Analyze, manual_buffer_overrun)]
      "Debug level for buffer-overrun checker (0-4)"
  and deduplicate =
    CLOpt.mk_bool ~long:"deduplicate" ~default:true
      ~in_help:
        InferCommand.
          [(Analyze, manual_generic); (Report, manual_generic); (ReportDiff, manual_generic)]
      "Apply issue-specific deduplication during analysis and/or reporting."
  and debug_level_analysis =
    CLOpt.mk_int ~long:"debug-level-analysis" ~default:0 ~in_help:all_generic_manuals
      "Debug level for the analysis. See $(b,--debug-level) for accepted values."
  and debug_level_capture =
    CLOpt.mk_int ~long:"debug-level-capture" ~default:0 ~in_help:all_generic_manuals
      "Debug level for the capture. See $(b,--debug-level) for accepted values."
  and debug_level_linters =
    CLOpt.mk_int ~long:"debug-level-linters" ~default:0
      ~in_help:(InferCommand.(Capture, manual_clang_linters) :: all_generic_manuals)
      "Debug level for the linters. See $(b,--debug-level) for accepted values."
  and debug_level_test_determinator =
    CLOpt.mk_int ~long:"debug-level-test-determinator" ~default:0
      "Debug level for the test determinator. See $(b,--debug-level) for accepted values."
  and developer_mode =
    CLOpt.mk_bool ~long:"developer-mode"
      ~default:(Option.value_map ~default:false ~f:InferCommand.(equal Report) initial_command)
      "Show internal exceptions"
  and filtering =
    CLOpt.mk_bool ~deprecated_no:["nf"] ~long:"filtering" ~short:'f' ~default:true
      ~in_help:InferCommand.[(Report, manual_generic)]
      "Do not show the experimental and blacklisted issue types"
  and only_cheap_debug =
    CLOpt.mk_bool ~long:"only-cheap-debug" ~default:true "Disable expensive debugging output"
  and print_buckets =
    CLOpt.mk_bool ~long:"print-buckets"
      "Show the internal bucket of Infer reports in their textual description"
  and print_jbir =
    CLOpt.mk_bool ~long:"print-jbir" "Print JBir translation of Java bytecode in logs"
  and print_types = CLOpt.mk_bool ~long:"print-types" ~default:false "Print types in symbolic heaps"
  and keep_going =
    CLOpt.mk_bool ~deprecated_no:["-no-failures-allowed"] ~long:"keep-going"
      ~in_help:InferCommand.[(Analyze, manual_generic)]
      "Keep going when the analysis encounters a failure"
  and reports_include_ml_loc =
    CLOpt.mk_bool ~deprecated:["with_infer_src_loc"] ~long:"reports-include-ml-loc"
      "Include the location in the Infer source code from where reports are generated"
  and trace_error =
    CLOpt.mk_bool ~long:"trace-error" "Detailed tracing information during error explanation"
  and write_html =
    CLOpt.mk_bool ~long:"write-html" "Produce html debug output in the results directory"
  and write_dotty =
    CLOpt.mk_bool ~long:"write-dotty" "Produce dotty files for specs in the results directory"
  in
  let set_debug_level level =
    bo_debug := level ;
    debug_level_analysis := level ;
    debug_level_capture := level ;
    debug_level_linters := level ;
    debug_level_test_determinator := level
  in
  let debug =
    CLOpt.mk_bool_group ~deprecated:["debug"; "-stats"] ~long:"debug" ~short:'g'
      ~in_help:all_generic_manuals
      "Debug mode (also sets $(b,--debug-level 2), $(b,--developer-mode), $(b,--print-buckets), \
       $(b,--print-types), $(b,--reports-include-ml-loc), $(b,--no-only-cheap-debug), \
       $(b,--trace-error), $(b,--write-dotty), $(b,--write-html))"
      ~f:(fun debug ->
        if debug then set_debug_level 2 else set_debug_level 0 ;
        CommandLineOption.keep_args_file := debug ;
        debug )
      [ developer_mode
      ; print_buckets
      ; print_types
      ; reports_include_ml_loc
      ; trace_error
      ; write_html
      ; write_dotty ]
      [only_cheap_debug]
  and (_ : int option ref) =
    CLOpt.mk_int_opt ~long:"debug-level" ~in_help:all_generic_manuals ~meta:"level"
      ~f:(fun level ->
        set_debug_level level ;
        level )
      {|Debug level (sets $(b,--bo-debug) $(i,level), $(b,--debug-level-analysis) $(i,level), $(b,--debug-level-capture) $(i,level), $(b,--debug-level-linters) $(i,level)):
  - 0: only basic debugging enabled
  - 1: verbose debugging enabled
  - 2: very verbose debugging enabled|}
  and debug_exceptions =
    CLOpt.mk_bool_group ~long:"debug-exceptions"
      "Generate lightweight debugging information: just print the internal exceptions during \
       analysis (also sets $(b,--developer-mode), $(b,--no-filtering), $(b,--no-deduplicate), \
       $(b,--print-buckets), $(b,--reports-include-ml-loc))"
      [developer_mode; print_buckets; reports_include_ml_loc]
      [filtering; keep_going; deduplicate]
  and default_linters =
    CLOpt.mk_bool ~long:"default-linters"
      ~in_help:InferCommand.[(Capture, manual_clang_linters)]
      ~default:true "Use the default linters for the analysis."
  and frontend_tests =
    CLOpt.mk_bool_group ~long:"frontend-tests"
      ~in_help:InferCommand.[(Capture, manual_clang)]
      "Save filename.ext.test.dot with the cfg in dotty format for frontend tests (also sets \
       $(b,--print-types))"
      [print_types] []
  and print_logs =
    CLOpt.mk_bool ~long:"print-logs"
      ~in_help:
        InferCommand.
          [ (Analyze, manual_generic)
          ; (Capture, manual_generic)
          ; (Run, manual_generic)
          ; (Report, manual_generic) ]
      "Also log messages to stdout and stderr"
  in
  let linters_developer_mode =
    CLOpt.mk_bool_group ~long:"linters-developer-mode"
      ~in_help:InferCommand.[(Capture, manual_clang_linters)]
      "Debug mode for developing new linters. (Sets the analyzer to $(b,linters); also sets \
       $(b,--debug), $(b,--debug-level-linters 2), $(b,--developer-mode), and unsets \
       $(b,--allowed-failures) and $(b,--default-linters)."
      ~f:(fun debug ->
        debug_level_linters := if debug then 2 else 0 ;
        debug )
      [debug; developer_mode] [default_linters; keep_going]
  in
  ( bo_debug
  , deduplicate
  , developer_mode
  , debug
  , debug_exceptions
  , debug_level_analysis
  , debug_level_capture
  , debug_level_linters
  , debug_level_test_determinator
  , default_linters
  , filtering
  , frontend_tests
  , keep_going
  , linters_developer_mode
  , only_cheap_debug
  , print_buckets
  , print_jbir
  , print_logs
  , print_types
  , reports_include_ml_loc
  , trace_error
  , write_html
  , write_dotty )


and dependencies =
  CLOpt.mk_bool ~deprecated:["dependencies"] ~long:"dependencies"
    ~in_help:InferCommand.[(Capture, manual_java)]
    "Translate all the dependencies during the capture. The classes in the given jar file will be \
     translated. No sources needed."


and differential_filter_files =
  CLOpt.mk_string_opt ~long:"differential-filter-files"
    ~in_help:InferCommand.[(Report, manual_generic)]
    "Specify the file containing the list of source files for which a differential report is \
     desired. Source files should be specified relative to project root or be absolute"


and differential_filter_set =
  CLOpt.mk_symbol_seq ~long:"differential-filter-set" ~eq:PolyVariantEqual.( = )
    "Specify which set of the differential results is filtered with the modified files provided \
     through the $(b,--differential-modified-files) argument. By default it is applied to all sets \
     ($(b,introduced), $(b,fixed), and $(b,preexisting))"
    ~symbols:[("introduced", `Introduced); ("fixed", `Fixed); ("preexisting", `Preexisting)]
    ~default:[`Introduced; `Fixed; `Preexisting]


and () =
  let mk b ?deprecated ~long ?default doc =
    let (_ : string RevList.t ref) =
      CLOpt.mk_string_list ?deprecated ~long
        ~f:(fun issue_id ->
          let issue =
            match IssueType.find_from_string ~id:issue_id with
            | Some issue ->
                issue
            | None ->
                (* unknown issue type: assume it will be defined in AL *)
                IssueType.register_dynamic ~id:issue_id Warning ~linters_def_file:None Linters
          in
          IssueType.set_enabled issue b ;
          issue_id )
        ?default ~meta:"issue_type"
        ~default_to_string:(fun _ -> "")
        ~in_help:InferCommand.[(Report, manual_generic)]
        doc
    in
    ()
  in
  let all_issues = IssueType.all_issues () in
  let disabled_issues_ids =
    List.filter_map all_issues ~f:(fun issue ->
        Option.some_if (not issue.IssueType.enabled) issue.IssueType.unique_id )
  in
  let pp_issue fmt issue =
    let pp_enabled fmt enabled =
      if enabled then F.pp_print_string fmt "enabled by default"
      else F.pp_print_string fmt "disabled by default"
    in
    F.fprintf fmt "%s (%a)" issue.IssueType.unique_id pp_enabled issue.IssueType.enabled
  in
  mk false ~default:disabled_issues_ids ~long:"disable-issue-type"
    ~deprecated:["disable_checks"; "-disable-checks"]
    (F.asprintf
       "Do not show reports coming from this type of issue. Each checker can report a range of \
        issue types. This option provides fine-grained filtering over which types of issue should \
        be reported once the checkers have run. In particular, note that disabling issue types \
        does not make the corresponding checker not run.\n\
        Available issue types are as follows:\n\
       \  @[<v2>%a@].\n"
       (Pp.seq ~print_env:Pp.text_break ~sep:"," pp_issue)
       all_issues) ;
  mk true ~long:"enable-issue-type"
    ~deprecated:["enable_checks"; "-enable-checks"]
    "Show reports coming from this type of issue. By default, all issue types are enabled except \
     the ones listed in $(b,--disable-issue-type). Note that enabling issue types does not make \
     the corresponding checker run; see individual checker options to turn them on or off."


and dotty_cfg_libs =
  CLOpt.mk_bool ~deprecated:["dotty_no_cfg_libs"] ~long:"dotty-cfg-libs" ~default:true
    "Print the cfg of the code coming from the libraries"


and dump_duplicate_symbols =
  CLOpt.mk_bool ~long:"dump-duplicate-symbols"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Dump all symbols with the same name that are defined in more than one file."


and eradicate_condition_redundant =
  CLOpt.mk_bool ~long:"eradicate-condition-redundant" "Condition redundant warnings"


and eradicate_field_over_annotated =
  CLOpt.mk_bool ~long:"eradicate-field-over-annotated" "Field over-annotated warnings"


and eradicate_return_over_annotated =
  CLOpt.mk_bool ~long:"eradicate-return-over-annotated" "Return over-annotated warning"


and eradicate_verbose = CLOpt.mk_bool ~long:"eradicate-verbose" "Print initial and final typestates"

and external_java_packages =
  CLOpt.mk_string_list ~long:"external-java-packages"
    ~in_help:InferCommand.[(Analyze, manual_java)]
    ~meta:"prefix"
    "Specify a list of Java package prefixes for external Java packages. If set, the analysis will \
     not report non-actionable warnings on those packages."


and fail_on_bug =
  CLOpt.mk_bool ~deprecated:["-fail-on-bug"] ~long:"fail-on-issue" ~default:false
    ~in_help:InferCommand.[(Run, manual_generic)]
    (Printf.sprintf "Exit with error code %d if Infer found something to report"
       fail_on_issue_exit_code)


and fcp_apple_clang =
  CLOpt.mk_path_opt ~long:"fcp-apple-clang" ~meta:"path" "Specify the path to Apple Clang"


and fcp_syntax_only = CLOpt.mk_bool ~long:"fcp-syntax-only" "Skip creation of object files"

and file_renamings =
  CLOpt.mk_path_opt ~long:"file-renamings"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "JSON with a list of file renamings to use while computing differential reports"


and filter_paths =
  CLOpt.mk_bool ~long:"filter-paths" ~default:true "Filters specified in .inferconfig"


and force_delete_results_dir =
  CLOpt.mk_bool ~long:"force-delete-results-dir" ~default:false
    ~in_help:
      InferCommand.[(Capture, manual_generic); (Compile, manual_generic); (Run, manual_generic)]
    "Do not refuse to delete the results directory if it doesn't look like an infer results \
     directory."


and force_integration =
  CLOpt.mk_symbol_opt ~long:"force-integration" ~meta:"command"
    ~symbols:(List.Assoc.inverse build_system_exe_assoc)
    ~in_help:InferCommand.[(Capture, manual_generic); (Run, manual_generic)]
    (Printf.sprintf
       "Proceed as if the first argument after $(b,--) was $(i,command). Possible values: %s."
       ( List.map build_system_exe_assoc ~f:(fun (_, s) -> Printf.sprintf "$(i,%s)" s)
       |> String.concat ~sep:", " ))


and from_json_report =
  CLOpt.mk_path_opt ~long:"from-json-report"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"report.json"
    "Load analysis results from a report file (default is to load the results from the specs files \
     generated by the analysis)."


and from_json_config_impact_report =
  CLOpt.mk_path_opt ~long:"from-json-config-impact-report"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"config-impact-report.json"
    "Load costs analysis results from a config-impact-report file."


and from_json_costs_report =
  CLOpt.mk_path_opt ~long:"from-json-costs-report"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"costs-report.json" "Load costs analysis results from a costs-report file."


and frontend_stats =
  CLOpt.mk_bool ~deprecated:["fs"] ~deprecated_no:["nfs"] ~long:"frontend-stats"
    "Output statistics about the capture phase to *.o.astlog (clang only)"


and function_pointer_specialization =
  CLOpt.mk_bool ~long:"function-pointer-specialization" ~default:false
    "Do function pointer preprocessing (clang only)."


and generated_classes =
  CLOpt.mk_path_opt ~long:"generated-classes"
    ~in_help:InferCommand.[(Capture, manual_java)]
    "Specify where to load the generated class files"


and genrule_mode =
  CLOpt.mk_bool ~default:false ~long:"genrule-mode"
    "Enable the genrule compatibility mode used for the Buck integration"


and headers =
  CLOpt.mk_bool ~deprecated:["headers"; "hd"] ~deprecated_no:["no_headers"; "nhd"] ~long:"headers"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Analyze code in header files"


and help =
  let var = ref `None in
  CLOpt.mk_set var `Help ~long:"help"
    ~in_help:(List.map InferCommand.all_commands ~f:(fun command -> (command, manual_generic)))
    "Show this manual" ;
  CLOpt.mk_set var `HelpFull ~long:"help-full"
    ~in_help:(List.map InferCommand.all_commands ~f:(fun command -> (command, manual_generic)))
    (Printf.sprintf "Show this manual with all internal options in the %s section" manual_internal) ;
  CLOpt.mk_set var `HelpScrubbed ~long:"help-scrubbed"
    "Show this manual without specifying default values induced by the current build configuration" ;
  CLOpt.mk_set var `HelpScrubbedFull ~long:"help-scrubbed-full"
    "Show the scrubbed manual with all internal options" ;
  var


and help_checker =
  CLOpt.mk_string_list ~long:"help-checker" ~meta:"checker-id"
    ~in_help:InferCommand.[(Help, manual_generic)]
    "Show information about a checker, for example $(i,biabduction). To see the list of all \
     checkers, see $(b,--list-checkers)."


and help_format =
  CLOpt.mk_symbol ~long:"help-format"
    ~symbols:[("auto", `Auto); ("groff", `Groff); ("pager", `Pager); ("plain", `Plain)]
    ~eq:PolyVariantEqual.( = ) ~default:`Auto
    ~in_help:(List.map InferCommand.all_commands ~f:(fun command -> (command, manual_generic)))
    "Show this help in the specified format. $(b,auto) sets the format to $(b,plain) if the \
     environment variable $(b,TERM) is \"dumb\" or undefined, and to $(b,pager) otherwise."


and help_issue_type =
  CLOpt.mk_string_list ~long:"help-issue-type" ~meta:"UNIQUE_ID"
    ~in_help:InferCommand.[(Help, manual_generic)]
    "Show information about an issue type, for example $(i,NULL_DEREFERENCE). To see the list of \
     all issue types, see $(b,--list-issue-types)."


and html =
  CLOpt.mk_bool ~long:"html"
    ~in_help:InferCommand.[(Explore, manual_explore_bugs)]
    "Generate html report."


and hoisting_report_only_expensive =
  CLOpt.mk_bool ~long:"hoisting-report-only-expensive" ~default:true
    ~in_help:InferCommand.[(Report, manual_hoisting)]
    "[Hoisting] Report loop-invariant calls only when the function is expensive, i.e. at least \
     linear"


and icfg_dotty_outfile =
  CLOpt.mk_path_opt ~long:"icfg-dotty-outfile" ~meta:"path"
    "If set, specifies path where .dot file should be written, it overrides the path for all other \
     options that would generate icfg file otherwise"


and inclusive_cost =
  CLOpt.mk_bool ~long:"inclusive-cost" ~default:true "Computes the inclusive cost"


and issues_tests_fields =
  CLOpt.mk_symbol_seq ~long:"issues-tests-fields"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~default:
      IssuesTestField.
        [File; Procedure; LineOffset; BugType; Bucket; Severity; BugTrace; NullsafeExtra]
    ~symbols:IssuesTestField.all_symbols ~eq:IssuesTestField.equal
    "Fields to emit with $(b,--issues-tests)"


and issues_tests =
  CLOpt.mk_path_opt ~long:"issues-tests"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~meta:"file" "Write a list of issues in a format suitable for tests to $(i,file)"


and iterations =
  CLOpt.mk_int ~deprecated:["iterations"] ~long:"iterations" ~default:1 ~meta:"int"
    "Specify the maximum number of operations for each function, expressed as a multiple of \
     symbolic operations and a multiple of seconds of elapsed time"


and java_debug_source_file_info =
  CLOpt.mk_path_opt ~long:"java-debug-source-file-info" ~meta:"path"
    "For debugging only: Call the Java declarations source file parser on the given file and do \
     not run anything else."


and java_jar_compiler =
  CLOpt.mk_path_opt ~long:"java-jar-compiler"
    ~in_help:InferCommand.[(Capture, manual_java)]
    ~meta:"path" "Specify the Java compiler jar used to generate the bytecode"


and java_source_parser_experimental =
  CLOpt.mk_bool ~long:"java-source-parser-experimental"
    "The experimental Java source parser for declaration locations."


and java_version =
  CLOpt.mk_int_opt ~long:"java-version" ?default:Version.java_version
    ~in_help:InferCommand.[(Capture, manual_java); (Analyze, manual_java)]
    "The version of Java being used. Set it to your Java version if mvn is failing."


and job_id = CLOpt.mk_string_opt ~long:"job-id" "Specify the job ID of this Infer run."

and jobs =
  CLOpt.mk_int ~deprecated:["-multicore"] ~long:"jobs" ~short:'j' ~default:ncpu
    ~default_to_string:(fun _ -> "<number of cores>")
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    ~meta:"int" "Run the specified number of analysis jobs simultaneously"


and join_cond =
  CLOpt.mk_int ~deprecated:["join_cond"] ~long:"join-cond" ~default:1 ~meta:"int"
    {|Set the strength of the final information-loss check used by the join:
- 0 = use the most aggressive join for preconditions
- 1 = use the least aggressive join for preconditions
|}


and liveness_dangerous_classes =
  CLOpt.mk_json ~long:"liveness-dangerous-classes"
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    "Specify classes where the destructor should be ignored when computing liveness. In other \
     words, assignement to variables of these types (or common wrappers around these types such as \
     $(i,unique_ptr<type>)) will count as dead stores when the variables are not read explicitly \
     by the program."


and liveness_ignored_constant =
  CLOpt.mk_string_list ~default:["0"] ~long:"liveness-ignored-constant"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "List of integer constants to be ignored by liveness analysis"


and _log_events =
  CLOpt.mk_bool ~long:"" ~deprecated:["-log-events"] ~deprecated_no:["-no-log-events"]
    "[DOES NOTHING] Turn on the feature that logs events in a machine-readable format"


and _log_skipped =
  CLOpt.mk_bool ~long:"" ~deprecated:["-log-skipped"] ~deprecated_no:["-no-log-skipped"]
    "[DOES NOTHING] Turn on the feature that logs skipped functions (one per file) in a \
     machine-readable format"


and linter =
  CLOpt.mk_string_opt ~long:"linter"
    ~in_help:InferCommand.[(Capture, manual_clang_linters)]
    "From the linters available, only run this one linter. (Useful together with \
     $(b,--linters-developer-mode))"


and linters_def_file =
  CLOpt.mk_path_list ~default:[] ~long:"linters-def-file"
    ~in_help:InferCommand.[(Capture, manual_clang_linters)]
    ~meta:"file" "Specify the file containing linters definition (e.g. 'linters.al')"


and linters_def_folder =
  let linters_def_folder =
    CLOpt.mk_path_list ~default:[] ~long:"linters-def-folder"
      ~in_help:InferCommand.[(Capture, manual_clang_linters)]
      ~meta:"dir" "Specify the folder containing linters files with extension .al"
  in
  let () =
    CLOpt.mk_set linters_def_folder RevList.empty ~long:"reset-linters-def-folder"
      "Reset the list of folders containing linters definitions to be empty (see \
       $(b,linters-def-folder))."
  in
  linters_def_folder


and linters_doc_url =
  CLOpt.mk_string_list ~long:"linters-doc-url"
    ~in_help:InferCommand.[(Capture, manual_clang_linters)]
    "Specify custom documentation URL for some linter that overrides the default one. Useful if \
     your project has specific ways of fixing a lint error that is not true in general or public \
     info. Format: linter_name:doc_url."


and linters_ignore_clang_failures =
  CLOpt.mk_bool ~long:"linters-ignore-clang-failures"
    ~in_help:InferCommand.[(Capture, manual_clang_linters)]
    ~default:false "Continue linting files even if some compilation fails."


and linters_validate_syntax_only =
  CLOpt.mk_bool ~long:"linters-validate-syntax-only"
    ~in_help:InferCommand.[(Capture, manual_clang_linters)]
    ~default:false "Validate syntax of AL files, then emit possible errors in JSON format to stdout"


and list_checkers =
  CLOpt.mk_bool ~long:"list-checkers"
    ~in_help:InferCommand.[(Help, manual_generic)]
    "Show the list of all available checkers."


and list_issue_types =
  CLOpt.mk_bool ~long:"list-issue-types"
    ~in_help:InferCommand.[(Help, manual_generic)]
    "Show the list of all issue types that infer might report."


and load_average =
  CLOpt.mk_float_opt ~long:"load-average" ~short:'l'
    ~in_help:InferCommand.[(Capture, manual_generic)]
    ~meta:"float"
    "Do not start new parallel jobs if the load average is greater than that specified (Buck and \
     make only)"


and margin =
  CLOpt.mk_int ~deprecated:["set_pp_margin"] ~long:"margin" ~default:100 ~meta:"int"
    "Set right margin for the pretty printing functions"


and max_jobs =
  CLOpt.mk_int_opt ~long:"max-jobs"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    ~meta:"int" "Maximum number of analysis jobs running simultaneously"


and max_nesting =
  CLOpt.mk_int_opt ~long:"max-nesting"
    ~in_help:InferCommand.[(Explore, manual_explore_bugs)]
    "Level of nested procedure calls to show. Trace elements beyond the maximum nesting level are \
     skipped. If omitted, all levels are shown."


and memtrace_analysis =
  CLOpt.mk_bool ~long:"memtrace-analysis-profiling"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Generate OCaml analysis allocation traces in `infer-out/memtrace`."


and memtrace_sampling_rate =
  CLOpt.mk_float_opt ~long:"memtrace-sampling-rate" ~default:1e-6
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Sampling rate for Memtrace allocation profiling. Default is 1e-6."


and merge =
  CLOpt.mk_bool ~deprecated:["merge"] ~long:"merge"
    ~in_help:InferCommand.[(Analyze, manual_buck)]
    "Merge the captured results directories specified in the dependency file."


and method_decls_info =
  CLOpt.mk_path_opt ~long:"method-decls-info" ~meta:"method_decls_info.json"
    "Specifies the file containing the method declarations info (eg. start line, end line, class, \
     method name, etc.) when Infer is run Test Determinator mode with $(b,--test-determinator)."


and ml_buckets =
  CLOpt.mk_symbol_seq ~deprecated:["ml_buckets"; "-ml_buckets"] ~long:"ml-buckets"
    ~default:[`MLeak_cf]
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    {|Specify the memory leak buckets to be checked in C++:
- $(b,cpp) from C++ code
|}
    ~symbols:ml_bucket_symbols ~eq:PolyVariantEqual.( = )


and modified_lines =
  CLOpt.mk_path_opt ~long:"modified-lines"
    "Specifies the file containing the modified lines when Infer is run Test Determinator mode \
     with $(b,--test-determinator)."


and monitor_prop_size =
  CLOpt.mk_bool ~deprecated:["monitor_prop_size"] ~long:"monitor-prop-size"
    "Monitor size of props, and print every time the current max is exceeded"


and nelseg = CLOpt.mk_bool ~deprecated:["nelseg"] ~long:"nelseg" "Use only nonempty lsegs"

and nullable_annotation =
  CLOpt.mk_string_opt ~long:"nullable-annotation-name" "Specify custom nullable annotation name"


and nullsafe_annotation_graph =
  CLOpt.mk_bool ~long:"nullsafe-annotation-graph"
    "Nullsafe: an experimental mode for calculating the dependency graph between potential \
     annotations to add in the source code."


and nullsafe_disable_field_not_initialized_in_nonstrict_classes =
  CLOpt.mk_bool ~long:"nullsafe-disable-field-not-initialized-in-nonstrict-classes" ~default:false
    "Nullsafe: In this mode field not initialized issues won't be reported unless the class is \
     marked as @NullsafeStrict. This feature is needed for compatibility reasons."


and nullsafe_optimistic_third_party_in_default_mode =
  CLOpt.mk_bool
    ~long:
      "nullsafe-optimistic-third-party-in-default-mode"
      (* Turned on for compatibility reasons
       *) ~default:true
    "Nullsafe: Unless @Nullsafe annotation is used, treat not annotated third party method params \
     as if they were annotated as nullable, and return values as if they were annotated as \
     non-null"


and nullsafe_third_party_signatures =
  CLOpt.mk_string_opt ~long:"nullsafe-third-party-signatures"
    "Path to a folder with annotated signatures of third-party methods to be taken into account by \
     nullsafe. Path is either relative to .inferconfig folder or absolute"


and nullsafe_third_party_location_for_messaging_only =
  CLOpt.mk_string_opt ~long:"nullsafe-third-party-location-for-messaging-only"
    "Path to a folder with annotated signatures to include into error message. If not specified, \
     path will be fetched from nullsafe-third-party-signatures. This param is only needed for the \
     case when the real repository is located in the different place, and \
     nullsafe-third-party-signatures contains only its copy (which can happen e.g. in case of \
     caching by the build system)"


and nullsafe_strict_containers =
  CLOpt.mk_bool ~long:"nullsafe-strict-containers" ~default:false
    "Warn when containers are used with nullable keys or values"


and only_footprint =
  CLOpt.mk_bool ~deprecated:["only_footprint"] ~long:"only-footprint" "Skip the re-execution phase"


and oom_threshold =
  CLOpt.mk_int_opt ~long:"oom-threshold"
    "Available memory threshold (in MB) below which multi-worker scheduling throttles back work. \
     Only for use on Linux."


and passthroughs =
  CLOpt.mk_bool ~long:"passthroughs" ~default:false
    "In error traces, show intermediate steps that propagate data. When false, error traces are \
     shorter and show only direct flow via souces/sinks"


and patterns_modeled_expensive =
  let long = "modeled-expensive" in
  ( long
  , CLOpt.mk_json ~deprecated:["modeled_expensive"] ~long
      "Matcher or list of matchers for methods that should be considered expensive by the \
       performance critical checker." )


and patterns_never_returning_null =
  let long = "never-returning-null" in
  ( long
  , CLOpt.mk_json ~deprecated:["never_returning_null"] ~long
      "Matcher or list of matchers for functions that never return $(i,null)." )


and patterns_skip_implementation =
  let long = "skip-implementation" in
  ( long
  , CLOpt.mk_json ~long
      "Matcher or list of matchers for names of files where we only want to translate the method \
       declaration, skipping the body of the methods (Java only)." )


and patterns_skip_translation =
  let long = "skip-translation" in
  ( long
  , CLOpt.mk_json ~deprecated:["skip_translation"] ~long
      "Matcher or list of matchers for names of files that should not be analyzed at all." )


and pmd_xml =
  CLOpt.mk_bool ~long:"pmd-xml"
    ~in_help:InferCommand.[(Run, manual_generic)]
    "Output issues in (PMD) XML format in infer-out/report.xml"


and print_active_checkers =
  CLOpt.mk_bool ~long:"print-active-checkers"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Print the active checkers before starting the analysis"


and print_builtins =
  CLOpt.mk_bool ~deprecated:["print_builtins"] ~long:"print-builtins"
    "Print the builtin functions and exit"


and _print_log_identifier =
  CLOpt.mk_bool ~long:"" ~deprecated:["-print-log-identifier"]
    ~deprecated_no:["-no-print-log-identifier"]
    "[DOES NOTHING] Print the unique identifier that is common to all logged events"


and global_tenv =
  CLOpt.mk_bool ~long:"global-tenv" "Print the global type environment."
    ~in_help:InferCommand.[(Debug, manual_debug_global_tenv)]


and print_using_diff =
  CLOpt.mk_bool ~deprecated_no:["noprintdiff"] ~long:"print-using-diff" ~default:true
    "Highlight the difference w.r.t. the previous prop when printing symbolic execution debug info"


and procedures =
  CLOpt.mk_bool ~long:"procedures"
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Print functions and methods discovered by infer"


and procedures_attributes =
  CLOpt.mk_bool ~long:"procedures-attributes"
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Print the attributes of each procedure in the output of $(b,--procedures)"


and procedures_cfg =
  CLOpt.mk_bool ~long:"procedures-cfg"
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    (Printf.sprintf
       "Output a dotty file in %s/<file_name>/<proc_name>.dot for each procedure in the output of \
        $(b,--procedures)"
       (ResultsDirEntryName.get_path ~results_dir:"infer-out" Debug))


and procedures_definedness =
  CLOpt.mk_bool ~long:"procedures-definedness" ~default:true
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Include procedures definedness in the output of $(b,--procedures), i.e. whether the procedure \
     definition was found, or only the procedure declaration, or the procedure is an \
     auto-generated Objective-C accessor"


and procedures_filter =
  CLOpt.mk_string_opt ~long:"procedures-filter" ~meta:"filter"
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "With $(b,--procedures), only print functions and methods (procedures) matching the specified \
     $(i,filter). A procedure filter is of the form $(i,path_pattern:procedure_name). Patterns are \
     interpreted as OCaml Str regular expressions. For instance, to keep only methods named \
     \"foo\", one can use the filter \".*:foo\", or \"foo\" for short."


and procedures_name =
  CLOpt.mk_bool ~long:"procedures-name"
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Include procedures names in the output of $(b,--procedures)"


and procedures_source_file =
  CLOpt.mk_bool ~long:"procedures-source-file" ~default:true
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Include the source file in which the procedure definition or declaration was found in the \
     output of $(b,--procedures)"


and procedures_summary =
  CLOpt.mk_bool ~long:"procedures-summary" ~default:false
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Print the summaries of each procedure in the output of $(b,--procedures)"


and procedures_summary_json =
  CLOpt.mk_bool ~long:"procedures-summary-json" ~default:false
    ~in_help:InferCommand.[(Debug, manual_debug_procedures)]
    "Emit the summaries of each procedure in the output of $(b,--procedures) as JSON"


and process_clang_ast =
  CLOpt.mk_bool ~long:"process-clang-ast" ~default:false
    "process the ast to emit some info about the file (Not available for Java)"


and progress_bar =
  CLOpt.mk_bool ~deprecated:["pb"] ~deprecated_no:["no_progress_bar"; "npb"] ~short:'p'
    ~long:"progress-bar" ~default:true
    ~in_help:InferCommand.[(Run, manual_generic)]
    "Show a progress bar"


and progress_bar_style =
  CLOpt.mk_symbol ~long:"progress-bar-style"
    ~symbols:[("auto", `Auto); ("plain", `Plain); ("multiline", `MultiLine)]
    ~eq:Stdlib.( = ) ~default:`Auto
    ~in_help:[(Analyze, manual_generic); (Capture, manual_generic)]
    "Style of the progress bar. $(b,auto) selects $(b,multiline) if connected to a tty, otherwise \
     $(b,plain)."


and project_root =
  CLOpt.mk_path
    ~deprecated:["project_root"; "-project_root"; "pr"]
    ~long:"project-root" ~short:'C' ~default:CLOpt.init_work_dir
    ~default_to_string:(fun _ -> ".")
    ~in_help:
      InferCommand.
        [ (Analyze, manual_generic)
        ; (Capture, manual_generic)
        ; (Run, manual_generic)
        ; (Report, manual_generic) ]
    ~meta:"dir" "Specify the root directory of the project"


and pulse_cut_to_one_path_procedures_pattern =
  CLOpt.mk_string_opt ~long:"pulse-cut-to-one-path-procedures-pattern"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Regex of methods for which pulse will only explore one path. Can be used on pathologically \
     large procedures to prevent too-big states from being produced."


and pulse_intraprocedural_only =
  CLOpt.mk_bool ~long:"pulse-intraprocedural-only"
    "Disable inter-procedural analysis in Pulse. Used for experimentations only."


and pulse_isl =
  CLOpt.mk_bool ~long:"pulse-isl" ~default:false
    "[Pulse] Incorrectness Separation Logic (ISL) mode: explicit Ok/Error summaries are recorded. \
     For experiments only."


and pulse_max_disjuncts =
  CLOpt.mk_int ~long:"pulse-max-disjuncts" ~default:20
    "Under-approximate after $(i,int) disjunctions in the domain"


and pulse_model_abort =
  CLOpt.mk_string_list ~long:"pulse-model-abort"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Methods that should be modelled as abort in Pulse"


and pulse_model_alloc_pattern =
  CLOpt.mk_string_opt ~long:"pulse-model-alloc-pattern"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Regex of methods that should be modelled as allocs in Pulse"


and pulse_model_release_pattern =
  CLOpt.mk_string_opt ~long:"pulse-model-release-pattern"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Regex of methods that should be modelled as release in Pulse"


and pulse_model_return_nonnull =
  CLOpt.mk_string_opt ~long:"pulse-model-return-nonnull"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Regex of methods that should be modelled as returning non-null in Pulse"


and pulse_model_skip_pattern =
  CLOpt.mk_string_opt ~long:"pulse-model-skip-pattern"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Regex of methods that should be modelled as \"skip\" in Pulse"


and pulse_model_transfer_ownership =
  CLOpt.mk_string_list ~long:"pulse-model-transfer-ownership"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Methods that should be modelled as transfering memory ownership in Pulse. Accepted formats \
     are method or namespace::method"


and pulse_recency_limit =
  CLOpt.mk_int ~long:"pulse-recency-limit" ~default:32
    "Maximum number of array elements and structure fields to keep track of for a given array \
     address."


and pulse_report_latent_issues =
  CLOpt.mk_bool ~long:"pulse-report-latent-issues"
    "Only use for testing, there should be no need to turn this on for regular code analysis. \
     Report latent issues instead of waiting for them to become concrete."


and pulse_widen_threshold =
  CLOpt.mk_int ~long:"pulse-widen-threshold" ~default:3
    "Under-approximate after $(i,int) loop iterations"


and pulse_nullsafe_report_npe =
  CLOpt.mk_bool ~long:"pulse-nullsafe-report-npe" ~default:true
    "[Pulse] Suppress NPE reports on files marked @Nullsafe."


and pure_by_default =
  CLOpt.mk_bool ~long:"pure-by-default" ~default:false
    "[Purity]Consider unknown functions to be pure by default"


and quandary_endpoints =
  CLOpt.mk_json ~long:"quandary-endpoints"
    ~in_help:InferCommand.[(Analyze, manual_quandary)]
    "Specify endpoint classes for Quandary"


and quandary_sanitizers =
  CLOpt.mk_json ~long:"quandary-sanitizers"
    ~in_help:InferCommand.[(Analyze, manual_quandary)]
    "Specify custom sanitizers for Quandary"


and quandary_sources =
  CLOpt.mk_json ~long:"quandary-sources"
    ~in_help:InferCommand.[(Analyze, manual_quandary)]
    "Specify custom sources for Quandary"


and quandary_sinks =
  CLOpt.mk_json ~long:"quandary-sinks"
    ~in_help:InferCommand.[(Analyze, manual_quandary)]
    "Specify custom sinks for Quandary"


and quiet =
  CLOpt.mk_bool ~long:"quiet" ~short:'q' ~default:false
    ~in_help:InferCommand.[(Analyze, manual_generic); (Report, manual_generic)]
    "Do not print anything on standard output."


and racerd_guardedby =
  CLOpt.mk_bool ~long:"racerd-guardedby" ~default:false
    ~in_help:InferCommand.[(Analyze, manual_racerd)]
    "Check @GuardedBy annotations with RacerD"


and[@warning "-32"] racerd_unknown_returns_owned =
  CLOpt.mk_bool ~deprecated:["racerd-unknown-returns-owned"] ~long:"racerd-unknown-returns-owned"
    ~default:true
    ~in_help:InferCommand.[(Analyze, manual_racerd)]
    "DEPRECATED, does nothing."


and reactive =
  CLOpt.mk_bool ~deprecated:["reactive"] ~long:"reactive" ~short:'r'
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Reactive mode: the analysis starts from the files captured since the $(i,infer) command \
     started"


and reanalyze =
  CLOpt.mk_bool ~long:"reanalyze"
    "Rerun the analysis. Not compatible with $(b,--incremental-analysis) and \
     $(b,--continue-analysis)."


and relative_path_backtrack =
  CLOpt.mk_int ~long:"backtrack-level" ~default:0 ~meta:"int"
    "Maximum level of backtracking to convert an absolute path to path relative to the common \
     prefix between the project root and the path. For instance, with bactraking level 1, it will \
     convert /my/source/File.java with project root /my/root into ../source/File.java"


and report =
  CLOpt.mk_bool ~long:"report" ~default:true
    ~in_help:InferCommand.[(Analyze, manual_generic); (Run, manual_generic)]
    "Run the reporting phase once the analysis has completed"


and ( report_blacklist_files_containing
    , report_path_regex_blacklist
    , report_path_regex_whitelist
    , report_suppress_errors ) =
  let mk_filtering_option ~suffix ~help ~meta =
    let deprecated =
      List.map ["checkers"; "infer"] ~f:(fun name -> Printf.sprintf "%s-%s" name suffix)
    in
    let long = Printf.sprintf "report-%s" suffix in
    CLOpt.mk_string_list ~deprecated ~long ~meta
      ~in_help:InferCommand.[(Report, manual_generic); (Run, manual_generic)]
      help
  in
  ( mk_filtering_option ~suffix:"blacklist-files-containing"
      ~help:"Do not report any issues on files containing the specified string" ~meta:"string"
  , mk_filtering_option ~suffix:"blacklist-path-regex"
      ~help:
        "Do not report any issues on files whose relative path matches the specified OCaml regex, \
         even if they match the whitelist specified by $(b,--report-whitelist-path-regex)"
      ~meta:"path_regex"
  , mk_filtering_option ~suffix:"whitelist-path-regex"
      ~help:
        "Report issues only on files whose relative path matches the specified OCaml regex (and \
         which do not match $(b,--report-blacklist-path-regex))"
      ~meta:"path_regex"
  , mk_filtering_option ~suffix:"suppress-errors" ~help:"do not report a type of errors"
      ~meta:"error_name" )


and report_console_limit =
  CLOpt.mk_int_opt ~long:"report-console-limit" ~default:5
    ~in_help:InferCommand.[(Report, manual_generic)]
    "Maximum number of issues to display on standard output. Unset with \
     $(b,--report-console-limit-reset) to show all."


and report_current =
  CLOpt.mk_path_opt ~long:"report-current"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "report of the latest revision"


and report_custom_error = CLOpt.mk_bool ~long:"report-custom-error" ""

and report_force_relative_path =
  CLOpt.mk_bool ~long:"report-force-relative-path" ~default:false
    ~in_help:InferCommand.[(Analyze, manual_generic); (Run, manual_generic)]
    "Force converting an absolute path to a relative path to the root directory"


and report_formatter =
  CLOpt.mk_symbol ~long:"report-formatter"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~default:`Phabricator_formatter
    ~symbols:[("none", `No_formatter); ("phabricator", `Phabricator_formatter)]
    ~eq:PolyVariantEqual.( = ) "Which formatter to use when emitting the report"


and report_immutable_modifications =
  CLOpt.mk_bool ~long:"report-immutable-modifications" ~default:false
    ~in_help:InferCommand.[(Report, manual_generic); (Run, manual_generic)]
    "Report modifications to immutable fields"


and report_previous =
  CLOpt.mk_path_opt ~long:"report-previous"
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Report of the base revision to use for comparison"


and rest =
  CLOpt.mk_rest_actions
    ~in_help:InferCommand.[(Capture, manual_generic); (Run, manual_generic)]
    "Stop argument processing, use remaining arguments as a build command" ~usage:exe_usage
    (fun build_exe ->
      match Filename.basename build_exe with "java" | "javac" -> CLOpt.Javac | _ -> CLOpt.NoParse )


and results_dir =
  CLOpt.mk_path ~deprecated:["results_dir"; "-out"] ~long:"results-dir" ~short:'o'
    ~default:(CLOpt.init_work_dir ^/ "infer-out")
    ~default_to_string:(fun _ -> "./infer-out")
    ~in_help:
      InferCommand.
        [ (Analyze, manual_generic)
        ; (Capture, manual_generic)
        ; (Explore, manual_generic)
        ; (Run, manual_generic)
        ; (Report, manual_generic) ]
    ~meta:"dir" "Write results and internal files in the specified directory"


and seconds_per_iteration =
  CLOpt.mk_float_opt ~deprecated:["seconds_per_iteration"] ~long:"seconds-per-iteration"
    ~meta:"float" "Set the number of seconds per iteration (see $(b,--iterations))"


and select =
  CLOpt.mk_string_opt ~long:"select" ~meta:"(N|all)"
    ~in_help:InferCommand.[(Debug, manual_generic); (Explore, manual_explore_bugs)]
    "Select option number $(i,N) or $(i,all) of them. If omitted, prompt for input."


and scuba_logging, cost_scuba_logging =
  let scuba_logging = CLOpt.mk_bool ~long:"scuba-logging" "(direct) logging to scuba" in
  let cost_scuba_logging =
    CLOpt.mk_bool_group ~long:"cost-scuba-logging"
      "Log unknown functions to scuba in cost/inferbo checkers; also sets $(b,--scuba-logging)."
      [scuba_logging] []
  in
  (scuba_logging, cost_scuba_logging)


and scuba_normals =
  CLOpt.mk_string_map ~long:"scuba-normal"
    "add an extra string (normal) field to be set for each sample of scuba, format <name>=<value>"


and scuba_tags =
  CLOpt.mk_string_map ~long:"scuba-tags"
    "add an extra set of strings (tagset) field to be set for each sample of scuba, format \
     <name>=(<value>,<value>,<value>|NONE)"


and siof_safe_methods =
  CLOpt.mk_string_list ~long:"siof-safe-methods"
    ~in_help:InferCommand.[(Analyze, manual_siof)]
    "Methods that are SIOF-safe; \"foo::bar\" will match \"foo::bar()\", \"foo<int>::bar()\", etc. \
     (can be specified multiple times)"


and skip_analysis_in_path =
  CLOpt.mk_string_list ~deprecated:["-skip-clang-analysis-in-path"] ~long:"skip-analysis-in-path"
    ~in_help:InferCommand.[(Capture, manual_generic); (Run, manual_generic)]
    ~meta:"path_prefix_OCaml_regex"
    "Ignore files whose path matches the given prefix (can be specified multiple times)"


and skip_analysis_in_path_skips_compilation =
  CLOpt.mk_bool ~long:"skip-analysis-in-path-skips-compilation"
    ~in_help:InferCommand.[(Report, manual_generic)]
    ~default:false "Whether paths in --skip-analysis-in-path should be compiled or not"


and skip_duplicated_types =
  CLOpt.mk_bool ~long:"skip-duplicated-types" ~default:true
    ~in_help:InferCommand.[(ReportDiff, manual_generic)]
    "Skip fixed-then-introduced duplicated types while computing differential reports"


and skip_non_capture_clang_commands =
  CLOpt.mk_bool ~long:"skip-non-capture-clang-commands"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    ~default:false "Skip clang commands that Infer doesn't use to capture data"


and skip_translation_headers =
  CLOpt.mk_string_list ~deprecated:["skip_translation_headers"] ~long:"skip-translation-headers"
    ~in_help:InferCommand.[(Capture, manual_clang)]
    ~meta:"path_prefix" "Ignore headers whose path matches the given prefix"


and source_preview =
  CLOpt.mk_bool ~long:"source-preview" ~default:true
    ~in_help:InferCommand.[(Explore, manual_explore_bugs)]
    "print code excerpts around trace elements"


and source_files =
  CLOpt.mk_bool ~long:"source-files"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    "Print source files discovered by infer"


and source_files_cfg =
  CLOpt.mk_bool ~long:"source-files-cfg"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    (Printf.sprintf
       "Output a dotty file in %s for each source file in the output of $(b,--source-files)"
       (ResultsDirEntryName.get_path ~results_dir:"infer-out" Debug))


and source_files_filter =
  CLOpt.mk_string_opt ~long:"source-files-filter" ~meta:"filter"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    "With $(b,--source-files), only print source files matching the specified $(i,filter). The \
     filter is a pattern that should match the file path. Patterns are interpreted as OCaml Str \
     regular expressions."


and source_files_type_environment =
  CLOpt.mk_bool ~long:"source-files-type-environment"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    "Print the type environment of each source file in the output of $(b,--source-files)"


and source_files_procedure_names =
  CLOpt.mk_bool ~long:"source-files-procedure-names"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    "Print the names of procedure of each source file in the output of $(b,--source-files)"


and source_files_freshly_captured =
  CLOpt.mk_bool ~long:"source-files-freshly-captured"
    ~in_help:InferCommand.[(Debug, manual_debug_source_files)]
    "Print whether the source file has been captured in the most recent capture phase in the \
     output of $(b,--source-files)."


and sources = CLOpt.mk_string_list ~long:"sources" "Specify the list of source files"

and sourcepath = CLOpt.mk_string_opt ~long:"sourcepath" "Specify the sourcepath"

and starvation_skip_analysis =
  CLOpt.mk_json ~long:"starvation-skip-analysis"
    "Specify combinations of class/method list that should be skipped during starvation analysis"


and starvation_whole_program =
  CLOpt.mk_bool ~long:"starvation-whole-program" ~default:false
    "Run whole-program starvation analysis"


and suppress_lint_ignore_types =
  CLOpt.mk_bool ~long:"suppress-lint-ignore-types" ~default:false
    "[DEPRECATED] Check only the presence of @SuppressLint but not the issues types specified as \
     parameters to the annotations when deciding to suppress issues. Use for backwards \
     compatibility only!"


and sqlite_cache_size =
  CLOpt.mk_int ~long:"sqlite-cache-size" ~default:2000
    ~in_help:
      InferCommand.[(Analyze, manual_generic); (Capture, manual_generic); (Run, manual_generic)]
    "SQLite cache size in pages (if positive) or kB (if negative), follows formal of corresponding \
     SQLite PRAGMA."


and sqlite_page_size =
  CLOpt.mk_int ~long:"sqlite-page-size" ~default:32768
    ~in_help:
      InferCommand.[(Analyze, manual_generic); (Capture, manual_generic); (Run, manual_generic)]
    "SQLite page size in bytes, must be a power of two between 512 and 65536."


and sqlite_lock_timeout =
  (* some lame estimate: when the frontend writes CFGs to disk, it may take a few seconds to write
     one CFG and all the cores may be trying to write to the database at the same time. This means
     one process might wait (a few seconds) * (number of cores) to write its CFG. *)
  let five_seconds_per_core = ncpu * 5_000 in
  CLOpt.mk_int ~long:"sqlite-lock-timeout" ~default:five_seconds_per_core
    ~default_to_string:(fun _ -> "five seconds times number of cores")
    ~in_help:
      InferCommand.[(Analyze, manual_generic); (Capture, manual_generic); (Run, manual_generic)]
    "Timeout for SQLite results database operations, in milliseconds."


and sqlite_vfs =
  let default =
    match Utils.read_file "/proc/version" with
    | Result.Ok [line] ->
        let re = Str.regexp "Linux.+-Microsoft" in
        (* on WSL (bash on Windows) standard SQLite VFS can't be used, see WSL/issues/1927 WSL/issues/2395 *)
        if Str.string_match re line 0 then Some "unix-excl" else None
    | _ ->
        None
  in
  CLOpt.mk_string_opt ?default ~long:"sqlite-vfs" "VFS for SQLite"


and (_ : bool ref) =
  CLOpt.mk_bool ~default:false "[DEPRECATED][DOES NOTHING] option does not exist any more"
    ~deprecated:["-sqlite-write-daemon"] ~deprecated_no:["-no-sqlite-write-daemon"] ~long:""


and subtype_multirange =
  CLOpt.mk_bool ~deprecated:["subtype_multirange"] ~long:"subtype-multirange" ~default:true
    "Use the multirange subtyping domain"


and summaries_caches_max_size =
  CLOpt.mk_int ~long:"summaries-caches-max-size" ~default:500
    "The maximum amount of elements the summaries LRU caches can hold"


and symops_per_iteration =
  CLOpt.mk_int_opt ~deprecated:["symops_per_iteration"] ~long:"symops-per-iteration" ~meta:"int"
    "Set the number of symbolic operations per iteration (see $(b,--iterations))"


and test_determinator =
  CLOpt.mk_bool ~long:"test-determinator" ~default:false
    "Run infer in Test Determinator mode. It is used together with the $(b,--modified-lines) and \
     $(b,--profiler-samples) flags, which specify the relevant arguments."


and export_changed_functions =
  CLOpt.mk_bool ~deprecated:["test-determinator-clang"] ~long:"export-changed-functions"
    ~default:false
    "Make infer output changed functions, similar to test-determinator. It is used together with \
     the $(b,--modified-lines)."


and scheduler =
  CLOpt.mk_symbol ~long:"scheduler" ~default:File ~eq:equal_scheduler
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    ~symbols:[("file", File); ("restart", Restart); ("callgraph", SyntacticCallGraph)]
    "Specify the scheduler used for the analysis phase:\n\
     - file: schedule one job per file\n\
     - callgraph: schedule one job per procedure, following the syntactic call graph. Usually \
     faster than \"file\".\n\
     - restart: same as callgraph but uses locking to try and avoid duplicate work between \
     different analysis processes and thus performs better in some circumstances"


and test_filtering =
  CLOpt.mk_bool ~deprecated:["test_filtering"] ~long:"test-filtering"
    "List all the files Infer can report on (should be called from the root of the project)"


and topl_max_conjuncts =
  CLOpt.mk_int ~long:"topl-max-conjuncts" ~default:20
    "Stop tracking states that reach have at least $(i,int) conjuncts"


and topl_max_disjuncts =
  CLOpt.mk_int ~long:"topl-max-disjuncts" ~default:20
    "Under-approximate after $(i,int) disjunctions in the domain"


and topl_properties =
  CLOpt.mk_path_list ~default:[] ~long:"topl-properties"
    "[EXPERIMENTAL] Specify a file containing a temporal property definition (e.g., jdk.topl).\n\
     One needs to also select one of the three implementations, by enabling one of the following \
     checkers\n\
     $(b,--pulse)       will run pulse with updated transfer functions\n\
     $(b,--topl-pulse)  [SLOW] uses SIL-instrumentation, runs pulse, and analyzes pulse summaries\n\
     $(b,--topl-biabd)  [SLOW] uses SIL-instrumentation, runs biabduction, and analyzes \
     biabduction summaries\n\
     Note that enabling topl-pulse or topl-biabd will disable the first implementation using \
     updated pulse transfer functions."


and profiler_samples =
  CLOpt.mk_path_opt ~long:"profiler-samples"
    "File containing the profiler samples when Infer is run Test Determinator mode with \
     $(b,--test-determinator)."


and starvation_strict_mode =
  CLOpt.mk_bool ~long:"starvation-strict-mode" ~default:true
    "During starvation analysis, report strict mode violations (Android only)"


and tenv_json =
  CLOpt.mk_path_opt ~long:"tenv-json"
    ~in_help:InferCommand.[(AnalyzeJson, manual_generic)]
    ~meta:"file" "Path to TEnv json file"


and testing_mode =
  CLOpt.mk_bool
    ~deprecated:["testing_mode"; "-testing_mode"; "tm"]
    ~deprecated_no:["ntm"] ~long:"testing-mode"
    "Mode for testing, where no headers are translated, and dot files are created (clang only)"


and threadsafe_aliases =
  CLOpt.mk_json ~long:"threadsafe-aliases"
    ~in_help:InferCommand.[(Analyze, manual_racerd)]
    "Specify custom annotations that should be considered aliases of @ThreadSafe"


and trace_events =
  CLOpt.mk_bool ~long:"trace-events"
    (Printf.sprintf "Emit Chrome performance trace events in %s"
       (ResultsDirEntryName.get_path ~results_dir:"infer-out" PerfEvents))


and trace_join =
  CLOpt.mk_bool ~deprecated:["trace_join"] ~long:"trace-join"
    "Detailed tracing information during prop join operations"


and trace_ondemand = CLOpt.mk_bool ~long:"trace-ondemand" ""

and trace_rearrange =
  CLOpt.mk_bool ~deprecated:["trace_rearrange"] ~long:"trace-rearrange"
    "Detailed tracing information during prop re-arrangement operations"


and trace_topl =
  CLOpt.mk_bool ~long:"trace-topl" "Detailed tracing information during TOPL analysis"


and tv_commit =
  CLOpt.mk_string_opt ~long:"tv-commit" ~meta:"commit" "Commit hash to submit to Traceview"


and tv_limit =
  CLOpt.mk_int ~long:"tv-limit" ~default:100 ~meta:"int"
    "The maximum number of traces to submit to Traceview"


and tv_limit_filtered =
  CLOpt.mk_int ~long:"tv-limit-filtered" ~default:100 ~meta:"int"
    "The maximum number of traces for issues filtered out by --report-filter to submit to Traceview"


and type_size =
  CLOpt.mk_bool ~deprecated:["type_size"] ~long:"type-size"
    "Consider the size of types during analysis, e.g. cannot use an int pointer to write to a char"


and uninit_interproc =
  CLOpt.mk_bool ~long:"uninit-interproc" "Run uninit check in the experimental interprocedural mode"


and unsafe_malloc =
  CLOpt.mk_bool ~long:"unsafe-malloc"
    ~in_help:InferCommand.[(Analyze, manual_clang)]
    "Assume that malloc(3) never returns null."


and incremental_analysis =
  CLOpt.mk_bool ~long:"incremental-analysis" ~default:false
    "[EXPERIMENTAL] Use incremental analysis for changed files. Not compatible with \
     $(b,--reanalyze) and $(b,--continue-analysis)."


and version =
  let var = ref `None in
  CLOpt.mk_set var `Full ~deprecated:["version"] ~long:"version"
    ~in_help:InferCommand.[(Run, manual_generic)]
    "Print version information and exit" ;
  CLOpt.mk_set var `Json ~deprecated:["version_json"] ~long:"version-json"
    ~in_help:InferCommand.[(Run, manual_generic)]
    "Print version information in json format and exit" ;
  CLOpt.mk_set var `Vcs ~long:"version-vcs" "Print version control system commit and exit" ;
  var


(** visit mode for the worklist:

    - 0 depth - fist visit
    - 1 bias towards exit node
    - 2 least visited first *)
and worklist_mode =
  let var = ref 0 in
  CLOpt.mk_set var 2 ~long:"coverage" "analysis mode to maximize coverage (can take longer)" ;
  CLOpt.mk_set var 1 ~long:"exit-node-bias" ~deprecated:["exit_node_bias"]
    "nodes nearest the exit node are analyzed first" ;
  CLOpt.mk_set var 2 ~long:"visits-bias" ~deprecated:["visits_bias"]
    "nodes visited fewer times are analyzed first" ;
  var


and workspace =
  CLOpt.mk_path_opt ~long:"workspace"
    ~in_help:InferCommand.[(Capture, manual_generic)]
    "Specifies the root of the workspace, which is a directory containing $(b,--project-root). \
     This can be needed if the capture phase is expected to require several $(i,different) project \
     roots, all relative to a common workspace. Usually a single project root is enough, though."


and write_html_whitelist_regex =
  CLOpt.mk_string_list ~long:"write-html-whitelist-regex"
    "Whitelist files that will have their html debug output printed when $(b,--html) is true."


and write_website =
  CLOpt.mk_path_opt ~long:"write-website" ~meta:"path_to_website_dir"
    ~in_help:InferCommand.[(Help, manual_generic)]
    "Use to write website files documenting issue types and checkers under \
     $(i,path_to_website_dir/). Meant to be used within the Infer directory to generate its \
     website at $(i,fbinfer.com) at $(i,website/)."


and xcode_developer_dir =
  CLOpt.mk_path_opt ~long:"xcode-developer-dir"
    ~in_help:InferCommand.[(Capture, manual_buck)]
    ~meta:"XCODE_DEVELOPER_DIR"
    "Specify the path to Xcode developer directory, to use for Buck clang targets"


and xcode_isysroot_suffix =
  CLOpt.mk_string_opt ~long:"xcode-isysroot-suffix"
    ~in_help:InferCommand.[(Analyze, manual_generic)]
    "Specify the suffix of Xcode isysroot directory, to avoid absolute paths in tests"


and xcpretty =
  CLOpt.mk_bool ~long:"xcpretty" ~default:false
    ~in_help:InferCommand.[(Capture, manual_clang)]
    "Infer will use xcpretty together with xcodebuild to analyze an iOS app. xcpretty just needs \
     to be in the path, infer command is still just $(i,`infer -- <xcodebuild command>`)."


(* The "rest" args must appear after "--" on the command line, and hence after other args, so they
   are allowed to refer to the other arg variables. *)

let javac_classes_out =
  CLOpt.mk_string ~parse_mode:CLOpt.Javac ~deprecated:["classes_out"] ~long:""
    ~short:
      'd'
      (* Ensure that some form of "-d ..." is passed to javac. It's unclear whether this is strictly
         needed but the tests break without this for now. See discussion in D4397716. *)
    ~default:CLOpt.init_work_dir
    ~default_to_string:(fun _ -> ".")
    ~f:(fun classes_out ->
      if !buck then (
        let classes_out_infer = resolve classes_out ^/ buck_results_dir_name in
        (* extend env var args to pass args to children that do not receive the rest args *)
        CLOpt.extend_env_args ["--results-dir"; classes_out_infer] ;
        results_dir := classes_out_infer ) ;
      classes_out )
    ""


and () = CLOpt.mk_set ~parse_mode:CLOpt.Javac version ~deprecated:["version"] ~long:"" `Javac ""

and (_ : bool ref) =
  CLOpt.mk_bool ~long:"" ~deprecated:["-cxx-infer-headers"] ~deprecated_no:["-no-cxx-infer-headers"]
    "This option doesn't exist anymore."


(** Parse Command Line Args *)

let inferconfig_dir =
  let rec find dir =
    match Sys.file_exists ~follow_symlinks:false (dir ^/ CommandDoc.inferconfig_file) with
    | `Yes ->
        Some dir
    | `No | `Unknown ->
        let parent = Filename.dirname dir in
        let is_root = String.equal dir parent in
        if is_root then None else find parent
  in
  find (Sys.getcwd ())


let inferconfig_file =
  match Sys.getenv CommandDoc.inferconfig_env_var with
  | Some _ as env_path ->
      env_path
  | None ->
      Option.map inferconfig_dir ~f:(fun dir -> dir ^/ CommandDoc.inferconfig_file)


let post_parsing_initialization command_opt =
  if CommandLineOption.is_originator then
    (* make sure subprocesses read from the same .inferconfig as the toplevel process *)
    Option.iter inferconfig_file ~f:(fun filename ->
        let abs_filename =
          if Filename.is_relative filename then
            (* make sure the path makes sense in children infer processes *)
            CLOpt.init_work_dir ^/ filename
          else filename
        in
        Unix.putenv ~key:CommandDoc.inferconfig_env_var ~data:abs_filename ) ;
  ( match !version with
  | `Full when !buck ->
      (* Buck reads stderr in some versions, stdout in others *)
      print_endline version_string ;
      prerr_endline version_string
  | `Javac when !buck ->
      (* print buck key *)
      let infer_version =
        match inferconfig_file with
        | Some inferconfig ->
            Printf.sprintf "version %s/inferconfig %s" Version.commit
              (Caml.Digest.to_hex (Caml.Digest.file inferconfig))
        | None ->
            Version.commit
      in
      F.printf "infer/%s@." infer_version ;
      F.eprintf "infer/%s@." infer_version
  | `Full ->
      print_endline version_string
  | `Javac ->
      (* javac prints version on stderr *) prerr_endline version_string
  | `Json ->
      print_endline Version.versionJson
  | `Vcs ->
      print_endline Version.commit
  | `None ->
      () ) ;
  ( match !help with
  | `Help ->
      CLOpt.show_manual !help_format CommandDoc.infer command_opt
  | `HelpFull ->
      CLOpt.show_manual ~internal_section:manual_internal !help_format CommandDoc.infer command_opt
  | `HelpScrubbed ->
      CLOpt.show_manual ~scrub_defaults:true !help_format CommandDoc.infer command_opt
  | `HelpScrubbedFull ->
      CLOpt.show_manual ~scrub_defaults:true ~internal_section:manual_internal !help_format
        CommandDoc.infer command_opt
  | `None ->
      () ) ;
  if PolyVariantEqual.(!version <> `None || !help <> `None) then Stdlib.exit 0 ;
  let uncaught_exception_handler exn raw_backtrace =
    let is_infer_exit_zero = match exn with L.InferExit 0 -> true | _ -> false in
    let should_print_backtrace_default =
      match exn with L.InferUserError _ | L.InferExit _ -> false | _ -> true
    in
    let suggest_keep_going = should_print_backtrace_default && not !keep_going in
    let backtrace =
      if is_infer_exit_zero then "" else Caml.Printexc.raw_backtrace_to_string raw_backtrace
    in
    let print_exception () =
      let error prefix msg =
        ANSITerminal.prerr_string L.(term_styles_of_style Fatal) prefix ;
        ANSITerminal.prerr_string L.(term_styles_of_style Fatal) msg ;
        ANSITerminal.prerr_string L.(term_styles_of_style Normal) "\n"
      in
      match exn with
      | Failure msg ->
          error "ERROR: " msg
      | L.InferExternalError msg ->
          error "External Error: " msg
      | L.InferInternalError msg ->
          error "Internal Error: " msg
      | L.InferUserError msg ->
          error "Usage Error: " msg
      | L.InferExit _ ->
          ()
      | _ ->
          error "Uncaught Internal Error: " (Exn.to_string exn)
    in
    print_exception () ;
    if (not is_infer_exit_zero) && (should_print_backtrace_default || !developer_mode) then (
      ANSITerminal.prerr_string L.(term_styles_of_style Error) "Error backtrace:\n" ;
      ANSITerminal.prerr_string L.(term_styles_of_style Error) backtrace ) ;
    if not is_infer_exit_zero then Out_channel.newline stderr ;
    if suggest_keep_going then
      ANSITerminal.prerr_string
        L.(term_styles_of_style Normal)
        "Run the command again with `--keep-going` to try and ignore this error.\n" ;
    let exitcode = L.exit_code_of_exception exn in
    L.log_uncaught_exception exn ~exitcode ;
    Epilogues.run () ;
    Stdlib.exit exitcode
  in
  Caml.Printexc.set_uncaught_exception_handler uncaught_exception_handler ;
  F.set_margin !margin ;
  let set_gc_params () =
    let ctrl = Gc.get () in
    let words_of_Mb nMb = nMb * 1024 * 1024 * 8 / Sys.word_size in
    let new_size nMb = max ctrl.minor_heap_size (words_of_Mb nMb) in
    (* increase the minor heap size *)
    let minor_heap_size = new_size 8 in
    (* use the best-fit allocator *)
    let allocation_policy = 2 in
    (* increase the overhead as the default is tuned for the next-fit allocator *)
    let space_overhead = 120 in
    Gc.set {ctrl with minor_heap_size; allocation_policy; space_overhead}
  in
  set_gc_params () ;
  let symops_timeout, seconds_timeout =
    let default_symops_timeout = 1100 in
    let default_seconds_timeout = 10.0 in
    if !biabduction_models_mode then (* disable timeouts when analyzing models *)
      (None, None)
    else (Some default_symops_timeout, Some default_seconds_timeout)
  in
  if is_none !symops_per_iteration then symops_per_iteration := symops_timeout ;
  if is_none !seconds_per_iteration then seconds_per_iteration := seconds_timeout ;
  clang_compilation_dbs :=
    RevList.rev_map ~f:(fun x -> `Raw x) !compilation_database
    |> RevList.rev_map_append ~f:(fun x -> `Escaped x) !compilation_database_escaped ;
  (* set analyzer mode to linters in linters developer mode *)
  if !linters_developer_mode then enable_checker Linters ;
  if !default_linters then
    linters_def_file := RevList.cons linters_def_default_file !linters_def_file ;
  ( match !analyzer with
  | Linters ->
      disable_all_checkers () ;
      capture := false ;
      enable_checker Linters
  | Checkers ->
      () ) ;
  Option.value ~default:InferCommand.Run command_opt


let command =
  let command_opt, _usage_exit =
    CLOpt.parse ?config_file:inferconfig_file ~usage:exe_usage startup_action initial_command
  in
  post_parsing_initialization command_opt


let process_linters_doc_url args =
  let linters_doc_url arg =
    match String.lsplit2 ~on:':' arg with
    | Some linter_doc_url_assoc ->
        linter_doc_url_assoc
    | None ->
        L.(die UserError)
          "Incorrect format for the option linters-doc-url. The correct format is linter:doc_url \
           but got %s"
          arg
  in
  let linter_doc_url_assocs = RevList.rev_map ~f:linters_doc_url args in
  fun ~linter_id -> List.Assoc.find ~equal:String.equal linter_doc_url_assocs linter_id


(** Freeze initialized configuration values *)

let rest = !rest

and abs_struct = !abs_struct

and abs_val = !abs_val

and allow_leak = !allow_leak

and annotation_reachability_cxx = !annotation_reachability_cxx

and annotation_reachability_cxx_sources = !annotation_reachability_cxx_sources

and annotation_reachability_custom_pairs = !annotation_reachability_custom_pairs

and append_buck_flavors = RevList.to_list !append_buck_flavors

and array_level = !array_level

and biabduction_models_mode = !biabduction_models_mode

and bootclasspath = !bootclasspath

and bo_debug = !bo_debug

and bo_field_depth_limit = !bo_field_depth_limit

and buck = !buck

and buck_blacklist = RevList.to_list !buck_blacklist

and buck_build_args = RevList.to_list !buck_build_args

and buck_build_args_no_inline = RevList.to_list !buck_build_args_no_inline_rev

and buck_cache_mode = (!buck || !genrule_mode) && not !debug

and buck_java_heap_size_gb = !buck_java_heap_size_gb

and buck_java_flavor_suppress_config = !buck_java_flavor_suppress_config

and buck_merge_all_deps = !buck_merge_all_deps

and buck_mode : BuckMode.t option =
  match (!buck_mode, !buck_compilation_database_depth) with
  | `None, _ ->
      None
  | `ClangFlavors, _ ->
      Some ClangFlavors
  | `ClangCompilationDB `NoDeps, _ ->
      Some (ClangCompilationDB NoDependencies)
  | `ClangCompilationDB `DepsTmp, None ->
      Some (ClangCompilationDB DepsAllDepths)
  | `ClangCompilationDB `DepsTmp, Some depth ->
      Some (ClangCompilationDB (DepsUpToDepth depth))
  | `JavaFlavor, _ ->
      Some JavaFlavor


and buck_targets_blacklist = RevList.to_list !buck_targets_blacklist

and call_graph_schedule = !call_graph_schedule

and capture = !capture

and capture_blacklist = !capture_blacklist

and cfg_json = !cfg_json

and censor_report =
  RevList.rev_map !censor_report ~f:(fun str ->
      match String.split str ~on:':' with
      | [issue_type_re; filename_re; reason_str]
        when not String.(is_empty issue_type_re || is_empty filename_re || is_empty reason_str) ->
          let polarity_regex re =
            let polarity = not (Char.equal '!' re.[0]) in
            let regex = Str.regexp (if polarity then re else String.slice re 1 0) in
            (polarity, regex)
          in
          (polarity_regex issue_type_re, polarity_regex filename_re, reason_str)
      | _ ->
          L.(die UserError) "Ill-formed report filter: %s" str )


and changed_files_index = !changed_files_index

and check_version = !check_version

and checkers = List.map !all_checkers ~f:(fun (checker, _, var) -> (checker, !var))

and clang_ast_file =
  match (!clang_biniou_file, !clang_yojson_file) with
  | Some _, Some _ ->
      L.die UserError "Please provide only one of --clang-biniou-file and --clang-yojson-file"
  | Some b, _ ->
      Some (`Biniou b)
  | _, Some y ->
      Some (`Yojson y)
  | _ ->
      None


and clang_compilation_dbs = !clang_compilation_dbs

and clang_compound_literal_init_limit = !clang_compound_literal_init_limit

and clang_extra_flags = RevList.to_list !clang_extra_flags

and clang_blacklisted_flags = RevList.to_list !clang_blacklisted_flags

and clang_blacklisted_flags_with_arg = RevList.to_list !clang_blacklisted_flags_with_arg

and clang_ignore_regex = !clang_ignore_regex

and clang_idirafter_to_override_regex = Option.map ~f:Str.regexp !clang_idirafter_to_override_regex

and clang_isystem_to_override_regex = Option.map ~f:Str.regexp !clang_isystem_to_override_regex

and clang_libcxx_include_to_override_regex = !clang_libcxx_include_to_override_regex

and classpath = !classpath

and config_impact_current = !config_impact_current

and config_impact_data_file = !config_impact_data_file

and config_impact_issues_tests = !config_impact_issues_tests

and config_impact_max_callees_to_print = !config_impact_max_callees_to_print

and config_impact_previous = !config_impact_previous

and continue_analysis = !continue_analysis

and continue_capture = !continue

and costs_current = !costs_current

and cost_issues_tests = !cost_issues_tests

and cost_scuba_logging = !cost_scuba_logging

and costs_previous = !costs_previous

and cost_suppress_func_ptr = !cost_suppress_func_ptr

and cost_tests_only_autoreleasepool = !cost_tests_only_autoreleasepool

and cxx = !cxx

and cxx_scope_guards = !cxx_scope_guards

and debug_level_analysis = !debug_level_analysis

and debug_level_capture = !debug_level_capture

and debug_level_linters = !debug_level_linters

and debug_level_test_determinator = !debug_level_test_determinator

and debug_exceptions = !debug_exceptions

and debug_mode = !debug

and deduplicate = !deduplicate

and default_linters = !default_linters

and dependency_mode = !dependencies

and developer_mode = !developer_mode

and differential_filter_files = !differential_filter_files

and differential_filter_set = !differential_filter_set

and dotty_cfg_libs = !dotty_cfg_libs

and dump_duplicate_symbols = !dump_duplicate_symbols

and eradicate_condition_redundant = !eradicate_condition_redundant

and eradicate_field_over_annotated = !eradicate_field_over_annotated

and eradicate_return_over_annotated = !eradicate_return_over_annotated

and eradicate_verbose = !eradicate_verbose

and external_java_packages = !external_java_packages

and fail_on_bug = !fail_on_bug

and fcp_apple_clang = !fcp_apple_clang

and fcp_syntax_only = !fcp_syntax_only

and file_renamings = !file_renamings

and filter_paths = !filter_paths

and filtering = !filtering

and force_delete_results_dir = !force_delete_results_dir

and force_integration = !force_integration

and from_json_report =
  Option.value !from_json_report
    ~default:(ResultsDirEntryName.get_path ~results_dir:!results_dir ReportJson)


and from_json_config_impact_report =
  Option.value !from_json_config_impact_report
    ~default:(ResultsDirEntryName.get_path ~results_dir:!results_dir ReportConfigImpactJson)


and from_json_costs_report =
  Option.value !from_json_costs_report
    ~default:(ResultsDirEntryName.get_path ~results_dir:!results_dir ReportCostsJson)


and frontend_stats = !frontend_stats

and function_pointer_specialization = !function_pointer_specialization

and frontend_tests = !frontend_tests

and generated_classes = !generated_classes

and genrule_mode = !genrule_mode

and get_linter_doc_url = process_linters_doc_url !linters_doc_url

and help_checker =
  RevList.rev_map !help_checker ~f:(fun checker_string ->
      match Checker.from_id checker_string with
      | Some checker ->
          checker
      | None ->
          L.die UserError
            "Wrong argument for --help-checker: '%s' is not a known checker identifier.@\n\
             @\n\
             See --list-checkers for the list of all checkers." checker_string )


and help_issue_type =
  RevList.rev_map !help_issue_type ~f:(fun id ->
      match IssueType.find_from_string ~id with
      | Some issue_type ->
          issue_type
      | None ->
          L.die UserError
            "Wrong argument for --help-issue-type: '%s' is not a known issue type identifier, or \
             is defined in a linters file.@\n\
             @\n\
             See --list-issue-types for the list of all known issue types." id )


and html = !html

and hoisting_report_only_expensive = !hoisting_report_only_expensive

and global_tenv = !global_tenv

and icfg_dotty_outfile = !icfg_dotty_outfile

and inclusive_cost = !inclusive_cost

and issues_tests = !issues_tests

and issues_tests_fields = !issues_tests_fields

and iterations = !iterations

and java_debug_source_file_info = !java_debug_source_file_info

and java_jar_compiler = !java_jar_compiler

and java_source_parser_experimental = !java_source_parser_experimental

and java_version = !java_version

and javac_classes_out = !javac_classes_out

and job_id = !job_id

and jobs = Option.fold !max_jobs ~init:!jobs ~f:min

and join_cond = !join_cond

and linter = !linter

and linters_def_file = RevList.to_list !linters_def_file

and linters_def_folder = RevList.to_list !linters_def_folder

and linters_developer_mode = !linters_developer_mode

and linters_ignore_clang_failures = !linters_ignore_clang_failures

and linters_validate_syntax_only = !linters_validate_syntax_only

and list_checkers = !list_checkers

and list_issue_types = !list_issue_types

and liveness_dangerous_classes = !liveness_dangerous_classes

and liveness_ignored_constant = RevList.to_list !liveness_ignored_constant

and load_average =
  match !load_average with None when !buck -> Some (float_of_int ncpu) | _ -> !load_average


and max_nesting = !max_nesting

and memtrace_analysis = !memtrace_analysis

and memtrace_sampling_rate = Option.value_exn !memtrace_sampling_rate

and merge = !merge

and method_decls_info = !method_decls_info

and ml_buckets = !ml_buckets

and modified_lines = !modified_lines

and monitor_prop_size = !monitor_prop_size

and nelseg = !nelseg

and nullable_annotation = !nullable_annotation

and nullsafe_annotation_graph = !nullsafe_annotation_graph

and nullsafe_disable_field_not_initialized_in_nonstrict_classes =
  !nullsafe_disable_field_not_initialized_in_nonstrict_classes


and nullsafe_optimistic_third_party_in_default_mode =
  !nullsafe_optimistic_third_party_in_default_mode


and nullsafe_third_party_signatures = !nullsafe_third_party_signatures

and nullsafe_third_party_location_for_messaging_only =
  !nullsafe_third_party_location_for_messaging_only


and nullsafe_strict_containers = !nullsafe_strict_containers

and no_translate_libs = not !headers

and oom_threshold = !oom_threshold

and only_cheap_debug = !only_cheap_debug

and only_footprint = !only_footprint

and passthroughs = !passthroughs

and patterns_modeled_expensive = match patterns_modeled_expensive with k, r -> (k, !r)

and patterns_never_returning_null = match patterns_never_returning_null with k, r -> (k, !r)

and patterns_skip_implementation = match patterns_skip_implementation with k, r -> (k, !r)

and patterns_skip_translation = match patterns_skip_translation with k, r -> (k, !r)

and pmd_xml = !pmd_xml

and print_active_checkers = !print_active_checkers

and print_builtins = !print_builtins

and print_logs = !print_logs

and print_types = !print_types

and print_using_diff = !print_using_diff

and procedures = !procedures

and procedures_attributes = !procedures_attributes

and procedures_cfg = !procedures_cfg

and procedures_definedness = !procedures_definedness

and procedures_filter = !procedures_filter

and procedures_name = !procedures_name

and procedures_source_file = !procedures_source_file

and procedures_summary = !procedures_summary

and procedures_summary_json = !procedures_summary_json

and process_clang_ast = !process_clang_ast

and progress_bar =
  if !progress_bar && not !quiet then
    match !progress_bar_style with
    | `Auto when Unix.(isatty stdin && isatty stderr) ->
        `MultiLine
    | `Auto ->
        `Plain
    | (`Plain | `MultiLine) as style ->
        style
  else `Quiet


and project_root = !project_root

and pulse_cut_to_one_path_procedures_pattern =
  Option.map ~f:Str.regexp !pulse_cut_to_one_path_procedures_pattern


and pulse_intraprocedural_only = !pulse_intraprocedural_only

and pulse_isl = !pulse_isl

and pulse_max_disjuncts = !pulse_max_disjuncts

and pulse_model_abort = RevList.to_list !pulse_model_abort

and pulse_model_alloc_pattern = Option.map ~f:Str.regexp !pulse_model_alloc_pattern

and pulse_model_release_pattern = Option.map ~f:Str.regexp !pulse_model_release_pattern

and pulse_model_return_nonnull = Option.map ~f:Str.regexp !pulse_model_return_nonnull

and pulse_model_skip_pattern = Option.map ~f:Str.regexp !pulse_model_skip_pattern

and pulse_model_transfer_ownership_namespace, pulse_model_transfer_ownership =
  let models =
    let re = Str.regexp "::" in
    RevList.map ~f:(fun model -> (model, Str.split re model)) !pulse_model_transfer_ownership
  in
  let aux el =
    match el with
    | _, [namespace; m] ->
        First (namespace, m)
    | _, [m] ->
        Second m
    | option, splits ->
        L.die UserError
          "Wrong use of option pulse-model-transfer-ownership %s: expected at most one namespace \
           but found %d"
          option
          (List.length splits - 1)
  in
  RevList.rev_partition_map ~f:aux models


and pulse_recency_limit = !pulse_recency_limit

and pulse_report_latent_issues = !pulse_report_latent_issues

and pulse_widen_threshold = !pulse_widen_threshold

and pulse_nullsafe_report_npe = !pulse_nullsafe_report_npe

and pure_by_default = !pure_by_default

and quandary_endpoints = !quandary_endpoints

and quandary_sanitizers = !quandary_sanitizers

and quandary_sources = !quandary_sources

and quandary_sinks = !quandary_sinks

and quiet = !quiet

and racerd_guardedby = !racerd_guardedby

and reactive_mode = !reactive

and reanalyze = !reanalyze

and relative_path_backtrack = !relative_path_backtrack

and report = !report

and report_blacklist_files_containing = RevList.to_list !report_blacklist_files_containing

and report_console_limit = !report_console_limit

and report_current = !report_current

and report_custom_error = !report_custom_error

and report_force_relative_path = !report_force_relative_path

and report_formatter = !report_formatter

and report_immutable_modifications = !report_immutable_modifications

and report_path_regex_blacklist = RevList.to_list !report_path_regex_blacklist

and report_path_regex_whitelist = RevList.to_list !report_path_regex_whitelist

and report_previous = !report_previous

and report_suppress_errors = RevList.to_list !report_suppress_errors

and reports_include_ml_loc = !reports_include_ml_loc

and results_dir = !results_dir

and scheduler = !scheduler

and scuba_logging = !scuba_logging

and scuba_normals = !scuba_normals

and scuba_tags = String.Map.map !scuba_tags ~f:(fun v -> String.split v ~on:',')

and seconds_per_iteration = !seconds_per_iteration

and select =
  match !select with
  | None ->
      None
  | Some "all" ->
      Some `All
  | Some n -> (
    try Some (`Select (Int.of_string n))
    with _ ->
      L.die UserError "Wrong argument for --select: expected an integer or \"all\" but got '%s'" n )


and show_buckets = !print_buckets

and print_jbir = !print_jbir

and siof_check_iostreams = !siof_check_iostreams

and siof_safe_methods = RevList.to_list !siof_safe_methods

and skip_analysis_in_path = RevList.to_list !skip_analysis_in_path

and skip_analysis_in_path_skips_compilation = !skip_analysis_in_path_skips_compilation

and skip_duplicated_types = !skip_duplicated_types

and skip_non_capture_clang_commands = !skip_non_capture_clang_commands

and skip_translation_headers = RevList.to_list !skip_translation_headers

and source_preview = !source_preview

and source_files = !source_files

and source_files_cfg = !source_files_cfg

and source_files_filter = !source_files_filter

and source_files_type_environment = !source_files_type_environment

and source_files_procedure_names = !source_files_procedure_names

and source_files_freshly_captured = !source_files_freshly_captured

and sources = RevList.to_list !sources

and sourcepath = !sourcepath

and sqlite_cache_size = !sqlite_cache_size

and sqlite_page_size = !sqlite_page_size

and sqlite_lock_timeout = !sqlite_lock_timeout

and sqlite_vfs = !sqlite_vfs

and starvation_skip_analysis = !starvation_skip_analysis

and starvation_strict_mode = !starvation_strict_mode

and starvation_whole_program = !starvation_whole_program

and subtype_multirange = !subtype_multirange

and summaries_caches_max_size = !summaries_caches_max_size

and suppress_lint_ignore_types = !suppress_lint_ignore_types

and custom_symbols =
  (* Convert symbol lists to regexps just once, here *)
  match !custom_symbols with
  | `Assoc sym_lists ->
      List.Assoc.map ~f:get_symbols_regexp sym_lists
  | `List [] ->
      []
  | _ ->
      L.(die UserError)
        "--custom-symbols must be dictionary of symbol lists not %s"
        (Yojson.Basic.to_string !custom_symbols)


and symops_per_iteration = !symops_per_iteration

and keep_going = !keep_going

and tenv_json = !tenv_json

and test_determinator = !test_determinator

and export_changed_functions = !export_changed_functions

and test_filtering = !test_filtering

and profiler_samples = !profiler_samples

and testing_mode = !testing_mode

and threadsafe_aliases = !threadsafe_aliases

and topl_max_conjuncts = !topl_max_conjuncts

and topl_max_disjuncts = !topl_max_disjuncts

and topl_properties = RevList.to_list !topl_properties

and trace_error = !trace_error

and trace_events = !trace_events

and trace_ondemand = !trace_ondemand

and trace_join = !trace_join

and trace_rearrange = !trace_rearrange

and trace_topl = !trace_topl

and tv_commit = !tv_commit

and tv_limit = !tv_limit

and tv_limit_filtered = !tv_limit_filtered

and type_size = !type_size

and uninit_interproc = !uninit_interproc

and unsafe_malloc = !unsafe_malloc

and incremental_analysis = !incremental_analysis

and worklist_mode = !worklist_mode

and workspace = !workspace

and write_dotty = !write_dotty

and write_html = !write_html

and write_html_whitelist_regex = RevList.to_list !write_html_whitelist_regex

and write_website = !write_website

and xcode_developer_dir = !xcode_developer_dir

and xcode_isysroot_suffix = !xcode_isysroot_suffix

and xcpretty = !xcpretty

(** Configuration values derived from command-line options *)

let mem_checkers enabled_checkers (c : Checker.t) = List.mem ~equal:Checker.equal enabled_checkers c

let enabled_checkers =
  (* invariant: [newly_enabled_checkers] is *included* in [enabled_checkers] and [enabled_checkers]
     has at most one occurrence of each checker.

     NOTE: the complexity of this is quadratic in the number of checkers but this is fine as we
     don't have hundreds of checkers (yet). Also we have few dependencies and shallow dependency
     chains so we don't even get close to the worst case. *)
  let rec fixpoint newly_enabled_checkers enabled_checkers =
    let newly_enabled_checkers, enabled_checkers' =
      List.fold newly_enabled_checkers ~init:([], enabled_checkers)
        ~f:(fun (newly_enabled_checkers, enabled_checkers) checker ->
          let to_enable =
            (Checker.config checker).activates
            |> List.filter ~f:(fun checker_dep -> not (mem_checkers enabled_checkers checker_dep))
          in
          match to_enable with
          | [] ->
              (newly_enabled_checkers, enabled_checkers)
          | _ :: _ ->
              (to_enable @ newly_enabled_checkers, to_enable @ enabled_checkers) )
    in
    if List.is_empty newly_enabled_checkers then enabled_checkers
    else fixpoint newly_enabled_checkers enabled_checkers'
  in
  let enabled_checkers =
    let enabled0 =
      List.filter_map checkers ~f:(fun (checker, active) -> if active then Some checker else None)
    in
    fixpoint enabled0 enabled0
  in
  if
    hoisting_report_only_expensive
    && mem_checkers enabled_checkers LoopHoisting
    && not (mem_checkers enabled_checkers Cost)
  then fixpoint [Checker.Cost] (Checker.Cost :: enabled_checkers)
  else enabled_checkers


let is_checker_enabled c = mem_checkers enabled_checkers c

let clang_frontend_action_string =
  let text = if capture then ["translating"] else [] in
  let text = if is_checker_enabled Linters then "linting" :: text else text in
  let text =
    if process_clang_ast && test_determinator then "Test Determinator with" :: text else text
  in
  let text =
    if process_clang_ast && export_changed_functions then "Export Changed Functions with" :: text
    else text
  in
  String.concat ~sep:", " text


(* Specify treatment of dynamic dispatch in Java code: false 'none' treats dynamic dispatch as
   a call to unknown code and true triggers lazy dynamic dispatch. The latter mode follows the
   JVM semantics and creates procedure descriptions during symbolic execution using the type
   information found in the abstract state *)
let dynamic_dispatch = is_checker_enabled Biabduction

(** Check if a Java package is external to the repository *)
let java_package_is_external package =
  RevList.exists external_java_packages ~f:(fun (prefix : string) ->
      String.is_prefix package ~prefix )


let is_in_custom_symbols list_name symbol =
  match List.Assoc.find ~equal:String.equal custom_symbols list_name with
  | Some regexp ->
      Str.string_match regexp symbol 0
  | None ->
      false


let scuba_execution_id =
  if scuba_logging then (
    Random.self_init () ;
    Some (Random.int64 Int64.max_value) )
  else None


let toplevel_results_dir =
  if CLOpt.is_originator then (
    (* let subprocesses know where the toplevel process' results dir is *)
    Unix.putenv ~key:infer_top_results_dir_env_var ~data:results_dir ;
    results_dir )
  else Sys.getenv infer_top_results_dir_env_var |> Option.value ~default:results_dir
