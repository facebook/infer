(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type t =
  { exec: string
  ; argv: string list
  ; orig_argv: string list
  ; quoting_style: ClangQuotes.style
  ; is_driver: bool }

(** bad for every clang invocation *)
let clang_blacklisted_flags =
  ["--expt-relaxed-constexpr"; "-fembed-bitcode-marker"; "-fno-canonical-system-headers"]


let fcp_dir =
  Config.bin_dir ^/ Filename.parent_dir_name ^/ Filename.parent_dir_name
  ^/ "facebook-clang-plugins"


(** path of the plugin to load in clang *)
let plugin_path = fcp_dir ^/ "libtooling" ^/ "build" ^/ "FacebookClangPlugin.dylib"

(** name of the plugin to use *)
let plugin_name = "BiniouASTExporter"

let value_of_argv_option argv opt_name =
  List.fold
    ~f:(fun (prev_arg, result) arg ->
      let result' =
        if Option.is_some result then result
        else if String.equal opt_name prev_arg then Some arg
        else None
      in
      (arg, result') )
    ~init:("", None) argv
  |> snd


let value_of_option {orig_argv} = value_of_argv_option orig_argv

let has_flag {orig_argv} flag = List.exists ~f:(String.equal flag) orig_argv

let can_attach_ast_exporter cmd =
  let is_supported_language cmd =
    match value_of_option cmd "-x" with
    | None ->
        if cmd.is_driver then (* let's continue and ask clang -### *) true
        else (
          L.external_warning "malformed -cc1 command has no \"-x\" flag!" ;
          false )
    | Some "cuda" ->
        false
    | Some lang when String.is_prefix ~prefix:"assembler" lang ->
        false
    | Some _ ->
        true
  in
  (* -Eonly is -cc1 flag that gets produced by 'clang -M -### ...' *)
  let is_preprocessor_only cmd = has_flag cmd "-E" || has_flag cmd "-Eonly" in
  (cmd.is_driver || has_flag cmd "-cc1")
  && is_supported_language cmd
  && not (is_preprocessor_only cmd)


let may_capture cmd = can_attach_ast_exporter cmd

let argv_cons a b = a :: b

let argv_do_if cond action x = if cond then action x else x

let file_arg_cmd_sanitizer cmd =
  let file = ClangQuotes.mk_arg_file "clang_command_" cmd.quoting_style cmd.argv in
  {cmd with argv= [Format.sprintf "@%s" file]}


let include_override_regex = Option.map ~f:Str.regexp Config.clang_include_to_override_regex

(** Filter arguments from [args], looking into argfiles too. [replace_options_arg prev arg] returns
   [arg'], where [arg'] is the new version of [arg] given the preceding arguments (in reverse order) [prev]. *)
let filter_and_replace_unsupported_args ?(replace_options_arg = fun _ s -> s)
    ?(blacklisted_flags = []) ?(blacklisted_flags_with_arg = []) ?(post_args = []) args =
  (* [prev] is the previously seen argument, [res_rev] is the reversed result, [changed] is true if
     some change has been performed *)
  let rec aux in_argfiles (prev_is_blacklisted_with_arg, res_rev, changed) args =
    match args with
    | [] ->
        (prev_is_blacklisted_with_arg, res_rev, changed)
    | _ :: tl when prev_is_blacklisted_with_arg ->
        (* in the unlikely event that a blacklisted flag with arg sits as the last option in some
           arg file, we need to remove its argument now *)
        aux in_argfiles (false, res_rev, true) tl
    | at_argfile :: tl
      when String.is_prefix at_argfile ~prefix:"@" && not (String.Set.mem in_argfiles at_argfile)
          -> (
        let in_argfiles' = String.Set.add in_argfiles at_argfile in
        let argfile = String.slice at_argfile 1 (String.length at_argfile) in
        match In_channel.read_lines argfile with
        | lines ->
            (* poor parsing of arguments with some stripping supported; hope that tools generating
               argfiles more or less put one argument per line *)
            let strip s =
              String.strip s
              |> Utils.strip_balanced_once ~drop:(function '"' | '\'' -> true | _ -> false)
            in
            let last_in_file_is_blacklisted, rev_res_with_file_args, changed_file =
              List.map ~f:strip lines
              |> aux in_argfiles' (prev_is_blacklisted_with_arg, res_rev, false)
            in
            if changed_file then
              aux in_argfiles' (last_in_file_is_blacklisted, rev_res_with_file_args, true) tl
            else
              (* keep the same argfile if we haven't needed to change anything in it *)
              aux in_argfiles' (last_in_file_is_blacklisted, at_argfile :: res_rev, changed) tl
        | exception e ->
            L.external_warning "Error reading argument file '%s': %s@\n" at_argfile
              (Exn.to_string e) ;
            aux in_argfiles' (false, at_argfile :: res_rev, changed) tl )
    | flag :: tl when List.mem ~equal:String.equal blacklisted_flags flag ->
        aux in_argfiles (false, res_rev, true) tl
    | flag :: tl when List.mem ~equal:String.equal blacklisted_flags_with_arg flag ->
        (* remove the flag and its arg separately in case we are at the end of an argfile *)
        aux in_argfiles (true, res_rev, true) tl
    | arg :: tl ->
        let arg' = replace_options_arg res_rev arg in
        aux in_argfiles (false, arg' :: res_rev, changed || not (phys_equal arg arg')) tl
  in
  match aux String.Set.empty (false, [], false) args with _, res_rev, _ ->
    (* return non-reversed list *)
    List.rev_append res_rev post_args


(* Work around various path or library issues occurring when one tries to substitute Apple's version
   of clang with a different version. Also mitigate version discrepancies in clang's
   fatal warnings. *)
let clang_cc1_cmd_sanitizer cmd =
  (* command line options not supported by the opensource compiler or the plugins *)
  let blacklisted_flags_with_arg = ["-mllvm"] in
  let replace_options_arg options arg =
    match options with
    | option :: _ ->
        if String.equal option "-arch" && String.equal arg "armv7k" then "armv7"
          (* replace armv7k arch with armv7 *)
        else if String.is_suffix arg ~suffix:"dep.tmp" then (
          (* compilation-database Buck integration produces path to `dep.tmp` file that doesn't exist. Create it *)
          Unix.mkdir_p (Filename.dirname arg) ;
          arg )
        else if
          String.equal option "-dependency-file" && Option.is_some Config.buck_compilation_database
          (* In compilation database mode, dependency files are not assumed to exist *)
        then "/dev/null"
        else if String.equal option "-isystem" then
          match include_override_regex with
          | Some regexp when Str.string_match regexp arg 0 ->
              fcp_dir ^/ "clang" ^/ "install" ^/ "lib" ^/ "clang" ^/ "7.0.0" ^/ "include"
          | _ ->
              arg
        else arg
    | [] ->
        arg
  in
  let args_defines =
    if Config.bufferoverrun && not Config.biabduction then ["-D__INFER_BUFFEROVERRUN"] else []
  in
  let post_args_rev =
    []
    |> List.rev_append ["-include"; Config.lib_dir ^/ "clang_wrappers" ^/ "global_defines.h"]
    |> List.rev_append args_defines
    |> (* Never error on warnings. Clang is often more strict than Apple's version.  These arguments
       are appended at the end to override previous opposite settings.  How it's done: suppress
       all the warnings, since there are no warnings, compiler can't elevate them to error
       level. *)
       argv_cons "-Wno-everything"
  in
  let clang_arguments =
    filter_and_replace_unsupported_args ~blacklisted_flags:clang_blacklisted_flags
      ~blacklisted_flags_with_arg ~replace_options_arg ~post_args:(List.rev post_args_rev) cmd.argv
  in
  file_arg_cmd_sanitizer {cmd with argv= clang_arguments}


let mk ~is_driver quoting_style ~prog ~args =
  (* Some arguments break the compiler so they need to be removed even before the normalization step *)
  let blacklisted_flags_with_arg = ["-index-store-path"] in
  let sanitized_args =
    filter_and_replace_unsupported_args ~blacklisted_flags:clang_blacklisted_flags
      ~blacklisted_flags_with_arg args
  in
  let sanitized_args =
    if is_driver then sanitized_args @ List.rev Config.clang_extra_flags else sanitized_args
  in
  {exec= prog; orig_argv= sanitized_args; argv= sanitized_args; quoting_style; is_driver}


let to_unescaped_args cmd =
  let mk_exec_argv normalizer =
    let {exec; argv} = normalizer cmd in
    exec :: argv
  in
  if can_attach_ast_exporter cmd then mk_exec_argv clang_cc1_cmd_sanitizer
  else if String.is_prefix ~prefix:"clang" (Filename.basename cmd.exec) then
    (* `clang` supports argument files and the commands can be longer than the maximum length of the
       command line, so put arguments in a file *)
    mk_exec_argv file_arg_cmd_sanitizer
  else (* other commands such as `ld` do not support argument files *)
    mk_exec_argv (fun x -> x)


let pp f cmd = to_unescaped_args cmd |> Pp.cli_args f

let command_to_run cmd =
  to_unescaped_args cmd
  |> List.map ~f:(ClangQuotes.quote cmd.quoting_style)
  |> String.concat ~sep:" "


let with_plugin_args args =
  let plugin_arg_flag = "-plugin-arg-" ^ plugin_name in
  let args_before_rev =
    []
    |> (* -cc1 has to be the first argument or clang will think it runs in driver mode *)
       argv_cons "-cc1"
    |> (* It's important to place this option before other -isystem options. *)
       argv_do_if
         Config.(cxx_infer_headers && (biabduction || bufferoverrun || siof))
         (List.rev_append ["-isystem"; Config.cpp_extra_include_dir])
    |> List.rev_append
         [ "-load"
         ; plugin_path
         ; (* (t7400979) this is a workaround to avoid that clang crashes when the -fmodules flag and the
         YojsonASTExporter plugin are used. Since the -plugin argument disables the generation of .o
         files, we invoke apple clang again to generate the expected artifacts. This will keep
         xcodebuild plus all the sub-steps happy. *)
           (if has_flag args "-fmodules" then "-plugin" else "-add-plugin")
         ; plugin_name
         ; plugin_arg_flag
         ; "-"
         ; plugin_arg_flag
         ; "PREPEND_CURRENT_DIR=1"
         ; plugin_arg_flag
         ; "MAX_STRING_SIZE=" ^ string_of_int CFrontend_config.biniou_buffer_size ]
  in
  (* add -O0 option to avoid compiler obfuscation of AST *)
  let args_after_rev =
    [] |> argv_cons "-O0" |> argv_do_if Config.fcp_syntax_only (argv_cons "-fsyntax-only")
  in
  {args with argv= List.rev_append args_before_rev (args.argv @ List.rev args_after_rev)}


let prepend_arg arg clang_args = {clang_args with argv= arg :: clang_args.argv}

let append_args args clang_args = {clang_args with argv= clang_args.argv @ args}

let get_orig_argv {exec; orig_argv} = exec :: orig_argv
