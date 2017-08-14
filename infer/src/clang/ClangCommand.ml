(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging

type t = {exec: string; argv: string list; orig_argv: string list; quoting_style: ClangQuotes.style}

let fcp_dir =
  Config.bin_dir ^/ Filename.parent_dir_name ^/ Filename.parent_dir_name
  ^/ "facebook-clang-plugins"

(** path of the plugin to load in clang *)
let plugin_path = fcp_dir ^/ "libtooling" ^/ "build" ^/ "FacebookClangPlugin.dylib"

(** name of the plugin to use *)
let plugin_name = "BiniouASTExporter"

(** whether to amend include search path with C++ model headers *)
let infer_cxx_models = Config.cxx_infer_headers

let value_of_argv_option argv opt_name =
  List.fold
    ~f:(fun (prev_arg, result) arg ->
      let result' =
        if Option.is_some result then result
        else if String.equal opt_name prev_arg then Some arg
        else None
      in
      (arg, result'))
    ~init:("", None) argv
  |> snd

let value_of_option {orig_argv} = value_of_argv_option orig_argv

let has_flag {orig_argv} flag = List.exists ~f:(String.equal flag) orig_argv

let can_attach_ast_exporter cmd =
  let is_supported_language cmd =
    match value_of_option cmd "-x" with
    | None
     -> L.external_warning "malformed -cc1 command has no \"-x\" flag!" ; false
    | Some lang when String.is_prefix ~prefix:"assembler" lang
     -> false
    | Some _
     -> true
  in
  (* -Eonly is -cc1 flag that gets produced by 'clang -M -### ...' *)
  let is_preprocessor_only cmd = has_flag cmd "-E" || has_flag cmd "-Eonly" in
  has_flag cmd "-cc1" && is_supported_language cmd && not (is_preprocessor_only cmd)

let argv_cons a b = a :: b

let argv_do_if cond action x = if cond then action x else x

let file_arg_cmd_sanitizer cmd =
  let file = ClangQuotes.mk_arg_file "clang_command_" cmd.quoting_style cmd.argv in
  {cmd with argv= [Format.sprintf "@%s" file]}

let include_override_regex = Option.map ~f:Str.regexp Config.clang_include_to_override_regex

let filter_and_replace_unsupported_args ?(replace_option_arg= fun _ s -> s)
    ?(blacklisted_flags= []) ?(blacklisted_flags_with_arg= []) ?(post_args= []) args =
  let rec aux (prev, res_rev) args =
    match args with
    | []
     -> (* return non-reversed list *)
        List.rev_append res_rev post_args
    | flag :: tl when List.mem ~equal:String.equal blacklisted_flags flag
     -> aux (flag, res_rev) tl
    | flag1 :: flag2 :: tl when List.mem ~equal:String.equal blacklisted_flags_with_arg flag1
     -> aux (flag2, res_rev) tl
    | arg :: tl
     -> let res_rev' = replace_option_arg prev arg :: res_rev in
        aux (arg, res_rev') tl
  in
  aux ("", []) args

(* Work around various path or library issues occurring when one tries to substitute Apple's version
   of clang with a different version. Also mitigate version discrepancies in clang's
   fatal warnings. *)
let clang_cc1_cmd_sanitizer cmd =
  (* command line options not supported by the opensource compiler or the plugins *)
  let blacklisted_flags = ["-fembed-bitcode-marker"; "-fno-canonical-system-headers"] in
  let blacklisted_flags_with_arg = ["-mllvm"] in
  let replace_option_arg option arg =
    if String.equal option "-arch" && String.equal arg "armv7k" then "armv7"
      (* replace armv7k arch with armv7 *)
    else if String.is_suffix arg ~suffix:"dep.tmp" then (
      (* compilation-database Buck integration produces path to `dep.tmp` file that doesn't exist. Create it *)
      Unix.mkdir_p (Filename.dirname arg) ;
      arg )
    else if String.equal option "-dependency-file"
            && Option.is_some Config.buck_compilation_database
            (* In compilation database mode, dependency files are not assumed to exist *)
    then "/dev/null"
    else if String.equal option "-isystem" then
      match include_override_regex with
      | Some regexp when Str.string_match regexp arg 0
       -> fcp_dir ^/ "clang" ^/ "install" ^/ "lib" ^/ "clang" ^/ "5.0.0" ^/ "include"
      | _
       -> arg
    else arg
  in
  let args_defines = if Config.bufferoverrun then ["-D__INFER_BUFFEROVERRUN"] else [] in
  let post_args_rev =
    [] |> List.rev_append ["-include"; (Config.lib_dir ^/ "clang_wrappers" ^/ "global_defines.h")]
    |> List.rev_append args_defines
    |> (* Never error on warnings. Clang is often more strict than Apple's version.  These arguments
       are appended at the end to override previous opposite settings.  How it's done: suppress
       all the warnings, since there are no warnings, compiler can't elevate them to error
       level. *)
       argv_cons "-Wno-everything"
  in
  let clang_arguments =
    filter_and_replace_unsupported_args ~blacklisted_flags ~blacklisted_flags_with_arg
      ~replace_option_arg ~post_args:(List.rev post_args_rev) cmd.argv
  in
  file_arg_cmd_sanitizer {cmd with argv= clang_arguments}

let mk quoting_style ~prog ~args =
  (* Some arguments break the compiler so they need to be removed even before the normalization step *)
  let blacklisted_flags_with_arg = ["-index-store-path"] in
  let sanitized_args = filter_and_replace_unsupported_args ~blacklisted_flags_with_arg args in
  {exec= prog; orig_argv= sanitized_args; argv= sanitized_args; quoting_style}

let command_to_run cmd =
  let mk_cmd normalizer =
    let {exec; argv; quoting_style} = normalizer cmd in
    Printf.sprintf "'%s' %s" exec
      (List.map ~f:(ClangQuotes.quote quoting_style) argv |> String.concat ~sep:" ")
  in
  if can_attach_ast_exporter cmd then mk_cmd clang_cc1_cmd_sanitizer
  else if String.is_prefix ~prefix:"clang" (Filename.basename cmd.exec) then
    (* `clang` supports argument files and the commands can be longer than the maximum length of the
       command line, so put arguments in a file *)
    mk_cmd file_arg_cmd_sanitizer
  else (* other commands such as `ld` do not support argument files *)
    mk_cmd (fun x -> x)

let with_exec exec args = {args with exec}

let with_plugin_args args =
  let args_before_rev =
    []
    |> (* -cc1 has to be the first argument or clang will think it runs in driver mode *)
       argv_cons "-cc1"
    |> (* It's important to place this option before other -isystem options. *)
       argv_do_if infer_cxx_models (List.rev_append ["-isystem"; Config.cpp_extra_include_dir])
    |> List.rev_append
         [ "-load"
         ; plugin_path
         ; (* (t7400979) this is a workaround to avoid that clang crashes when the -fmodules flag and the
         YojsonASTExporter plugin are used. Since the -plugin argument disables the generation of .o
         files, we invoke apple clang again to generate the expected artifacts. This will keep
         xcodebuild plus all the sub-steps happy. *)
         (if has_flag args "-fmodules" then "-plugin" else "-add-plugin")
         ; plugin_name
         ; ("-plugin-arg-" ^ plugin_name)
         ; "-"
         ; ("-plugin-arg-" ^ plugin_name)
         ; "PREPEND_CURRENT_DIR=1" ]
  in
  (* add -O0 option to avoid compiler obfuscation of AST *)
  let args_after_rev =
    [] |> argv_cons "-O0" |> argv_do_if Config.fcp_syntax_only (argv_cons "-fsyntax-only")
  in
  {args with argv= List.rev_append args_before_rev (args.argv @ List.rev args_after_rev)}

let prepend_arg arg clang_args = {clang_args with argv= arg :: clang_args.argv}

let prepend_args args clang_args = {clang_args with argv= args @ clang_args.argv}

let append_args args clang_args = {clang_args with argv= clang_args.argv @ args}

let get_orig_argv {exec; orig_argv} = Array.of_list (exec :: orig_argv)
