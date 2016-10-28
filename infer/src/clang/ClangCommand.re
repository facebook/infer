/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;

type args = {
  exec: string,
  argv: list string,
  orig_argv: list string,
  quoting_style: ClangQuotes.style
};

type t =
  | Assembly args
  /** a normalized clang command that runs the assembler */
  | CC1 args
  /** a -cc1 clang command */
  | ClangError string
  | ClangWarning string
  | NonCCCommand args /** other commands (as, ld, ...) */;

let fcp_dir =
  Config.bin_dir /\/ Filename.parent_dir_name /\/ Filename.parent_dir_name /\/ "facebook-clang-plugins";


/** path of the plugin to load in clang */
let plugin_path = fcp_dir /\/ "libtooling" /\/ "build" /\/ "FacebookClangPlugin.dylib";


/** name of the plugin to use */
let plugin_name = "BiniouASTExporter";


/** are we running as Apple's clang? */
let has_apple_clang = Config.fcp_apple_clang != None;


/** whether to amend include search path with C++ model headers */
let infer_cxx_models = Config.cxx_experimental;

let value_of_argv_option argv opt_name =>
  IList.fold_left
    (
      fun (prev_arg, result) arg => {
        let result' =
          if (Option.is_some result) {
            result
          } else if (string_equal opt_name prev_arg) {
            Some arg
          } else {
            None
          };
        (arg, result')
      }
    )
    ("", None)
    argv |> snd;

let value_of_option {orig_argv} => value_of_argv_option orig_argv;

let has_flag {orig_argv} flag => IList.exists (string_equal flag) orig_argv;

/* Work around various path or library issues occurring when one tries to substitute Apple's version
   of clang with a different version. Also mitigate version discrepancies in clang's
   fatal warnings. */
let mk_clang_compat_args args => {
  /* command line options not supported by the opensource compiler or the plugins */
  let flags_blacklist = ["-fembed-bitcode-marker", "-fno-canonical-system-headers"];
  let replace_option_arg option arg =>
    if (string_equal option "-arch" && string_equal arg "armv7k") {
      "armv7"
      /* replace armv7k arch with armv7 */
    } else if (
      string_equal option "-isystem"
    ) {
      switch Config.clang_include_to_override {
      | Some to_replace when string_equal arg to_replace =>
        fcp_dir /\/ "clang" /\/ "install" /\/ "lib" /\/ "clang" /\/ "4.0.0" /\/ "include"
      | _ => arg
      }
    } else {
      arg
    };
  let post_args = {
    let global_defines_h = Config.lib_dir /\/ "clang_wrappers" /\/ "global_defines.h";
    [
      "-Wno-everything",
      /* Never error on warnings. Clang is often more strict than Apple's version.  These arguments
         are appended at the end to override previous opposite settings.  How it's done: suppress
         all the warnings, since there are no warnings, compiler can't elevate them to error
         level. */
      "-include",
      global_defines_h
    ]
  };
  let rec filter_unsupported_args_and_swap_includes (prev, res) =>
    fun
    | [] => IList.rev (IList.rev post_args @ res)
    | [flag, ...tl] when IList.mem string_equal flag flags_blacklist =>
      filter_unsupported_args_and_swap_includes (flag, res) tl
    | [arg, ...tl] => {
        let res' = [replace_option_arg prev arg, ...res];
        filter_unsupported_args_and_swap_includes (arg, res') tl
      };
  let clang_arguments = filter_unsupported_args_and_swap_includes ("", []) args.argv;
  let file = ClangQuotes.mk_arg_file "clang_args_" args.quoting_style clang_arguments;
  {...args, argv: [Format.sprintf "@%s" file]}
};

let mk quoting_style argv => {
  let argv_list = Array.to_list argv;
  let is_assembly = {
    /* whether language is set to "assembler" or "assembler-with-cpp" */
    let assembly_language =
      switch (value_of_argv_option argv_list "-x") {
      | Some lang => string_is_prefix "assembler" lang
      | _ => false
      };
    /* Detect -cc1as or assembly language commands. -cc1as is always the first argument if
       present. */
    string_equal argv.(1) "-cc1as" || assembly_language
  };
  let args =
    switch argv_list {
    | [exec, ...argv_no_exec] => {exec, orig_argv: argv_no_exec, argv: argv_no_exec, quoting_style}
    | [] => failwith "argv cannot be an empty list"
    };
  if is_assembly {
    Assembly args
  } else if (argv.(1) == "-cc1") {
    CC1
      /* -cc1 is always the first argument if present. */
      args
  } else {
    NonCCCommand args
  }
};

let command_to_run args => {
  let {exec, argv, quoting_style} = mk_clang_compat_args args;
  Printf.sprintf
    "'%s' %s" exec (IList.map (ClangQuotes.quote quoting_style) argv |> String.concat " ")
};

let with_exec exec args => {...args, exec};

let with_plugin_args args => {
  let cons a b => [a, ...b];
  let do_if cond action x =>
    if cond {
      action x
    } else {
      x
    };
  let rev_args_before =
    [] |>
    /* -cc1 has to be the first argument or clang will think it runs in driver mode */
    cons "-cc1" |>
    /* It's important to place this option before other -isystem options. */
    do_if infer_cxx_models (IList.rev_append ["-isystem", Config.cpp_models_dir]) |>
    IList.rev_append [
      "-load",
      plugin_path,
      /* (t7400979) this is a workaround to avoid that clang crashes when the -fmodules flag and the
         YojsonASTExporter plugin are used. Since the -plugin argument disables the generation of .o
         files, we invoke apple clang again to generate the expected artifacts. This will keep
         xcodebuild plus all the sub-steps happy. */
      if has_apple_clang {"-plugin"} else {"-add-plugin"},
      plugin_name,
      "-plugin-arg-" ^ plugin_name,
      "-",
      "-plugin-arg-" ^ plugin_name,
      "PREPEND_CURRENT_DIR=1"
    ];
  let args_after = [] |> do_if Config.fcp_syntax_only (cons "-fsyntax-only");
  {...args, argv: IList.rev_append rev_args_before (args.argv @ args_after)}
};

let prepend_arg arg clang_args => {...clang_args, argv: [arg, ...clang_args.argv]};

let prepend_args args clang_args => {...clang_args, argv: args @ clang_args.argv};

let append_args args clang_args => {...clang_args, argv: clang_args.argv @ args};

let get_orig_argv {exec, orig_argv} => Array.of_list [exec, ...orig_argv];
