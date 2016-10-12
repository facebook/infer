/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;

type quoting_style = | DoubleQuotes | SingleQuotes;

type args = {exec: string, argv: list string, quoting_style: quoting_style};

type t = | Assembly of args | CC1 of args | ClangError of string | NonCCCommand of args;


/** path of the plugin to load in clang */
let plugin_path =
  Config.bin_dir /\/
    Filename.parent_dir_name /\/
    Filename.parent_dir_name /\/
    "facebook-clang-plugins" /\/
    "libtooling" /\/
    "build" /\/
    "FacebookClangPlugin.dylib";

let test_env_var var =>
  switch (Sys.getenv var) {
  | "1" => true
  | _ => false
  | exception Not_found => false
  };


/** name of the plugin to use */
let plugin_name = "BiniouASTExporter";


/** this skips the creation of .o files */
let syntax_only = test_env_var "FCP_RUN_SYNTAX_ONLY";


/** specify where is located Apple's clang */
let apple_clang = test_env_var "FCP_APPLE_CLANG";


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

let value_of_option {argv} => value_of_argv_option argv;

let has_flag {argv} flag => IList.exists (string_equal flag) argv;

let mk quoting_style argv => {
  let argv_list = Array.to_list argv;
  let is_assembly =
    /* whether language is set to "assembler" or "assembler-with-cpp" */
    {
      let assembly_language =
        switch (value_of_argv_option argv_list "-x") {
        | Some lang => string_is_prefix "assembler" lang
        | _ => false
        };
      /* Detect -cc1as or assembly language commands. -cc1as is always the first argument if
         present. */
      string_equal argv.(1) "-cc1as" || assembly_language
    };
  let args = {exec: argv.(0), argv: IList.tl argv_list, quoting_style};
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

let command_to_run {exec, argv, quoting_style} => {
  let quote =
    switch quoting_style {
    | DoubleQuotes => (fun s => "\"" ^ s ^ "\"")
    | SingleQuotes => (fun s => "'" ^ s ^ "'")
    };
  Printf.sprintf "'%s' %s" exec (IList.map quote argv |> String.concat " ")
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
        if apple_clang {
          "-plugin"
        } else {
          "-add-plugin"
        },
        plugin_name,
        "-plugin-arg-" ^ plugin_name,
        "-",
        "-plugin-arg-" ^ plugin_name,
        "PREPEND_CURRENT_DIR=1"
      ];
  let args_after = [] |> do_if syntax_only (cons "-fsyntax-only");
  {...args, argv: IList.rev_append rev_args_before (args.argv @ args_after)}
};

let prepend_arg arg clang_args => {...clang_args, argv: [arg, ...clang_args.argv]};

let prepend_args args clang_args => {...clang_args, argv: args @ clang_args.argv};

let append_args args clang_args => {...clang_args, argv: clang_args.argv @ args};

let get_argv {exec, argv} => Array.of_list [exec, ...argv];
