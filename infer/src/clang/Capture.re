/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;

let module CLOpt = CommandLineOption;


/** this fails the execution of clang if the frontend fails */
let report_frontend_failure = not Config.failures_allowed;


/** enable debug mode (to get more data saved to disk for future inspections) */
let debug_mode = Config.debug_mode || Config.frontend_stats;

let buffer_len = 262143;

let catch_biniou_buffer_errors f x =>
  try (f x) {
  | Invalid_argument "Bi_inbuf.refill_from_channel" =>
    Logging.err "WARNING: biniou buffer too short, skipping the file@\n";
    assert false
  };

/* This function reads the json file in fname, validates it, and encoded in the AST data structure
   defined in Clang_ast_t.  */
let validate_decl_from_file fname =>
  catch_biniou_buffer_errors
    (Ag_util.Biniou.from_file len::buffer_len Clang_ast_b.read_decl) fname;

let validate_decl_from_channel chan =>
  catch_biniou_buffer_errors
    (Ag_util.Biniou.from_channel len::buffer_len Clang_ast_b.read_decl) chan;

let register_perf_stats_report source_file => {
  let stats_dir = Filename.concat Config.results_dir Config.frontend_stats_dir_name;
  let abbrev_source_file = DB.source_file_encoding source_file;
  let stats_file = Config.perf_stats_prefix ^ "_" ^ abbrev_source_file ^ ".json";
  create_dir Config.results_dir;
  create_dir stats_dir;
  PerfStats.register_report_at_exit (Filename.concat stats_dir stats_file)
};

let init_global_state_for_capture_and_linters source_file => {
  Logging.set_log_file_identifier
    CommandLineOption.Clang (Some (Filename.basename (DB.source_file_to_string source_file)));
  register_perf_stats_report source_file;
  Config.curr_language := Config.Clang;
  CLocation.curr_file := source_file;
  DB.Results_dir.init source_file;
  Clang_ast_main.reset_cache ();
  CFrontend_config.reset_global_state ()
};

let run_clang_frontend trans_unit_ctx ast_source => {
  let init_time = Unix.gettimeofday ();
  let print_elapsed () => {
    let elapsed = Unix.gettimeofday () -. init_time;
    Logging.out "Elapsed: %07.3f seconds.@\n" elapsed
  };
  let (ast_filename, ast_decl) =
    switch ast_source {
    | `File path => (path, validate_decl_from_file path)
    | `Pipe chan => (
        "stdin of " ^ DB.source_file_to_string trans_unit_ctx.CFrontend_config.source_file,
        validate_decl_from_channel chan
      )
    };
  let (decl_index, stmt_index, type_index, ivar_to_property_index) = Clang_ast_main.index_node_pointers ast_decl;
  CFrontend_config.pointer_decl_index := decl_index;
  CFrontend_config.pointer_stmt_index := stmt_index;
  CFrontend_config.pointer_type_index := type_index;
  CFrontend_config.ivar_to_property_index := ivar_to_property_index;
  CFrontend_config.json := ast_filename;
  Logging.out "Clang frontend action is  %s@\n" Config.clang_frontend_action_string;
  Logging.out
    "Start %s of AST from %s@\n" Config.clang_frontend_action_string !CFrontend_config.json;
  if Config.clang_frontend_do_lint {
    CFrontend_checkers_main.do_frontend_checks trans_unit_ctx ast_decl
  };
  if Config.clang_frontend_do_capture {
    CFrontend.do_source_file trans_unit_ctx ast_decl
  };
  Logging.out "End translation AST file %s... OK!@\n" !CFrontend_config.json;
  print_elapsed ()
};

let run_clang clang_command read =>
  switch (with_process_in clang_command read) {
  | (res, Unix.WEXITED 0) => res
  | (_, Unix.WEXITED n) =>
    /* exit with the same error code as clang in case of compilation failure */
    exit n
  | _ => exit 1
  };

let run_plugin_and_frontend frontend clang_args => {
  let clang_command = ClangCommand.command_to_run (ClangCommand.with_plugin_args clang_args);
  if debug_mode {
    /* -cc1 clang commands always set -o explicitly */
    let object_filename = Option.get (ClangCommand.value_of_option clang_args "-o");
    /* Emit the clang command with the extra args piped to InferClang */
    let frontend_script_fname = Printf.sprintf "%s.sh" object_filename;
    let debug_script_out = open_out frontend_script_fname;
    let debug_script_fmt = Format.formatter_of_out_channel debug_script_out;
    let biniou_fname = Printf.sprintf "%s.biniou" object_filename;
    Format.fprintf debug_script_fmt "%s \\@\n  > %s@\n" clang_command biniou_fname;
    let infer_clang_options =
      String.concat
        "^"
        (
          (
            try [Unix.getenv CLOpt.args_env_var] {
            | Not_found => []
            }
          ) @ [
            "--clang-biniou-file",
            biniou_fname
          ]
        );
    Format.fprintf
      debug_script_fmt
      "%s=\"%s\" %s@\n"
      CLOpt.args_env_var
      infer_clang_options
      (ClangCommand.with_exec Sys.executable_name clang_args |> ClangCommand.command_to_run);
    Format.fprintf
      debug_script_fmt
      "bdump -x -d \"%s/clang_ast.dict\" -w '!!DUMMY!!' %s \\@\n  > %s.bdump"
      Config.etc_dir
      biniou_fname
      object_filename;
    close_out debug_script_out
  };
  run_clang clang_command frontend
};

let capture clang_args => {
  let source_path = {
    let orig_argv = ClangCommand.get_orig_argv clang_args;
    /* the source file is always the last argument of the original -cc1 clang command */
    filename_to_absolute orig_argv.(Array.length orig_argv - 1)
  };
  Logging.out "@\n*** Beginning capture of file %s ***@\n" source_path;
  if (Config.analyzer == Some Config.Compile || CLocation.is_file_blacklisted source_path) {
    Logging.out "@\n Skip the analysis of source file %s@\n@\n" source_path;
    /* We still need to run clang, but we don't have to attach the plugin. */
    run_clang (ClangCommand.command_to_run clang_args) consume_in
  } else {
    let source_file = CLocation.source_file_from_path source_path;
    init_global_state_for_capture_and_linters source_file;
    let trans_unit_ctx = {
      let clang_langs =
        CFrontend_config.[
          ("c", C),
          ("objective-c", ObjC),
          ("c++", CPP),
          ("objective-c++", ObjCPP)
        ];
      let lang =
        switch (ClangCommand.value_of_option clang_args "-x") {
        | Some lang_opt when IList.mem_assoc string_equal lang_opt clang_langs =>
          IList.assoc string_equal lang_opt clang_langs
        | _ => assert false
        };
      {CFrontend_config.source_file: source_file, lang}
    };
    Config.arc_mode := ClangCommand.has_flag clang_args "-fobjc-arc";
    try (
      switch Config.clang_biniou_file {
      | Some fname => run_clang_frontend trans_unit_ctx (`File fname)
      | None =>
        run_plugin_and_frontend
          (fun chan_in => run_clang_frontend trans_unit_ctx (`Pipe chan_in)) clang_args
      }
    ) {
    | exc =>
      if report_frontend_failure {
        raise exc
      }
    };
    ()
  }
};
