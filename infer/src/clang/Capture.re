/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

module CLOpt = CommandLineOption;


/** enable debug mode (to get more data saved to disk for future inspections) */
let debug_mode = Config.debug_mode || Config.frontend_stats || Config.frontend_debug;

let buffer_len = 262143;

let catch_biniou_buffer_errors f x =>
  (
    try (f x) {
    /* suppress warning: allow this one case because we're just reraising the error with another
       error message so it doesn't really matter if this eventually fails */
    | Invalid_argument "Bi_inbuf.refill_from_channel" =>
      Logging.err "WARNING: biniou buffer too short, skipping the file@\n";
      assert false
    }
  )
  [@warning "-52"];

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
  Utils.create_dir Config.results_dir;
  Utils.create_dir stats_dir;
  PerfStats.register_report_at_exit (Filename.concat stats_dir stats_file)
};

let init_global_state_for_capture_and_linters source_file => {
  Logging.set_log_file_identifier
    CLOpt.Clang (Some (Filename.basename (SourceFile.to_abs_path source_file)));
  register_perf_stats_report source_file;
  Config.curr_language := Config.Clang;
  DB.Results_dir.init source_file;
  Clang_ast_main.reset_cache ();
  CFrontend_config.reset_global_state ()
};

let run_clang_frontend ast_source => {
  let init_time = Unix.gettimeofday ();
  let print_elapsed () => {
    let elapsed = Unix.gettimeofday () -. init_time;
    Logging.out "Elapsed: %07.3f seconds.@\n" elapsed
  };
  let ast_decl =
    switch ast_source {
    | `File path => validate_decl_from_file path
    | `Pipe chan => validate_decl_from_channel chan
    };
  let trans_unit_ctx =
    switch ast_decl {
    | Clang_ast_t.TranslationUnitDecl (_, _, _, info) =>
      Config.arc_mode := info.Clang_ast_t.tudi_arc_enabled;
      let source_file = SourceFile.from_abs_path info.Clang_ast_t.tudi_input_path;
      init_global_state_for_capture_and_linters source_file;
      let lang =
        switch info.Clang_ast_t.tudi_input_kind {
        | `IK_C => CFrontend_config.C
        | `IK_CXX => CFrontend_config.CPP
        | `IK_ObjC => CFrontend_config.ObjC
        | `IK_ObjCXX => CFrontend_config.ObjCPP
        | _ => assert false
        };
      {CFrontend_config.source_file: source_file, lang}
    | _ => assert false
    };
  let pp_ast_filename fmt ast_source =>
    switch ast_source {
    | `File path => Format.fprintf fmt "%s" path
    | `Pipe _ =>
      Format.fprintf fmt "stdin of %a" SourceFile.pp trans_unit_ctx.CFrontend_config.source_file
    };
  let (decl_index, stmt_index, type_index, ivar_to_property_index) =
    Clang_ast_main.index_node_pointers ast_decl;
  CFrontend_config.pointer_decl_index := decl_index;
  CFrontend_config.pointer_stmt_index := stmt_index;
  CFrontend_config.pointer_type_index := type_index;
  CFrontend_config.ivar_to_property_index := ivar_to_property_index;
  Logging.out "Clang frontend action is  %s@\n" Config.clang_frontend_action_string;
  Logging.out
    "Start %s of AST from %a@\n" Config.clang_frontend_action_string pp_ast_filename ast_source;
  if Config.clang_frontend_do_lint {
    CFrontend_checkers_main.do_frontend_checks trans_unit_ctx ast_decl
  };
  if Config.clang_frontend_do_capture {
    CFrontend.do_source_file trans_unit_ctx ast_decl
  };
  Logging.out "End translation AST file %a... OK!@\n" pp_ast_filename ast_source;
  print_elapsed ()
};

let run_and_validate_clang_frontend ast_source =>
  try (run_clang_frontend ast_source) {
  | exc =>
    if (not Config.failures_allowed) {
      raise exc
    }
  };

let run_clang clang_command read => {
  let exit_with_error exit_code => {
    Logging.stderr
      "Error: the following clang command did not run successfully:@\n  %s@\n" clang_command;
    exit exit_code
  };
  /* NOTE: exceptions will propagate through without exiting here */
  switch (Utils.with_process_in clang_command read) {
  | (res, Ok ()) => res
  | (_, Error (`Exit_non_zero n)) =>
    /* exit with the same error code as clang in case of compilation failure */
    exit_with_error n
  | _ => exit_with_error 1
  }
};

let run_plugin_and_frontend source_path frontend clang_args => {
  let clang_command = ClangCommand.command_to_run (ClangCommand.with_plugin_args clang_args);
  if debug_mode {
    /* -cc1 clang commands always set -o explicitly */
    let basename = source_path ^ ".ast";
    /* Emit the clang command with the extra args piped to infer-as-clang */
    let frontend_script_fname = Printf.sprintf "%s.sh" basename;
    let debug_script_out = open_out frontend_script_fname;
    let debug_script_fmt = Format.formatter_of_out_channel debug_script_out;
    let biniou_fname = Printf.sprintf "%s.biniou" basename;
    Format.fprintf debug_script_fmt "%s \\@\n  > %s@\n" clang_command biniou_fname;
    Format.fprintf
      debug_script_fmt
      "bdump -x -d \"%s/clang_ast.dict\" -w '!!DUMMY!!' %s \\@\n  > %s.bdump"
      Config.etc_dir
      biniou_fname
      basename;
    Out_channel.close debug_script_out
  };
  run_clang clang_command frontend
};

let cc1_capture clang_cmd => {
  let source_path = {
    let root = Unix.getcwd ();
    let orig_argv = ClangCommand.get_orig_argv clang_cmd;
    /* the source file is always the last argument of the original -cc1 clang command */
    Utils.filename_to_absolute ::root orig_argv.(Array.length orig_argv - 1)
  };
  Logging.out "@\n*** Beginning capture of file %s ***@\n" source_path;
  if (
    Config.equal_analyzer Config.analyzer Config.CompileOnly ||
    CLocation.is_file_blacklisted source_path
  ) {
    Logging.out "@\n Skip the analysis of source file %s@\n@\n" source_path;
    /* We still need to run clang, but we don't have to attach the plugin. */
    run_clang (ClangCommand.command_to_run clang_cmd) Utils.consume_in
  } else {
    switch Config.clang_biniou_file {
    | Some fname => run_and_validate_clang_frontend (`File fname)
    | None =>
      run_plugin_and_frontend
        source_path (fun chan_in => run_and_validate_clang_frontend (`Pipe chan_in)) clang_cmd
    };
    /* reset logging to stop capturing log output into the source file's log */
    Logging.set_log_file_identifier CLOpt.Capture None;
    ()
  }
};

let capture clang_cmd =>
  if (ClangCommand.can_attach_ast_exporter clang_cmd) {
    /* this command compiles some code; replace the invocation of clang with our own clang and
       plugin */
    cc1_capture clang_cmd
  } else {
    /* Non-compilation (eg, linking) command. Run the command as-is. It will not get captured
       further since `clang -### ...` will only output commands that invoke binaries using their
       absolute paths. */
    let command_to_run = ClangCommand.command_to_run clang_cmd;
    Logging.out "Running non-cc command without capture: %s@\n" command_to_run;
    run_clang command_to_run Utils.consume_in
  };
