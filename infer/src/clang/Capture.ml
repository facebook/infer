(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module CLOpt = CommandLineOption
module L = Logging

(** enable debug mode (to get more data saved to disk for future inspections) *)
let debug_mode = Config.debug_mode || Config.frontend_stats || Config.frontend_debug

let buffer_len = 262143

let catch_biniou_buffer_errors f x =
  try[@warning "-52"] f x
  with
  | Invalid_argument
      (* suppress warning: allow this one case because we're just reraising the error with another
       error message so it doesn't really matter if this eventually fails *)
      "Bi_inbuf.refill_from_channel"
  ->
    L.external_error "WARNING: biniou buffer too short, skipping the file@\n" ;
    assert false

(* This function reads the json file in fname, validates it, and encoded in the AST data structure
   defined in Clang_ast_t.  *)
let validate_decl_from_file fname =
  catch_biniou_buffer_errors (Ag_util.Biniou.from_file ~len:buffer_len Clang_ast_b.read_decl) fname

let validate_decl_from_channel chan =
  catch_biniou_buffer_errors (Ag_util.Biniou.from_channel ~len:buffer_len Clang_ast_b.read_decl)
    chan

let register_perf_stats_report source_file =
  let stats_dir = Filename.concat Config.results_dir Config.frontend_stats_dir_name in
  let abbrev_source_file = DB.source_file_encoding source_file in
  let stats_file = Config.perf_stats_prefix ^ "_" ^ abbrev_source_file ^ ".json" in
  Unix.mkdir_p stats_dir ;
  PerfStats.register_report_at_exit (Filename.concat stats_dir stats_file)

let init_global_state_for_capture_and_linters source_file =
  L.(debug Capture Medium) "Processing %s" (Filename.basename (SourceFile.to_abs_path source_file)) ;
  register_perf_stats_report source_file ;
  Config.curr_language := Config.Clang ;
  DB.Results_dir.init source_file ;
  CFrontend_config.reset_global_state ()

let run_clang_frontend ast_source =
  let init_time = Unix.gettimeofday () in
  let print_elapsed () =
    let elapsed = Unix.gettimeofday () -. init_time in
    L.(debug Capture Quiet) "Elapsed: %07.3f seconds.@\n" elapsed
  in
  let ast_decl =
    match ast_source with
    | `File path
     -> validate_decl_from_file path
    | `Pipe chan
     -> validate_decl_from_channel chan
  in
  let trans_unit_ctx =
    match ast_decl with
    | Clang_ast_t.TranslationUnitDecl (_, _, _, info)
     -> Config.arc_mode := info.Clang_ast_t.tudi_arc_enabled ;
        let source_file = SourceFile.from_abs_path info.Clang_ast_t.tudi_input_path in
        init_global_state_for_capture_and_linters source_file ;
        let lang =
          match info.Clang_ast_t.tudi_input_kind with
          | `IK_C
           -> CFrontend_config.C
          | `IK_CXX
           -> CFrontend_config.CPP
          | `IK_ObjC
           -> CFrontend_config.ObjC
          | `IK_ObjCXX
           -> CFrontend_config.ObjCPP
          | _
           -> assert false
        in
        {CFrontend_config.source_file= source_file; lang}
    | _
     -> assert false
  in
  let pp_ast_filename fmt ast_source =
    match ast_source with
    | `File path
     -> Format.fprintf fmt "%s" path
    | `Pipe _
     -> Format.fprintf fmt "stdin of %a" SourceFile.pp trans_unit_ctx.CFrontend_config.source_file
  in
  ClangPointers.populate_all_tables ast_decl ;
  L.(debug Capture Quiet) "Clang frontend action is %s@\n" Config.clang_frontend_action_string ;
  L.(debug Capture Medium)
    "Start %s of AST from %a@\n" Config.clang_frontend_action_string pp_ast_filename ast_source ;
  if Config.clang_frontend_do_lint then
    CFrontend_checkers_main.do_frontend_checks trans_unit_ctx ast_decl ;
  if Config.clang_frontend_do_capture then CFrontend.do_source_file trans_unit_ctx ast_decl ;
  L.(debug Capture Medium) "End translation AST file %a... OK!@\n" pp_ast_filename ast_source ;
  print_elapsed ()

let run_and_validate_clang_frontend ast_source =
  try run_clang_frontend ast_source
  with exc -> if not Config.failures_allowed then raise exc

let run_clang clang_command read =
  let exit_with_error exit_code =
    L.external_error "Error: the following clang command did not run successfully:@\n  %s@\n"
      clang_command ;
    exit exit_code
  in
  (* NOTE: exceptions will propagate through without exiting here *)
  match Utils.with_process_in clang_command read with
  | res, Ok ()
   -> res
  | _, Error `Exit_non_zero n
   -> (* exit with the same error code as clang in case of compilation failure *)
      exit_with_error n
  | _
   -> exit_with_error 1

let run_plugin_and_frontend source_path frontend clang_args =
  let clang_command = ClangCommand.command_to_run (ClangCommand.with_plugin_args clang_args) in
  ( if debug_mode then
      (* -cc1 clang commands always set -o explicitly *)
      let basename = source_path ^ ".ast" in
      (* Emit the clang command with the extra args piped to infer-as-clang *)
      let frontend_script_fname = Printf.sprintf "%s.sh" basename in
      let debug_script_out = Out_channel.create frontend_script_fname in
      let debug_script_fmt = Format.formatter_of_out_channel debug_script_out in
      let biniou_fname = Printf.sprintf "%s.biniou" basename in
      Format.fprintf debug_script_fmt "%s \\@\n  > %s@\n" clang_command biniou_fname ;
      Format.fprintf debug_script_fmt
        "bdump -x -d \"%s/clang_ast.dict\" -w '!!DUMMY!!' %s \\@\n  > %s.bdump" Config.etc_dir
        biniou_fname basename ;
      Out_channel.close debug_script_out ) ;
  run_clang clang_command frontend

let cc1_capture clang_cmd =
  let source_path =
    let root = Unix.getcwd () in
    let orig_argv = ClangCommand.get_orig_argv clang_cmd in
    (* the source file is always the last argument of the original -cc1 clang command *)
    Utils.filename_to_absolute ~root orig_argv.(Array.length orig_argv - 1)
  in
  L.(debug Capture Quiet) "@\n*** Beginning capture of file %s ***@\n" source_path ;
  if Config.equal_analyzer Config.analyzer Config.CompileOnly
     || not Config.skip_analysis_in_path_skips_compilation
        && CLocation.is_file_blacklisted source_path
  then (
    L.(debug Capture Quiet) "@\n Skip the analysis of source file %s@\n@\n" source_path ;
    (* We still need to run clang, but we don't have to attach the plugin. *)
    run_clang (ClangCommand.command_to_run clang_cmd) Utils.consume_in )
  else if Config.skip_analysis_in_path_skips_compilation
          && CLocation.is_file_blacklisted source_path
  then (
    L.(debug Capture Quiet) "@\n Skip compilation and analysis of source file %s@\n@\n" source_path ;
    () )
  else (
    ( match Config.clang_biniou_file with
    | Some fname
     -> run_and_validate_clang_frontend (`File fname)
    | None
     -> run_plugin_and_frontend source_path
          (fun chan_in -> run_and_validate_clang_frontend (`Pipe chan_in))
          clang_cmd ) ;
    () )

let capture clang_cmd =
  if ClangCommand.can_attach_ast_exporter clang_cmd then
    (* this command compiles some code; replace the invocation of clang with our own clang and
       plugin *)
    cc1_capture clang_cmd
  else
    (* Non-compilation (eg, linking) command. Run the command as-is. It will not get captured
       further since `clang -### ...` will only output commands that invoke binaries using their
       absolute paths. *)
    let command_to_run = ClangCommand.command_to_run clang_cmd in
    L.(debug Capture Quiet) "Running non-cc command without capture: %s@\n" command_to_run ;
    run_clang command_to_run Utils.consume_in
