(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

let arg_desc =
  let options_to_keep = ["-results_dir"; "-project_root"] in
  let desc =
    arg_desc_filter options_to_keep base_arg_desc @
    [
      "-c",
      Arg.String (fun cfile -> LConfig.source_filename := Some cfile),
      Some "cfile",
      "C/C++ file being translated"
      ;
      "-debug",
      Arg.Unit (fun _ -> LConfig.debug_mode := true),
      None,
      "Enables debug mode"
      ;
    ] in
  Arg.create_options_desc false "Parsing Options" desc

let usage = "Usage: InferLLVM -c <cfile> [options]\n"

let print_usage_exit () =
  Arg.usage arg_desc usage;
  exit(1)

let register_perf_stats_report source_file =
  let stats_dir = Filename.concat !Config.results_dir Config.frontend_stats_dir_name in
  let abbrev_source_file = DB.source_file_encoding source_file in
  let stats_file = Config.perf_stats_prefix ^ "_" ^ abbrev_source_file ^ ".json" in
  DB.create_dir !Config.results_dir ;
  DB.create_dir stats_dir ;
  PerfStats.register_report_at_exit (Filename.concat stats_dir stats_file)

let init_global_state source_filename =
  Config.curr_language := Config.C_CPP;
  begin match !Config.project_root with
    | None -> DB.current_source := DB.abs_source_file_from_path source_filename
    | Some project_root ->
        DB.current_source := DB.rel_source_file_from_abs_path project_root
            (filename_to_absolute source_filename)
  end;
  register_perf_stats_report !DB.current_source ;
  DB.Results_dir.init ();
  Ident.NameGenerator.reset ();
  Config.nLOC := FileLOC.file_get_loc source_filename

let store_icfg tenv cg cfg =
  let source_dir = DB.source_dir_from_source_file !DB.current_source in
  let get_internal_file = DB.source_dir_get_internal_file source_dir in
  let cg_file = get_internal_file ".cg" in
  let cfg_file = get_internal_file ".cfg" in
  Preanal.doit cfg cg tenv;
  Cg.store_to_file cg_file cg;
  Cfg.store_cfg_to_file cfg_file true cfg;
  if !LConfig.debug_mode then
    begin
      Config.write_dotty := true;
      Config.print_types := true;
      Dotty.print_icfg_dotty cfg [];
      Cg.save_call_graph_dotty None Specs.get_specs cg
    end

let store_tenv tenv =
  let tenv_filename = DB.global_tenv_fname () in
  if DB.file_exists tenv_filename then DB.file_remove tenv_filename;
  Tenv.store_to_file tenv_filename tenv

let () =
  Arg.parse "INFERLLVM_ARGS" arg_desc (fun _ -> ()) usage;
  begin match !LConfig.source_filename with
    | None -> print_usage_exit ()
    | Some source_filename -> init_global_state source_filename
  end;
  let lexbuf = Lexing.from_channel stdin in
  let prog = LParser.program LLexer.token lexbuf in
  let (cfg, cg, tenv) = LTrans.trans_program prog in
  store_icfg tenv cg cfg;
  store_tenv tenv
