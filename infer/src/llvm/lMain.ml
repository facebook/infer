(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open Lexing
open Printf

exception UsageError of string

let init_global_state source_file =
  Sil.curr_language := Sil.C_CPP;
  DB.current_source := source_file;
  DB.Results_dir.init ();
  Ident.reset_name_generator ();
  Utils.SymOp.reset_total ();
  let nLOC = Utils.FileLOC.file_get_loc (DB.source_file_to_string source_file) in
  Config.nLOC := nLOC

let store_icfg tenv cg cfg source_file =
  let source_dir = DB.source_dir_from_source_file !DB.current_source in
  let cfg_file = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cg_file = DB.source_dir_get_internal_file source_dir ".cg" in
  Cfg.add_removetemps_instructions cfg;
  Preanal.doit cfg tenv;
  Cfg.add_abstraction_instructions cfg;
  Cg.store_to_file cg_file cg;
  Cfg.store_cfg_to_file cfg_file true cfg;
  (* debug *)
  Config.write_dotty := true;
  Config.print_types := true;
  Dotty.print_icfg_dotty cfg [];
  Cg.save_call_graph_dotty None Specs.get_specs cg

let store_tenv tenv =
  let tenv_filename = DB.global_tenv_fname () in
  if DB.file_exists tenv_filename then DB.file_remove tenv_filename;
  Sil.store_tenv_to_file tenv_filename tenv

let () = try
    if Array.length Sys.argv < 2 then
      raise (UsageError ("Missing source file as first command line argument."))
    else
      let filename = Sys.argv.(1) in
      let source_file = DB.abs_source_file_from_path filename in
      let () = init_global_state source_file in
      let lexbuf = Lexing.from_channel (open_in filename) in
      let prog = LParser.program LLexer.token lexbuf in
      (* let pretty = LPretty.pretty_prog prog in *)
      let (cfg, cg, tenv) = LTrans.trans_program prog in
      store_icfg tenv cg cfg source_file; store_tenv tenv
  with
  | UsageError msg -> print_string ("Usage error: " ^ msg ^ "\n")
