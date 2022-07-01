(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 (** Capture module for the json analysis after the capture phase *)

open! IStd
module L = Logging

 
let capture ~changed_files ~json_cfg ~json_tenv =
  InferAnalyze.register_active_checkers () ;
  let reanalyzed_procname_list = ref [] in
  Printexc.record_backtrace true ;
  let tenv = InferAnalyzeJson.parse_tenv (Yojson.Safe.from_file json_tenv) in
  let cfg = InferAnalyzeJson.parse_cfg (Yojson.Safe.from_file json_cfg) in
  let source_file = SourceFile.create ~warn_on_error:false "./Program.cs" in
  Tenv.store_global tenv ;
  Language.curr_language := Language.CIL ;
  SourceFiles.add source_file cfg Tenv.Global None ;
  (* L.progress "%a@." Cfg.pp_proc_signatures cfg ; *)
  let exe_env = Exe_env.mk () in
  Ondemand.analyze_file exe_env source_file ;
  if Config.write_html then Printer.write_all_html_files source_file ;
  ()