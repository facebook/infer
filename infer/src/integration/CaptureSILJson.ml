(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Capture module for the json analysis in the capture phase *)

open! IStd
module L = Logging

 
let capture ~changed_files ~cfg_json ~tenv_json =
  let tenv = InferAnalyzeJson.parse_tenv (Yojson.Safe.from_file tenv_json) in
  let cfg = InferAnalyzeJson.parse_cfg (Yojson.Safe.from_file cfg_json) in
  Tenv.store_global tenv ;
  Language.curr_language := Language.CIL ;
  match changed_files with
  | None ->
    let source_file = SourceFile.create ~warn_on_error:false "./Program.cs" in
    SourceFiles.add source_file cfg Tenv.Global None ;
  | Some changed_files_set ->
    let add_file sf = SourceFiles.add sf cfg Tenv.Global None in
    SourceFile.Set.iter add_file changed_files_set ;
  ()