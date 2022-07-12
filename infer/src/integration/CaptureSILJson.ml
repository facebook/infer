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
  let source_file = SourceFile.create ~warn_on_error:false "./Program.cs" in
  Tenv.store_global tenv ;
  Language.curr_language := Language.CIL ;
  SourceFiles.add source_file cfg Tenv.Global None ;
  ()