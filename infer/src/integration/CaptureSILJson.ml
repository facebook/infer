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
    let save_proc _ proc_desc =
      let attributes = Procdesc.get_attributes proc_desc in
      let source_file = attributes.loc.Location.file in
      let loc = attributes.loc in
      let attributes' =
        let loc' = if Location.equal loc Location.dummy then {loc with file= source_file} else loc in
        {attributes with loc= loc'; translation_unit= source_file}
      in
      Procdesc.set_attributes proc_desc attributes' ;
      Attributes.store ~proc_desc:(Option.some_if attributes.is_defined proc_desc) attributes'
    in
    Procname.Hash.iter save_proc cfg ;
  | Some changed_files_set ->
    let add_file sf = SourceFiles.add sf cfg Tenv.Global None in
    SourceFile.Set.iter add_file changed_files_set ;
  ()