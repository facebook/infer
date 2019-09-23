(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

let export_changed_functions trans_unit_ctx ast_decl =
  let source_file = trans_unit_ctx.CFrontend_config.source_file in
  let f () =
    if Config.export_changed_functions then
      let clang_range_map = AstToRangeMap.process_ast ast_decl source_file in
      TestDeterminator.compute_and_emit_relevant_methods ~clang_range_map ~source_file
  in
  let call_f () =
    CFrontend_errors.protect trans_unit_ctx
      ~recover:(fun () -> ())
      ~pp_context:(fun f () -> F.fprintf f "Error when processing %a" SourceFile.pp source_file)
      ~f
  in
  call_f ()


let process_ast trans_unit_ctx ast_decl =
  if Config.export_changed_functions then export_changed_functions trans_unit_ctx ast_decl
