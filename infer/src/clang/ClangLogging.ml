(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let log_frontend_exception (trans_unit_ctx : CFrontend_config.translation_unit_context)
    exception_type exception_triggered_location (source_location_start, source_location_end)
    ast_node =
  let frontend_exception =
    EventLogger.FrontendException
      { exception_type
      ; source_location_start=
          CLocation.clang_to_sil_location trans_unit_ctx.source_file source_location_start
      ; source_location_end=
          CLocation.clang_to_sil_location trans_unit_ctx.source_file source_location_end
      ; exception_triggered_location
      ; ast_node
      ; lang= CFrontend_config.string_of_clang_lang trans_unit_ctx.lang }
  in
  EventLogger.log frontend_exception


let log_caught_exception trans_unit_ctx exception_type exception_triggered_location source_range
    ast_node =
  log_frontend_exception trans_unit_ctx exception_type exception_triggered_location source_range
    ast_node


let log_unexpected_decl trans_unit_ctx exception_triggered_location source_range ast_node =
  log_frontend_exception trans_unit_ctx "Skipped declaration inside a class"
    exception_triggered_location source_range ast_node


let log_broken_cfg ~broken_node procdesc exception_triggered_location ~lang =
  let proc_location = Procdesc.get_loc procdesc in
  let exception_type =
    match broken_node with `Other -> "Broken CFG" | `Join -> "Broken CFG at join node"
  in
  let cfg_exception =
    EventLogger.FrontendException
      { source_location_start= proc_location
      ; source_location_end= proc_location
      ; ast_node= None
      ; exception_triggered_location
      ; exception_type
      ; lang }
  in
  EventLogger.log cfg_exception
