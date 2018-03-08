(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format

let log_caught_exception (trans_unit_ctx: CFrontend_config.translation_unit_context) exception_type
    exception_triggered_location (source_location_start, source_location_end) ast_node =
  let caught_exception =
    EventLogger.FrontendException
      { exception_type
      ; source_location_start= CLocation.clang_to_sil_location trans_unit_ctx source_location_start
      ; source_location_end= CLocation.clang_to_sil_location trans_unit_ctx source_location_end
      ; exception_triggered_location
      ; ast_node
      ; lang= CFrontend_config.string_of_clang_lang trans_unit_ctx.lang }
  in
  EventLogger.log caught_exception
