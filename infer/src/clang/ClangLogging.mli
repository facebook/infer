(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val log_caught_exception :
  CFrontend_config.translation_unit_context -> string -> Logging.ocaml_pos
  -> Clang_ast_t.source_location * Clang_ast_t.source_location -> string option -> unit

val log_broken_cfg :
  broken_node:[`Join | `Other] -> Procdesc.t -> Logging.ocaml_pos -> lang:string -> unit
