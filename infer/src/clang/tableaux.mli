(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module ClosureHashtbl : Caml.Map.S with type key = CTL.t

type context_linter_map = bool ClosureHashtbl.t

val init_global_nodes_valuation : unit -> unit

val init_active_map : unit -> bool ClosureHashtbl.t

val update_linter_context_map :
  Ctl_parser_types.ast_node -> context_linter_map -> context_linter_map

val build_valuation :
  Ctl_parser_types.ast_node -> CLintersContext.context -> context_linter_map -> unit

val is_decl_allowed : CLintersContext.context -> Clang_ast_t.decl -> bool
