(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module ClosureHashtbl : Caml.Map.S with type key = CTL.t

type context_linter_map = bool ClosureHashtbl.t

val init_global_nodes_valuation : unit -> unit

val init_active_map : CFrontend_errors.linter list -> bool ClosureHashtbl.t

val update_linter_context_map :
     CFrontend_errors.linter list
  -> Ctl_parser_types.ast_node
  -> context_linter_map
  -> context_linter_map

val build_valuation :
     CFrontend_errors.linter list
  -> Ctl_parser_types.ast_node
  -> CLintersContext.context
  -> context_linter_map
  -> unit

val is_decl_allowed : CLintersContext.context -> Clang_ast_t.decl -> bool
