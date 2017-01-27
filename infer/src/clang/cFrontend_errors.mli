(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(* Module for warnings detected at translation time by the frontend *)

(* Run frontend checkers on an AST node *)
val invoke_set_of_checkers_on_node : CLintersContext.context -> CTL.ast_node -> unit

val expand_checkers : Ctl_parser_types.ctl_checker list -> Ctl_parser_types.ctl_checker list

val make_condition_issue_desc_pair :
  Ctl_parser_types.ctl_checker list -> unit
