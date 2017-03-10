(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type linter = {
  condition : CTL.t;
  issue_desc : CIssue.issue_desc;
  def_file : string option;
}

(* List of checkers that will be filled after parsing them from a file *)
val parsed_linters : linter list ref

(* Module for warnings detected at translation time by the frontend *)

(* Run frontend checkers on an AST node *)
val invoke_set_of_checkers_on_node : CLintersContext.context -> Ctl_parser_types.ast_node -> unit

val expand_checkers : CTL.ctl_checker list -> CTL.ctl_checker list

val create_parsed_linters : string -> CTL.ctl_checker list -> linter list

val remove_new_lines : string -> string
