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

val filter_parsed_linters : linter list -> linter list

val linters_to_string : linter list -> string

(* map used to expand macro. It maps a formula id to a triple
   (visited, parameters, definition).
   Visited is used during the expansion phase to understand if the
   formula was already expanded and, if yes we have a cyclic definifion *)
type macros_map = (bool * ALVar.t list * CTL.t) ALVar.FormulaIdMap.t

(* List of checkers that will be filled after parsing them from a file *)
val parsed_linters : linter list ref

(* Module for warnings detected at translation time by the frontend *)

(* Run frontend checkers on an AST node *)
val invoke_set_of_checkers_on_node : CLintersContext.context -> Ctl_parser_types.ast_node -> unit

val build_macros_map : CTL.clause list -> macros_map

val expand_checkers : macros_map -> CTL.ctl_checker list -> CTL.ctl_checker list

val create_parsed_linters : string -> CTL.ctl_checker list -> linter list

val remove_new_lines : string -> string
