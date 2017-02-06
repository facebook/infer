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
val invoke_set_of_checkers_on_node : CLintersContext.context -> Ctl_parser_types.ast_node -> unit

val expand_checkers : CTL.ctl_checker list -> CTL.ctl_checker list

val make_condition_issue_desc_pair :
  CTL.ctl_checker list -> unit
