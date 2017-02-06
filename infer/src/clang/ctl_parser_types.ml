(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(* Types used by the ctl parser *)

(** the kind of AST nodes where formulas are evaluated *)
type ast_node =
  | Stmt of Clang_ast_t.stmt
  | Decl of Clang_ast_t.decl


let infer_prefix = "__infer_ctl_"
let formula_id_const = infer_prefix ^ "formula_id__"
let report_when_const = "report_when"
let message_const = "message"
let suggestion_const = "suggestion"
let severity_const = "severity"
let mode_const = "mode"
