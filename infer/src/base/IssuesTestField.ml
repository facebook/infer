(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | BugType
  | Qualifier
  | Severity
  | Suggestion
  | Bucket
  | Line
  | Column
  | Procedure
  | ProcedureStartLine
  | File
  | BugTrace
  | Key
  | Hash
  | LineOffset
  | QualifierContainsPotentialExceptionNote
  | TaintExtra
  | TransitiveCalleesExtra
[@@deriving equal]

let all_symbols =
  [ ("bug_type", BugType)
  ; ("bucket", Bucket)
  ; ("qualifier", Qualifier)
  ; ("suggestion", Suggestion)
  ; ("severity", Severity)
  ; ("line", Line)
  ; ("column", Column)
  ; ("procedure", Procedure)
  ; ("procedure_start_line", ProcedureStartLine)
  ; ("file", File)
  ; ("bug_trace", BugTrace)
  ; ("key", Key)
  ; ("hash", Hash)
  ; ("line_offset", LineOffset)
  ; ("qualifier_contains_potential_exception_note", QualifierContainsPotentialExceptionNote)
  ; ("taint_extra", TaintExtra)
  ; ("transitive_callees_extra", TransitiveCalleesExtra) ]
