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
  | NullsafeExtra
[@@deriving equal]

val all_symbols : (string * t) list
