(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val report_error :
     IntraproceduralAnalysis.t
  -> Checker.t
  -> IssueType.t
  -> Location.t
  -> ?field_name:Fieldname.t option
  -> severity:Exceptions.severity
  -> string
  -> unit
