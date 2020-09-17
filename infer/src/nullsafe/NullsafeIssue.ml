(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { issue_type: IssueType.t
  ; description: string  (** Human-readable description *)
  ; loc: Location.t  (** Where to report the error *)
  ; severity: IssueType.severity }

let make ~issue_type ~description ~loc ~severity = {issue_type; description; loc; severity}

let get_issue_type {issue_type} = issue_type

let get_description {description} = description

let get_loc {loc} = loc

let get_severity {severity} = severity
