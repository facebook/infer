(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

type t = {issue_type: IssueType.t; description: Localise.error_desc; ocaml_pos: L.ocaml_pos option}

(** pretty print an error *)
let pp_err ?severity_override loc issue_type desc ocaml_pos_opt fmt () =
  let severity = Option.value severity_override ~default:issue_type.IssueType.default_severity in
  let kind =
    IssueType.string_of_severity
      (if IssueType.equal_severity severity Info then Warning else severity)
  in
  F.fprintf fmt "%a:%d: %s: %a %a%a@\n" SourceFile.pp loc.Location.file loc.Location.line kind
    IssueType.pp issue_type Localise.pp_error_desc desc L.pp_ocaml_pos_opt ocaml_pos_opt
