(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** An issue about to be reported to the user *)
type t =
  { issue_type: IssueType.t
  ; description: Localise.error_desc
  ; ocaml_pos: L.ocaml_pos option  (** location in the infer source code *) }

val pp_err :
     ?severity_override:IssueType.severity
  -> Location.t
  -> IssueType.t
  -> Localise.error_desc
  -> Logging.ocaml_pos option
  -> Format.formatter
  -> unit
  -> unit
(** pretty print an error *)
