(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Eradicate-based user-defined checkers. *)

let report_error tenv proc_name proc_desc kind loc ?(field_name = None) ?(origin_loc = None)
    ?(exception_kind = fun k d -> Exceptions.Checkers (k, d)) ?(severity = Exceptions.Warning)
    description =
  let suppressed = Reporting.is_suppressed tenv proc_desc kind ~field_name in
  if not suppressed then
    let localized_description = Localise.verbatim_desc description in
    let exn = exception_kind kind localized_description in
    let trace =
      let origin_elements =
        match origin_loc with
        | Some oloc ->
            [Errlog.make_trace_element 0 oloc "origin" []]
        | None ->
            []
      in
      origin_elements @ [Errlog.make_trace_element 0 loc description []]
    in
    Reporting.log_issue_deprecated_using_state severity proc_name ~loc ~ltr:trace exn
