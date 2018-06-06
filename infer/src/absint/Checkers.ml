(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for user-defined checkers. *)

module ST = struct
  let report_error tenv proc_name proc_desc kind loc ?(field_name= None) ?(origin_loc= None)
      ?(exception_kind= fun k d -> Exceptions.Checkers (k, d)) ?(always_report= false) description =
    let localized_description =
      Localise.custom_desc description [("always_report", string_of_bool always_report)]
    in
    let exn = exception_kind kind localized_description in
    let suppressed = Reporting.is_suppressed tenv proc_desc kind ~field_name in
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
    if not suppressed then Reporting.log_error_deprecated proc_name ~loc ~ltr:trace exn
end
