(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Check an implicit cast when returning an immutable collection from a method whose type is
    mutable. *)
let check_immutable_cast analysis_data proc_desc typ_expected typ_found_opt loc : unit =
  match typ_found_opt with
  | Some typ_found -> (
      let casts =
        [ ("java.util.List", "com.google.common.collect.ImmutableList")
        ; ("java.util.Map", "com.google.common.collect.ImmutableMap")
        ; ("java.util.Set", "com.google.common.collect.ImmutableSet") ]
      in
      let in_casts expected given =
        List.exists
          ~f:(fun (x, y) ->
            String.equal (Typ.Name.name expected) x && String.equal (Typ.Name.name given) y )
          casts
      in
      match
        (PatternMatch.type_get_class_name typ_expected, PatternMatch.type_get_class_name typ_found)
      with
      | Some name_expected, Some name_given ->
          if in_casts name_expected name_given then
            let description =
              Format.asprintf
                "Method %s returns %a but the return type is %a. Make sure that users of this \
                 method do not try to modify the collection."
                (Procname.to_simplified_string (Procdesc.get_proc_name proc_desc))
                Typ.Name.pp name_given Typ.Name.pp name_expected
            in
            let issue_type = IssueType.checkers_immutable_cast in
            EradicateReporting.report_error analysis_data ImmutableCast
              (NullsafeIssue.make ~loc ~description ~severity:Warning ~issue_type ~field_name:None)
      | _ ->
          () )
  | None ->
      ()


let analyze analysis_data =
  Eradicate.analyze_for_immutable_cast_checker (check_immutable_cast analysis_data) analysis_data
