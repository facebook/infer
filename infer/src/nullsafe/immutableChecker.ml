(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Check an implicit cast when returning an immutable collection from a method whose type is mutable. *)
let check_immutable_cast tenv curr_pname curr_pdesc typ_expected typ_found_opt loc : unit =
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
                (Typ.Procname.to_simplified_string curr_pname)
                Typ.Name.pp name_given Typ.Name.pp name_expected
            in
            EradicateCheckers.report_error tenv curr_pname curr_pdesc
              IssueType.checkers_immutable_cast loc description
      | _ ->
          () )
  | None ->
      ()


let callback_check_immutable_cast ({Callbacks.tenv} as args) =
  Eradicate.callback_check_return_type (check_immutable_cast tenv) args
