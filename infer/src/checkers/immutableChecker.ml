(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

(** Check an implicit cast when returning an immutable collection from a method whose type is mutable. *)
let check_immutable_cast tenv curr_pname curr_pdesc typ_expected typ_found_opt loc : unit =
  match typ_found_opt with
  | Some typ_found ->
      begin
        let casts =
          [
            "java.util.List", "com.google.common.collect.ImmutableList";
            "java.util.Map", "com.google.common.collect.ImmutableMap";
            "java.util.Set", "com.google.common.collect.ImmutableSet"
          ] in
        let in_casts expected given =
          IList.exists (fun (x, y) ->
              string_equal (Typename.name expected) x && string_equal (Typename.name given) y
            ) casts in
        match PatternMatch.type_get_class_name typ_expected,
              PatternMatch.type_get_class_name typ_found with
        | Some name_expected, Some name_given ->
            if in_casts name_expected name_given then
              begin
                let description =
                  Format.asprintf
                    "Method %s returns %a but the return type is %a. \
                     Make sure that users of this method do not try to modify the collection."
                    (Procname.to_simplified_string curr_pname)
                    Typename.pp name_given
                    Typename.pp name_expected in
                Checkers.ST.report_error tenv
                  curr_pname
                  curr_pdesc
                  "CHECKERS_IMMUTABLE_CAST"
                  loc
                  description
              end
        | _ -> ()
      end
  | None -> ()

let callback_check_immutable_cast ({Callbacks.tenv} as args) =
  Eradicate.callback_check_return_type (check_immutable_cast tenv) args
