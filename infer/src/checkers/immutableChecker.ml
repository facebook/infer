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
let check_immutable_cast curr_pname curr_pdesc typ_expected typ_found_opt loc : unit =
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
          IList.exists (fun (x, y) -> Mangled.from_string x = expected && Mangled.from_string y = given) casts in
        match PatternMatch.type_get_class_name typ_expected,
              PatternMatch.type_get_class_name typ_found with
        | Some name_expected, Some name_given ->
            if in_casts name_expected name_given then
              begin
                let description =
                  Printf.sprintf
                    "Method %s returns %s but the return type is %s. Make sure that users of this method do not try to modify the collection."
                    (Procname.to_simplified_string curr_pname)
                    (Mangled.to_string name_given)
                    (Mangled.to_string name_expected) in
                Checkers.ST.report_error
                  curr_pname
                  curr_pdesc
                  "CHECKERS_IMMUTABLE_CAST"
                  loc
                  description
              end
        | _ -> ()
      end
  | None -> ()

let callback_check_immutable_cast =
  Eradicate.callback_check_return_type check_immutable_cast
