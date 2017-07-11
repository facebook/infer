(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let create_fake_jsonbug ?(bug_class= "bug_class") ?(kind= "kind") ?(bug_type= "bug_type")
    ?(qualifier= "qualifier") ?(severity= "severity") ?(visibility= "visibility") ?(line= 1)
    ?(column= 1) ?(procedure= "procedure") ?(procedure_id= "procedure_id")
    ?(procedure_start_line= 1) ?(file= "file/at/a/certain/path.java") ?(bug_trace= []) ?(key= 1234)
    ?(qualifier_tags= []) ?(hash= 1) ?(dotty= None) ?(infer_source_loc= None)
    ?(linters_def_file= Some "file/at/certain/path.al") ?doc_url () : Jsonbug_t.jsonbug =
  { bug_class
  ; kind
  ; bug_type
  ; qualifier
  ; severity
  ; visibility
  ; line
  ; column
  ; procedure
  ; procedure_id
  ; procedure_start_line
  ; file
  ; bug_trace
  ; key
  ; qualifier_tags
  ; hash
  ; dotty
  ; infer_source_loc
  ; bug_type_hum= kind
  ; linters_def_file
  ; doc_url
  ; traceview_id= None }

let pp_diff_of_int_list group_name fmt (expected, actual) =
  Format.fprintf fmt "[%s]: Expected: [%a] Found: [%a]" group_name
    (Pp.comma_seq Format.pp_print_int) expected (Pp.comma_seq Format.pp_print_int) actual

(* Sort hashes to make things easier to compare *)
let sorted_hashes_of_issues issues =
  let hash i = i.Jsonbug_t.hash in
  List.sort ~cmp:Int.compare (List.map ~f:hash issues)
