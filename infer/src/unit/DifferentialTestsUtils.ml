(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let create_fake_jsonbug ?(bug_type = "bug_type") ?(qualifier = "qualifier")
    ?(suggestion = Some "suggestion") ?(severity = "severity") ?(category = "category") ?(line = 1)
    ?(column = 1) ?(procedure = "procedure") ?(procedure_start_line = 1)
    ?(file = "file/at/a/certain/path.java") ?(bug_trace = []) ?(key = "File|method|TYPE")
    ?(node_key = Some "1234") ?(hash = "1") ?(dotty = None) ?(infer_source_loc = None) () :
    Jsonbug_t.jsonbug =
  { bug_type
  ; qualifier
  ; suggestion
  ; severity
  ; category
  ; line
  ; column
  ; procedure
  ; procedure_start_line
  ; file
  ; bug_trace
  ; bug_trace_length= 0
  ; bug_trace_max_depth= 0
  ; node_key
  ; key
  ; hash
  ; dotty
  ; infer_source_loc
  ; bug_type_hum= bug_type
  ; traceview_id= None
  ; censored_reason= None
  ; access= None
  ; extras= None }


let pp_diff_of_list ~pp group_name fmt (expected, actual) =
  Format.fprintf fmt "[%s]: Expected: [%a] Found: [%a]" group_name (Pp.comma_seq pp) expected
    (Pp.comma_seq pp) actual


let pp_diff_of_string_list = pp_diff_of_list ~pp:Format.pp_print_string

let pp_diff_of_int_list = pp_diff_of_list ~pp:Format.pp_print_int

(* Sort hashes to make things easier to compare *)
let sorted_hashes_of_issues (issues : Jsonbug_t.report) =
  let hash (i : Jsonbug_t.jsonbug) = i.Jsonbug_t.hash in
  List.sort ~compare:String.compare (List.rev_map ~f:hash issues)
