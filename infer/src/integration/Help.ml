(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let list_checkers () = assert false

let list_issue_types () =
  L.progress
    "@[Format:@\n\
     Issue type unique identifier:Human-readable version:Visibility:Default \
     severity:Enabled:Checker:Documentation URL (AL only):Linters definition file (AL only)@\n\
     @\n\
     @]%!" ;
  L.result "@[<v>" ;
  IssueType.all_issues ()
  |> List.iter
       ~f:(fun ({ IssueType.unique_id
                ; checker
                ; visibility
                ; user_documentation=
                    _
                    (* do not show this as this can be a big multi-line string and not tool-friendly *)
                ; default_severity
                ; enabled
                ; hum
                ; doc_url
                ; linters_def_file }[@warning "+9"])
          ->
         L.result "%s:%s:%s:%s:%b:%s:%s:%s@;" unique_id hum
           (IssueType.string_of_visibility visibility)
           (IssueType.string_of_severity default_severity)
           enabled (Checker.get_id checker)
           (Option.value ~default:"" doc_url)
           (Option.value ~default:"" linters_def_file) ) ;
  L.result "@]%!"


let show_checkers _ = assert false

let show_issue_type (issue_type : IssueType.t) =
  L.result "%s (unique ID: %s)@\n" issue_type.hum issue_type.unique_id ;
  L.result "Reported by %s@\n" (Checker.get_id issue_type.checker) ;
  match issue_type.user_documentation with
  | None ->
      L.result "No documentation@\n"
  | Some documentation ->
      L.result "Documentation:@\n  @[" ;
      let first_line = ref true in
      String.split_lines documentation
      |> List.iter ~f:(fun line ->
             if not !first_line then L.result "@\n" ;
             first_line := false ;
             L.result "%s" line ) ;
      L.result "@]@\n"


let show_issue_types issue_types =
  L.result "@[" ;
  List.iter ~f:show_issue_type issue_types ;
  L.result "@]%!"


let write_website ~website_root:_ = assert false
