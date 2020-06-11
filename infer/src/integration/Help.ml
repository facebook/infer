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

let show_issue_types _ = assert false

let write_website ~website_root:_ = assert false
