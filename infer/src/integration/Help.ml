(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let docs_dir = "docs"

let mk_markdown_docs_path ~website_root ~basename = website_root ^/ docs_dir ^/ basename ^ ".md"

let escape_double_quotes s = String.substr_replace_all s ~pattern:"\"" ~with_:"\\\""

let all_issues_basename = "all-issue-types"

let basename_checker_prefix = "checker-"

let basename_of_checker {Checker.id} = basename_checker_prefix ^ id

let abs_url_of_issue_type unique_id =
  Printf.sprintf "/%s/next/%s#%s" docs_dir all_issues_basename (String.lowercase unique_id)


let get_checker_web_documentation (checker : Checker.config) =
  match checker.kind with
  | UserFacing {title; markdown_body} ->
      Some (title, markdown_body, None)
  | UserFacingDeprecated {title; markdown_body; deprecation_message} ->
      Some (title, markdown_body, Some deprecation_message)
  | Internal | Exercise ->
      None


let markdown_one_issue f (issue_type : IssueType.t) =
  F.fprintf f "## %s@\n@\n" issue_type.unique_id ;
  let checker_config = Checker.config issue_type.checker in
  if Option.is_none (get_checker_web_documentation checker_config) then
    L.die InternalError
      "Checker %s can report user-facing issue %s but is not of type UserFacing in \
       src/base/Checker.ml. Please fix!"
      checker_config.id issue_type.unique_id ;
  F.fprintf f "Reported as \"%s\" by [%s](/%s/next/%s).@\n@\n" issue_type.hum checker_config.id
    docs_dir
    (basename_of_checker checker_config) ;
  match issue_type.user_documentation with
  | None ->
      ()
  | Some documentation ->
      F.pp_print_string f documentation


let pp_checker_name f checker = F.pp_print_string f (Checker.get_id checker)

let string_of_checker_kind (kind : Checker.kind) =
  match kind with
  | Exercise ->
      "Exercise"
  | Internal ->
      "Internal"
  | UserFacing _ ->
      "UserFacing"
  | UserFacingDeprecated _ ->
      "UserFacingDeprecated"


let string_of_support (support : Checker.support) =
  match support with NoSupport -> "No" | ExperimentalSupport -> "Experimental" | Support -> "Yes"


let list_checkers () =
  L.progress
    "@[Format:@\nChecker ID:kind:clang support:Java support:enabled by default:activates@\n@\n@]%!" ;
  L.result "@[<v>" ;
  Checker.all
  |> List.iter ~f:(fun checker ->
         let ({ Checker.id
              ; kind
              ; support
              ; short_documentation= _ (* only list in [show_checkers] *)
              ; cli_flags= _ (* only list in [show_checkers] *)
              ; enabled_by_default
              ; activates }[@warning "+9"]) =
           Checker.config checker
         in
         L.result "%s:%s:%s:%s:%b:%a@;" id (string_of_checker_kind kind)
           (string_of_support (support Clang))
           (string_of_support (support Java))
           enabled_by_default (Pp.seq ~sep:"," pp_checker_name) activates ) ;
  L.result "@]%!"


let all_issues_header =
  {|---
title: List of all issue types
---

Here is an overview of the issue types currently reported by Infer.

|}


let all_issues =
  lazy
    ( IssueType.all_issues ()
    |> List.filter ~f:(fun {IssueType.user_documentation} -> Option.is_some user_documentation)
    |> List.sort ~compare:(fun {IssueType.unique_id= id1} {IssueType.unique_id= id2} ->
           String.compare id1 id2 ) )


let all_issues_website ~website_root =
  let issues_to_document = Lazy.force all_issues in
  Utils.with_file_out (mk_markdown_docs_path ~website_root ~basename:all_issues_basename)
    ~f:(fun out_channel ->
      let f = F.formatter_of_out_channel out_channel in
      F.fprintf f "%s@\n%a@\n%!" all_issues_header
        (Pp.seq ~sep:"\n" markdown_one_issue)
        issues_to_document )


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


let pp_checker f checker =
  let ({Checker.id; kind; support; short_documentation; cli_flags; enabled_by_default; activates}[@warning
                                                                                                   "+9"])
      =
    Checker.config checker
  in
  F.fprintf f
    "@[<v>id=%s@\nkind=%s@\nsupport={clang:%s; Java:%s}@\nenabled_by_default=%b@\n@\n%s@\n" id
    (string_of_checker_kind kind)
    (string_of_support (support Clang))
    (string_of_support (support Java))
    enabled_by_default short_documentation ;
  ( match cli_flags with
  | None ->
      F.fprintf f
        "Cannot be activated from the command line (only used indirectly by other checkers)."
  | Some _ ->
      F.fprintf f "@\nActivated with --%s@\n" id ) ;
  if not (List.is_empty activates) then
    F.fprintf f
      "@\n\
       Depends on the following checker%s, that will automatically be activated together with %s: \
       %a@\n"
      (if List.length activates > 1 then "s" else "")
      id
      (Pp.seq ~sep:", " pp_checker_name)
      activates ;
  ()


let show_checkers checkers = L.result "%a" (Pp.seq ~sep:"\n" pp_checker) checkers

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


let mk_checkers_json checkers_base_filenames =
  `Assoc
    [ ( "README"
      , `String
          (Printf.sprintf
             "This is a %cgenerated file, run `make doc-publish` from the root of the infer \
              repository to generate it"
             (* avoid tooling thinking this source file itself is generated because of the string _at_generated appearing in it *)
             '@') )
    ; ( "doc_entries"
      , `List
          ( `String all_issues_basename
          :: List.map checkers_base_filenames ~f:(fun filename -> `String filename) ) ) ]


(** Writes an index of all the checkers documentation pages. Must correspond to all the pages
    written in docs/! *)
let write_checkers_json ~path =
  let json =
    List.filter_map Checker.all ~f:(fun checker ->
        let config = Checker.config checker in
        if Option.is_some (get_checker_web_documentation config) then
          Some (basename_of_checker config)
        else None )
    |> mk_checkers_json
  in
  Utils.with_file_out path ~f:(fun out_channel ->
      Yojson.pretty_to_channel ~std:true out_channel json )


let pp_checker_webpage_header f ~title ~short_documentation =
  F.fprintf f {|---
title: "%s"
description: "%s"
---

%s

|} (escape_double_quotes title)
    (escape_double_quotes short_documentation)
    short_documentation


let pp_checker_deprecation_message f message =
  F.fprintf f "**\\*\\*\\*DEPRECATED\\*\\*\\*** %s@\n@\n" message


let pp_checker_cli_flags f checker_config =
  F.fprintf f "Activate with `--%s`.@\n@\n" checker_config.Checker.id


let pp_checker_language_support f support =
  F.fprintf f "Supported languages:@\n" ;
  List.iter Language.all ~f:(fun language ->
      F.fprintf f "- %s: %s@\n" (Language.to_string language) (string_of_support (support language)) ) ;
  F.pp_print_newline f ()


let pp_checker_issue_types f checker =
  F.fprintf f "@\n@\n## List of Issue Types@\n@\n" ;
  F.fprintf f "The following issue types are reported by this checker:@\n" ;
  let checker_issues =
    List.filter (Lazy.force all_issues) ~f:(fun {IssueType.checker= issue_checker} ->
        Checker.equal issue_checker checker )
  in
  let pp_issue f {IssueType.unique_id} =
    F.fprintf f "- [%s](%s)@\n" unique_id (abs_url_of_issue_type unique_id)
  in
  List.iter checker_issues ~f:(pp_issue f)


let write_checker_webpage ~website_root (checker : Checker.t) =
  let checker_config = Checker.config checker in
  match get_checker_web_documentation checker_config with
  | None ->
      ()
  | Some (title, markdown_body, deprecated_opt) ->
      Utils.with_file_out
        (mk_markdown_docs_path ~website_root ~basename:(basename_of_checker checker_config))
        ~f:(fun out_channel ->
          let f = F.formatter_of_out_channel out_channel in
          pp_checker_webpage_header f ~title ~short_documentation:checker_config.short_documentation ;
          Option.iter deprecated_opt ~f:(pp_checker_deprecation_message f) ;
          Option.iter checker_config.cli_flags ~f:(fun _ -> pp_checker_cli_flags f checker_config) ;
          pp_checker_language_support f checker_config.support ;
          F.pp_print_string f markdown_body ;
          pp_checker_issue_types f checker ;
          () )


(** delete all files that look like they were generated by a previous invocation of
    [--write-website] to avoid keeping documentation for deleted checkers around *)
let delete_checkers_website ~website_root =
  Utils.directory_iter
    (fun path ->
      if String.is_prefix ~prefix:basename_checker_prefix (Filename.basename path) then (
        L.progress "deleting '%s'@\n" path ;
        Unix.unlink path ) )
    (website_root ^/ docs_dir)


let all_checkers_website ~website_root =
  delete_checkers_website ~website_root ;
  List.iter Checker.all ~f:(fun checker -> write_checker_webpage ~website_root checker)


let write_website ~website_root =
  write_checkers_json ~path:(website_root ^/ "checkers.json") ;
  all_checkers_website ~website_root ;
  all_issues_website ~website_root ;
  ()
