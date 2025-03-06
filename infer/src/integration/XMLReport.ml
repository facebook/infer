(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_current_date_and_time f =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec} = Unix.time () |> Unix.localtime in
  F.fprintf f "%d-%02d-%dT%d:%d:%d.000000" (1900 + tm_year) tm_mon tm_mday tm_hour tm_min tm_sec


let pp_xml_issue f (issue : Jsonbug_t.jsonbug) =
  let java_class_name, java_package, method_name =
    let java_result =
      let open IOption.Let_syntax in
      if String.is_suffix ~suffix:".java" issue.file then
        let* package_class_method, _formals_types = String.lsplit2 ~on:'(' issue.procedure in
        let* package_class, method_ = String.rsplit2 ~on:'.' package_class_method in
        let+ package, class_ = String.rsplit2 ~on:'.' package_class in
        (package, class_, method_)
      else None
    in
    match java_result with None -> ("", "", issue.procedure) | Some result -> result
  in
  let esc = Escape.escape_xml in
  F.fprintf f
    {|<file name="%s">
    <violation begincolumn="%d" beginline="%d" endcolumn="%d" endline="%d" class="%s" method="%s" package="%s" priority="1" rule="%s" ruleset="Infer Rules" externalinfourl="https://fbinfer.com/%s">%s</violation>
  </file>|}
    (esc issue.file) (max issue.column 0) issue.line (max issue.column 0) (issue.line + 1)
    (esc java_class_name) (esc method_name) (esc java_package) (esc issue.bug_type)
    (esc (Help.abs_url_of_issue_type issue.bug_type))
    (esc issue.qualifier)


let is_user_visible (issue : Jsonbug_t.jsonbug) =
  Option.is_none issue.censored_reason && not (String.equal issue.severity "INFO")


let pp_xml_issue_filter f issue = if is_user_visible issue then pp_xml_issue f issue

let write ~xml_path ~json_path =
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report json_path in
  Utils.with_file_out xml_path ~f:(fun out_channel ->
      let f = F.formatter_of_out_channel out_channel in
      F.fprintf f {|@[%cpmd version="5.4.1" date="%t">
  %a
</pmd>@.|} '<' pp_current_date_and_time
        (Pp.seq ~sep:"\n" pp_xml_issue_filter)
        report )
