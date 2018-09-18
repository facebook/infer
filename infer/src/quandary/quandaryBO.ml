(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let update_issues all_issues =
  let quandary_bug_names =
    IssueType.[untrusted_buffer_access; untrusted_heap_allocation; untrusted_variable_length_array]
  in
  let inferbo_bug_names =
    IssueType.
      [ buffer_overrun_l1
      ; buffer_overrun_l2
      ; buffer_overrun_l3
      ; buffer_overrun_l4
      ; buffer_overrun_l5
      ; buffer_overrun_s2
      ; buffer_overrun_u5
      ; inferbo_alloc_is_big
      ; inferbo_alloc_is_zero
      ; inferbo_alloc_is_negative
      ; inferbo_alloc_may_be_big
      ; inferbo_alloc_may_be_negative ]
  in
  let is_quandary_issue issue =
    List.mem quandary_bug_names issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let is_inferbo_issue issue =
    List.mem inferbo_bug_names issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let quandary_issues, inferBO_issues =
    List.fold all_issues ~init:([], []) ~f:(fun (q_issues, iBO_issues) issue ->
        if is_quandary_issue issue then (issue :: q_issues, iBO_issues)
        else if is_inferbo_issue issue then (q_issues, issue :: iBO_issues)
        else (q_issues, iBO_issues) )
  in
  let matching_issues quandary_issue inferbo_issue =
    SourceFile.equal quandary_issue.Issue.proc_location.file inferbo_issue.Issue.proc_location.file
    && Int.equal quandary_issue.Issue.proc_location.line inferbo_issue.Issue.proc_location.line
  in
  let paired_issues =
    (* Can be computed more efficiently (in n*log(n)) by using a Map mapping
    file name + line number to quandary_issues to match with inferbo_issues *)
    List.concat_map quandary_issues ~f:(fun quandary_issue ->
        List.filter_map inferBO_issues ~f:(fun inferbo_issue ->
            if matching_issues quandary_issue inferbo_issue then
              Some (quandary_issue, inferbo_issue)
            else None ) )
  in
  let merge_issues (issue1, issue2) =
    { Issue.proc_name= issue1.Issue.proc_name
    ; proc_location= {issue1.Issue.proc_location with col= -1}
    ; err_key=
        Errlog.merge_err_key issue1.Issue.err_key issue2.Issue.err_key
          ~merge_issues:(fun issue1 _ ->
            if IssueType.equal issue1 IssueType.untrusted_buffer_access then
              IssueType.tainted_buffer_access
            else IssueType.tainted_memory_allocation )
          ~merge_descriptions:(fun descs1 descs2 ->
            String.concat
              ( "QuandaryBO error. Quandary error(s):\n"
              :: (descs1 @ ("InferBO error(s):\n" :: descs2)) ) )
    ; err_data= Errlog.merge_err_data issue1.Issue.err_data issue2.Issue.err_data }
  in
  (* Can merge List.map, List.concat_map and List.filter_map into a single fold. *)
  let quandaryBO_issues = List.map ~f:merge_issues paired_issues in
  let quandary_issuetypes =
    IssueType.
      [ quandary_taint_error
      ; shell_injection
      ; shell_injection_risk
      ; sql_injection
      ; sql_injection_risk
      ; untrusted_buffer_access
      ; untrusted_file_risk
      ; untrusted_heap_allocation
      ; untrusted_url_risk
      ; untrusted_variable_length_array
      ; user_controlled_sql_risk ]
  in
  let inferBO_issuetypes = inferbo_bug_names in
  let all_issues_filtered =
    List.filter
      ~f:(fun issue ->
        ( Config.quandary
        || not (List.mem quandary_issuetypes issue.Issue.err_key.err_name ~equal:IssueType.equal)
        )
        && ( Config.bufferoverrun
           || not (List.mem inferBO_issuetypes issue.Issue.err_key.err_name ~equal:IssueType.equal)
           ) )
      all_issues
  in
  List.rev_append all_issues_filtered quandaryBO_issues
