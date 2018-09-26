(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let update_issues all_issues =
  let quandary_access_issues = [IssueType.untrusted_buffer_access] in
  let quandary_alloc_issues =
    IssueType.[untrusted_heap_allocation; untrusted_variable_length_array]
  in
  let inferbo_access_issues =
    IssueType.
      [ buffer_overrun_l1
      ; buffer_overrun_l2
      ; buffer_overrun_l3
      ; buffer_overrun_l4
      ; buffer_overrun_l5
      ; buffer_overrun_s2
      ; buffer_overrun_u5 ]
  in
  let inferbo_alloc_issues =
    IssueType.
      [ inferbo_alloc_is_big
      ; inferbo_alloc_is_zero
      ; inferbo_alloc_is_negative
      ; inferbo_alloc_may_be_big
      ; inferbo_alloc_may_be_negative ]
  in
  let is_quandary_access_issue issue =
    List.mem quandary_access_issues issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let is_quandary_alloc_issue issue =
    List.mem quandary_alloc_issues issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let is_relevant_quandary_issue issue =
    is_quandary_access_issue issue || is_quandary_alloc_issue issue
  in
  let is_inferbo_access_issue issue =
    List.mem inferbo_access_issues issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let is_inferbo_alloc_issue issue =
    List.mem inferbo_alloc_issues issue.Issue.err_key.err_name ~equal:IssueType.equal
  in
  let is_relevant_inferbo_issue issue =
    is_inferbo_access_issue issue || is_inferbo_alloc_issue issue
  in
  let quandary_issues, inferBO_issues =
    List.fold all_issues ~init:([], []) ~f:(fun (q_issues, iBO_issues) issue ->
        if is_relevant_quandary_issue issue then (issue :: q_issues, iBO_issues)
        else if is_relevant_inferbo_issue issue then (q_issues, issue :: iBO_issues)
        else (q_issues, iBO_issues) )
  in
  let matching_issues quandary_issue inferbo_issue =
    let trace_end_match () =
      let quandary_trace_end = List.last quandary_issue.Issue.err_data.loc_trace in
      let inferbo_trace_end = List.last inferbo_issue.Issue.err_data.loc_trace in
      match (quandary_trace_end, inferbo_trace_end) with
      | Some quandary_trace_elem, Some inferbo_trace_elem ->
          let q_loc = quandary_trace_elem.lt_loc in
          let i_loc = inferbo_trace_elem.lt_loc in
          SourceFile.equal q_loc.file i_loc.file && Int.equal q_loc.line i_loc.line
      | _ ->
          false
    in
    SourceFile.equal quandary_issue.Issue.proc_location.file inferbo_issue.Issue.proc_location.file
    && Int.equal quandary_issue.Issue.proc_location.line inferbo_issue.Issue.proc_location.line
    && trace_end_match ()
    && ( (is_quandary_alloc_issue quandary_issue && is_inferbo_alloc_issue inferbo_issue)
       || (is_quandary_access_issue quandary_issue && is_inferbo_access_issue inferbo_issue) )
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
              ( "QuandaryBO error. Quandary error(s): \""
              :: (descs1 @ ("\". InferBO error(s):\"" :: (descs2 @ ["\"."]))) ) )
    ; err_data= Errlog.merge_err_data issue1.Issue.err_data issue2.Issue.err_data }
  in
  (* Can merge List.map, List.concat_map and List.filter_map into a single fold. *)
  let quandaryBO_issues = List.map ~f:merge_issues paired_issues in
  let quandary_issues =
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
  let inferbo_issues =
    inferbo_alloc_issues @ inferbo_access_issues @ [IssueType.unreachable_code_after]
  in
  let filtered_issues = Config.quandaryBO_filtered_issues in
  let all_issues_filtered =
    List.filter
      ~f:(fun issue ->
        let issue_in ls = List.mem ls issue.Issue.err_key.err_name ~equal:IssueType.equal in
        (Config.quandary || not (issue_in quandary_issues))
        && (Config.bufferoverrun || not (issue_in inferbo_issues))
        && not (issue_in filtered_issues) )
      all_issues
  in
  List.rev_append all_issues_filtered quandaryBO_issues
