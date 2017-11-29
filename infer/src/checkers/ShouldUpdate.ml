(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging
module Domain = ShouldUpdateDomain

module Summary = Summary.Make (struct
  type payload = Domain.astate

  let update_payload astate (summary: Specs.summary) =
    {summary with payload= {summary.payload with should_update= Some astate}}


  let read_payload (summary: Specs.summary) = summary.payload.should_update
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras

  let is_getter = function
    | Typ.Procname.Java procname ->
        PatternMatch.is_getter procname
    | _ ->
        false


  let exec_instr astate _ _ (instr: HilInstr.t) : Domain.astate =
    match instr with
    | Call (Some return_base, Direct procname, (HilExp.AccessPath receiver) :: _, _, _)
      when is_getter procname ->
        let return_access_path = (return_base, []) in
        let return_calls =
          (try Domain.find return_access_path astate with Not_found -> Domain.CallSet.empty)
          |> Domain.CallSet.add {receiver; procname}
        in
        Domain.add return_access_path return_calls astate
    | Call _ ->
        (* TODO: interprocedural analysis
           (1) add caller actuals to their callee call set (according to the summary)
           (2) bind the caller return value to its callee call set (according to the summary
        *)
        astate
    | Assign (lhs_access_path, HilExp.AccessPath rhs_access_path, _) -> (
      try
        (* creating an alias for the rhs binding; assume all reads will now occur through the
           alias. this helps us keep track of chains in cases like tmp = getFoo(); x = tmp;
           tmp.getBar() *)
        let call_set = Domain.find rhs_access_path astate in
        Domain.remove rhs_access_path astate |> Domain.add lhs_access_path call_set
      with Not_found -> astate )
    | _ ->
        astate

end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let report receiver call_chain summary =
  let call_strings = List.map ~f:(Typ.Procname.to_simplified_string ~withclass:false) call_chain in
  let call_string = String.concat ~sep:"." call_strings in
  let message = F.asprintf "GraphQL getter chain %a.%s" AccessPath.pp receiver call_string in
  let issue_id = IssueType.graphql_field_access.unique_id in
  let exn = Exceptions.Checkers (issue_id, Localise.verbatim_desc message) in
  let loc = Specs.get_loc summary in
  Reporting.log_error summary ~loc exn


let unroll_call call astate summary =
  let max_depth = Domain.cardinal astate in
  let rec unroll_call_ ({receiver; procname}: Domain.CallWithReceiver.t) (acc, depth) =
    let acc' = procname :: acc in
    let depth' = depth + 1 in
    let is_cycle access_path =
      (* detect direct cycles and cycles due to mutual recursion *)
      AccessPath.equal access_path receiver || depth' > max_depth
    in
    try
      let calls' = Domain.find receiver astate in
      Domain.CallSet.iter
        (fun call -> if not (is_cycle call.receiver) then unroll_call_ call (acc', depth'))
        calls'
    with Not_found -> report receiver acc' summary
  in
  unroll_call_ call ([], 0)


let should_report proc_desc =
  match Procdesc.get_proc_name proc_desc with
  | Typ.Procname.Java java_pname -> (
    match Typ.Procname.java_get_method java_pname with "onCreateLayout" -> true | _ -> false )
  | _ ->
      false


let report_call_chains post summary =
  Domain.iter
    (fun _ call_set -> Domain.CallSet.iter (fun call -> unroll_call call post summary) call_set)
    post


let checker {Callbacks.summary; proc_desc; tenv} =
  let proc_data = ProcData.make_default proc_desc tenv in
  match Analyzer.compute_post proc_data ~initial:Domain.empty with
  | Some post ->
      if should_report proc_desc then report_call_chains post summary ;
      summary
  | None ->
      summary

