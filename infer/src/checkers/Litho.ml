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
module Domain = LithoDomain

module Summary = Summary.Make (struct
  type payload = Domain.astate

  let update_payload astate (summary: Specs.summary) =
    {summary with payload= {summary.payload with litho= Some astate}}


  let read_payload (summary: Specs.summary) = summary.payload.litho
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras

  let is_graphql_getter procname summary =
    Option.is_none summary
    (* we skip analysis of all GraphQL procs *)
    &&
    match procname with
    | Typ.Procname.Java java_procname
      -> (
        PatternMatch.is_getter java_procname
        &&
        match Typ.Procname.java_get_package java_procname with
        | Some package ->
            String.is_prefix ~prefix:"com.facebook.graphql.model" package
        | None ->
            false )
    | _ ->
        false


  let apply_callee_summary summary_opt caller_pname ret_opt actuals astate =
    match summary_opt with
    | Some summary ->
        (* TODO: append paths if the footprint access path is an actual path instead of a var *)
        let f_sub {Domain.LocalAccessPath.access_path= (var, _), _} =
          match Var.get_footprint_index var with
          | Some footprint_index -> (
            match List.nth actuals footprint_index with
            | Some HilExp.AccessPath actual_access_path ->
                Some (Domain.LocalAccessPath.make actual_access_path caller_pname)
            | _ ->
                None )
          | None ->
              if Var.is_return var then
                match ret_opt with
                | Some ret ->
                    Some (Domain.LocalAccessPath.make (ret, []) caller_pname)
                | None ->
                    assert false
              else None
        in
        Domain.substitute ~f_sub summary
    | None ->
        astate


  let exec_instr astate (proc_data: extras ProcData.t) _ (instr: HilInstr.t) : Domain.astate =
    let caller_pname = Procdesc.get_proc_name proc_data.pdesc in
    match instr with
    | Call
        ( (Some return_base as ret_opt)
        , Direct callee_procname
        , ((HilExp.AccessPath receiver_ap) :: _ as actuals)
        , _
        , _ ) ->
        let summary = Summary.read_summary proc_data.pdesc callee_procname in
        (* track the call if the callee is a graphql getter *or* the receiver is already tracked *)
        (* TODO: we should probably track all formals as well *)
        let receiver = Domain.LocalAccessPath.make receiver_ap caller_pname in
        if is_graphql_getter callee_procname summary || Domain.mem receiver astate then
          let receiver = Domain.LocalAccessPath.make receiver_ap caller_pname in
          let return_access_path = Domain.LocalAccessPath.make (return_base, []) caller_pname in
          let return_calls =
            (try Domain.find return_access_path astate with Not_found -> Domain.CallSet.empty)
            |> Domain.CallSet.add {receiver; procname= callee_procname}
          in
          Domain.add return_access_path return_calls astate
        else
          (* treat it like a normal call *)
          apply_callee_summary summary caller_pname ret_opt actuals astate
    | Call (ret_opt, Direct callee_procname, actuals, _, _) ->
        let summary = Summary.read_summary proc_data.pdesc callee_procname in
        apply_callee_summary summary caller_pname ret_opt actuals astate
    | Assign (lhs_ap, HilExp.AccessPath rhs_ap, _)
      -> (
        (* creating an alias for the rhs binding; assume all reads will now occur through the
           alias. this helps us keep track of chains in cases like tmp = getFoo(); x = tmp;
           tmp.getBar() *)
        let lhs_access_path = Domain.LocalAccessPath.make lhs_ap caller_pname in
        let rhs_access_path = Domain.LocalAccessPath.make rhs_ap caller_pname in
        try
          let call_set = Domain.find rhs_access_path astate in
          Domain.remove rhs_access_path astate |> Domain.add lhs_access_path call_set
        with Not_found -> astate )
    | _ ->
        astate
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let report access_path call_chain summary =
  let call_strings = List.map ~f:(Typ.Procname.to_simplified_string ~withclass:false) call_chain in
  let call_string = String.concat ~sep:"." call_strings in
  let message = F.asprintf "%a.%s" AccessPath.pp access_path call_string in
  let exn = Exceptions.Checkers (IssueType.graphql_field_access, Localise.verbatim_desc message) in
  let loc = Specs.get_loc summary in
  let ltr = [Errlog.make_trace_element 0 loc message []] in
  Reporting.log_error summary ~loc ~ltr exn


let unroll_call call astate summary =
  let max_depth = Domain.cardinal astate in
  let rec unroll_call_ ({receiver; procname}: Domain.MethodCall.t) (acc, depth) =
    let acc' = procname :: acc in
    let depth' = depth + 1 in
    let is_cycle access_path =
      (* detect direct cycles and cycles due to mutual recursion *)
      Domain.LocalAccessPath.equal access_path receiver || depth' > max_depth
    in
    try
      let calls' = Domain.find receiver astate in
      Domain.CallSet.iter
        (fun call ->
          if not (is_cycle call.receiver) then unroll_call_ call (acc', depth')
          else report receiver.access_path acc' summary )
        calls'
    with Not_found -> report receiver.access_path acc' summary
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


let postprocess astate proc_desc : Domain.astate =
  let formal_map = FormalMap.make proc_desc in
  let f_sub access_path = Domain.LocalAccessPath.to_formal_option access_path formal_map in
  Domain.substitute ~f_sub astate


let checker {Callbacks.summary; proc_desc; tenv} =
  let proc_data = ProcData.make_default proc_desc tenv in
  match Analyzer.compute_post proc_data ~initial:Domain.empty with
  | Some post ->
      if should_report proc_desc then report_call_chains post summary ;
      let payload = postprocess post proc_desc in
      Summary.update_summary payload summary
  | None ->
      summary
