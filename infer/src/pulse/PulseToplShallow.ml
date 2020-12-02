(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let add_errors {InterproceduralAnalysis.proc_desc; err_log} (pulse_summary : PulseSummary.t) =
  let pulse_summary =
    let f = function PulseExecutionDomain.ContinueProgram s -> Some s | _ -> None in
    List.filter_map ~f (pulse_summary :> PulseExecutionDomain.t list)
  in
  let proc_name = Procdesc.get_proc_name proc_desc in
  if not (ToplUtils.is_synthesized proc_name) then
    let start_to_error =
      Topl.automaton () |> ToplAutomaton.get_start_error_pairs |> Int.Table.of_alist_exn
    in
    let topl_property_var = Var.of_pvar ToplUtils.topl_class_pvar in
    let topl_state_field = HilExp.Access.FieldAccess (ToplUtils.make_field ToplName.state) in
    let check_pre_post pre_post =
      let open IOption.Let_syntax in
      let path_condition = pre_post.PulseAbductiveDomain.path_condition in
      let get_topl_state_opt pulse_state =
        let stack = pulse_state.PulseBaseDomain.stack in
        let heap = pulse_state.PulseBaseDomain.heap in
        let* topl_property_addr, _ = PulseBaseStack.find_opt topl_property_var stack in
        let* state_addr, _ =
          PulseBaseMemory.find_edge_opt topl_property_addr topl_state_field heap
        in
        let* state_val, _ =
          PulseBaseMemory.find_edge_opt state_addr HilExp.Access.Dereference heap
        in
        PulsePathCondition.as_int path_condition state_val
      in
      let* pre_topl = get_topl_state_opt (pre_post.PulseAbductiveDomain.pre :> PulseBaseDomain.t) in
      let* post_topl =
        get_topl_state_opt (pre_post.PulseAbductiveDomain.post :> PulseBaseDomain.t)
      in
      let* error_topl = Int.Table.find start_to_error pre_topl in
      if Int.equal post_topl error_topl then Some error_topl else None
    in
    let errors = List.filter_map ~f:check_pre_post pulse_summary in
    let errors = Int.Set.to_list (Int.Set.of_list errors) in
    let loc = Procdesc.get_loc proc_desc in
    let report error_state =
      let m =
        Format.asprintf "%a" ToplAutomaton.pp_message_of_state (Topl.automaton (), error_state)
      in
      Reporting.log_issue proc_desc err_log ToplOnPulse IssueType.topl_pulse_error ~loc m
    in
    List.iter ~f:report errors


let analyze pulse analysis_data = Topl.analyze_with add_errors pulse analysis_data
