(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
module Node = SpecializedProcname
module NodeMap = SpecializedProcname.Map
module NodeSet = SpecializedProcname.Set

let get_missed_captures ~get_summary entry_nodes =
  let from_execution = function
    | ExecutionDomain.ContinueProgram summary
    | ExceptionRaised summary
    | ExitProgram summary
    | AbortProgram summary ->
        AbductiveDomain.Summary.get_transitive_info summary
    | LatentAbortProgram {astate}
    | LatentInvalidAccess {astate}
    | LatentSpecializedTypeIssue {astate} ->
        AbductiveDomain.Summary.get_transitive_info astate
  in
  let from_pre_post_list pre_post_list =
    List.map pre_post_list ~f:(fun exec -> from_execution exec)
    |> List.reduce ~f:TransitiveInfo.join
    |> Option.value ~default:TransitiveInfo.bottom
  in
  let from_simple_summary {PulseSummary.pre_post_list; non_disj} =
    let from_disjs = from_pre_post_list pre_post_list in
    let from_non_disj =
      NonDisjDomain.Summary.get_transitive_info_if_not_top non_disj
      |> Option.value ~default:TransitiveInfo.bottom
    in
    TransitiveInfo.join from_disjs from_non_disj
  in
  let from_specialized_summary specialization summary =
    if Specialization.Pulse.is_bottom specialization then
      from_simple_summary summary.PulseSummary.main
    else
      Specialization.Pulse.Map.find_opt specialization summary.PulseSummary.specialized
      |> Option.value_map ~default:TransitiveInfo.bottom ~f:from_simple_summary
  in
  let rec visit (seen, missed_captures_map) (node : Node.t) =
    if NodeSet.mem node seen then (seen, missed_captures_map)
    else
      let seen = NodeSet.add node seen in
      let specialization =
        Option.value_map node.specialization ~default:Specialization.Pulse.bottom ~f:(function
            | Specialization.Pulse t -> t )
      in
      let direct_missed_captures, seen, missed_captures_map =
        match get_summary node.proc_name with
        | None ->
            (Typ.Name.Set.empty, seen, missed_captures_map)
        | Some summary ->
            let { TransitiveInfo.direct_callees
                ; has_transitive_missed_captures
                ; direct_missed_captures } =
              from_specialized_summary specialization summary
            in
            if has_transitive_missed_captures then
              let seen, missed_captures_map =
                TransitiveInfo.DirectCallee.Set.fold
                  (fun {proc_name; specialization} acc ->
                    let node = {Node.proc_name; specialization= Some (Pulse specialization)} in
                    visit acc node )
                  direct_callees (seen, missed_captures_map)
              in
              (direct_missed_captures, seen, missed_captures_map)
            else (direct_missed_captures, seen, missed_captures_map)
      in
      (seen, NodeMap.add node direct_missed_captures missed_captures_map)
  in
  let _, missed_captures_map =
    List.fold entry_nodes ~init:(NodeSet.empty, NodeMap.empty) ~f:visit
  in
  let normalize_node (node : Node.t) =
    match node.specialization with
    | Some (Pulse pulse_specialization) when Specialization.Pulse.is_bottom pulse_specialization ->
        {node with specialization= None}
    | _ ->
        node
  in
  NodeMap.fold
    (fun node missed_types acc ->
      let node = normalize_node node in
      Typ.Name.Set.fold
        (fun missed_type acc ->
          Typ.Name.Map.update missed_type
            (function
              | None ->
                  Some (NodeSet.singleton node)
              | Some node_set ->
                  Some (NodeSet.add node node_set) )
            acc )
        missed_types acc )
    missed_captures_map Typ.Name.Map.empty
