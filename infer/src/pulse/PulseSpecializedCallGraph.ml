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
module F = Format
module Node = SpecializedProcname
module NodeMap = SpecializedProcname.Map
module NodeSet = SpecializedProcname.Set

type location = Specialized_call_graph_t.location = {file: string; line: int; col: int}

type name = string

type callee = Specialized_call_graph_t.callee =
  {callee_name: name; callee_context: int; call_location: location}

type caller = Specialized_call_graph_t.caller = {caller_name: name; caller_context: int}

type node = Specialized_call_graph_t.node = {caller: caller; callees: callee list}

type specialization = string

type call_graph = Specialized_call_graph_t.call_graph =
  {nodes: node list; contexts: specialization list}

module JsonBuilder = struct
  type t =
    {nodes: node list; hashtbl: (Specialization.Pulse.t, int) Stdlib.Hashtbl.t; mutable fresh: int}

  let get_specialization_index builder specialization =
    match Stdlib.Hashtbl.find_opt builder.hashtbl specialization with
    | Some index ->
        index
    | None ->
        let index = builder.fresh in
        Stdlib.Hashtbl.add builder.hashtbl specialization index ;
        builder.fresh <- index + 1 ;
        index


  let make () = {nodes= []; hashtbl= Stdlib.Hashtbl.create 17; fresh= 0}

  let location_of (loc : Location.t) =
    {file= SourceFile.to_string ~force_relative:true loc.file; line= loc.line; col= loc.col}


  let direct_callees_of builder (non_disj : NonDisjDomain.Summary.t) =
    match NonDisjDomain.Summary.get_transitive_info_if_not_top non_disj with
    | None ->
        []
    | Some transitive_info ->
        TransitiveInfo.DirectCallee.Set.fold
          (fun (dc : TransitiveInfo.DirectCallee.t) acc ->
            { callee_name= Procname.to_string dc.proc_name
            ; callee_context= get_specialization_index builder dc.specialization
            ; call_location= location_of dc.loc }
            :: acc )
          transitive_info.direct_callees []


  let node_of builder caller_name specialization (summary : PulseSummary.summary) =
    let caller = {caller_name; caller_context= get_specialization_index builder specialization} in
    {caller; callees= direct_callees_of builder summary.non_disj}


  let add_summary builder proc_name (pulse_summary : PulseSummary.t) =
    let caller_name = Procname.to_string proc_name in
    let main = node_of builder caller_name Specialization.Pulse.bottom pulse_summary.main in
    let specialized =
      Specialization.Pulse.Map.fold
        (fun spec s acc -> node_of builder caller_name spec s :: acc)
        pulse_summary.specialized builder.nodes
    in
    {builder with nodes= main :: specialized}


  let add builder proc_name pulse_summary =
    Option.value_map pulse_summary ~default:builder ~f:(add_summary builder proc_name)


  let string_of_specialization specialization =
    if Specialization.Pulse.is_bottom specialization then ""
    else F.asprintf " [%a]" Specialization.Pulse.pp specialization


  let contexts_of hashtbl =
    let len = Stdlib.Hashtbl.length hashtbl in
    let contexts = Array.create ~len Specialization.Pulse.bottom in
    Stdlib.Hashtbl.iter (fun specialization index -> contexts.(index) <- specialization) hashtbl ;
    List.of_array contexts |> List.map ~f:string_of_specialization


  let finalize {nodes; hashtbl} = {nodes; contexts= contexts_of hashtbl}
end

let to_json call_graph = Specialized_call_graph_j.string_of_call_graph call_graph

let get_missed_captures ~get_summary entry_nodes =
  let from_execution (exec_state : ExecutionDomain.summary) =
    match exec_state with
    | ContinueProgram summary
    | ExceptionRaised summary
    | Stopped
        ( ExitProgram summary
        | AbortProgram {astate= summary}
        | LatentAbortProgram {astate= summary}
        | LatentInvalidAccess {astate= summary}
        | LatentSpecializedTypeIssue {astate= summary} ) ->
        AbductiveDomain.Summary.get_transitive_info summary
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
