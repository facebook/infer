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

type callee = Specialized_call_graph_t.callee = {call_location: location; callee: int}

type name = string

type node = Specialized_call_graph_t.node = {name: string; context: int}

type specialization = string

type edge = Specialized_call_graph_t.edge = {caller: node; callees: callee list}

type call_graph = Specialized_call_graph_t.call_graph =
  {edges: edge list; contexts: specialization list}

module JsonBuilder = struct
  type t =
    { caller_callees: (int * callee list) list
    ; node_tbl: (Procname.t * int, int) Stdlib.Hashtbl.t
    ; mutable next_node: int
    ; context_tbl: (Specialization.Pulse.t, int) Stdlib.Hashtbl.t
    ; mutable next_context: int }

  let get_context_index builder specialization =
    match Stdlib.Hashtbl.find_opt builder.context_tbl specialization with
    | Some index ->
        index
    | None ->
        let index = builder.next_context in
        Stdlib.Hashtbl.add builder.context_tbl specialization index ;
        builder.next_context <- index + 1 ;
        index


  let get_node_index builder name context =
    let key = (name, context) in
    match Stdlib.Hashtbl.find_opt builder.node_tbl key with
    | Some index ->
        index
    | None ->
        let index = builder.next_node in
        Stdlib.Hashtbl.add builder.node_tbl key index ;
        builder.next_node <- index + 1 ;
        index


  let make () =
    { caller_callees= []
    ; node_tbl= Stdlib.Hashtbl.create 17
    ; next_node= 0
    ; context_tbl= Stdlib.Hashtbl.create 17
    ; next_context= 0 }


  let location_of (loc : Location.t) =
    {file= SourceFile.to_string ~force_relative:true loc.file; line= loc.line; col= loc.col}


  let direct_callees_of builder (non_disj : NonDisjDomain.Summary.t) =
    match NonDisjDomain.Summary.get_transitive_info_if_not_top non_disj with
    | None ->
        []
    | Some transitive_info ->
        TransitiveInfo.DirectCallee.Set.fold
          (fun (dc : TransitiveInfo.DirectCallee.t) acc ->
            let callee_context = get_context_index builder dc.specialization in
            let callee = get_node_index builder dc.proc_name callee_context in
            {call_location= location_of dc.loc; callee} :: acc )
          transitive_info.direct_callees []


  let callees_of_caller builder caller_name specialization (summary : PulseSummary.summary) =
    let context = get_context_index builder specialization in
    let caller_index = get_node_index builder caller_name context in
    let callees = direct_callees_of builder summary.non_disj in
    (caller_index, callees)


  let add_summary builder proc_name (pulse_summary : PulseSummary.t) =
    let main = callees_of_caller builder proc_name Specialization.Pulse.bottom pulse_summary.main in
    let caller_callees =
      Specialization.Pulse.Map.fold
        (fun spec s acc -> callees_of_caller builder proc_name spec s :: acc)
        pulse_summary.specialized builder.caller_callees
    in
    {builder with caller_callees= main :: caller_callees}


  let add builder proc_name pulse_summary =
    Option.value_map pulse_summary ~default:builder ~f:(add_summary builder proc_name)


  let string_of_specialization specialization =
    if Specialization.Pulse.is_bottom specialization then ""
    else F.asprintf " [%a]" Specialization.Pulse.pp specialization


  let finalize builder =
    let nodes_arr = Array.create ~len:builder.next_node ("", 0) in
    Stdlib.Hashtbl.iter
      (fun (proc_name, context) index ->
        nodes_arr.(index) <- (Procname.to_string proc_name, context) )
      builder.node_tbl ;
    let callees_per_node = Array.create ~len:builder.next_node [] in
    List.iter builder.caller_callees ~f:(fun (caller_index, callees) ->
        callees_per_node.(caller_index) <- callees @ callees_per_node.(caller_index) ) ;
    let edges =
      Array.to_list
        (Array.mapi nodes_arr ~f:(fun i (name, context) ->
             {caller= {name; context}; callees= callees_per_node.(i)} ) )
    in
    let contexts_arr = Array.create ~len:builder.next_context Specialization.Pulse.bottom in
    Stdlib.Hashtbl.iter
      (fun specialization index -> contexts_arr.(index) <- specialization)
      builder.context_tbl ;
    let contexts = List.of_array contexts_arr |> List.map ~f:string_of_specialization in
    {edges; contexts}
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
