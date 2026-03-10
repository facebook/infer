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

let heap_path_of (hp : Specialization.HeapPath.t) =
  let rec aux = function
    | Specialization.HeapPath.Pvar pvar ->
        `Pvar (Mangled.to_string (Pvar.get_name pvar))
    | Specialization.HeapPath.FieldAccess (fieldname, base) ->
        `FieldAccess
          { Specialized_call_graph_t.field=
              { class_name= Typ.Name.name (Fieldname.get_class_name fieldname)
              ; field_name= Fieldname.get_field_name fieldname }
          ; base= aux base }
    | Specialization.HeapPath.Dereference base ->
        `Dereference (aux base)
  in
  aux hp


let specialization_of (spec : Specialization.Pulse.t) =
  let aliases =
    Option.map spec.aliases ~f:(fun alias_groups ->
        List.map alias_groups ~f:(List.map ~f:heap_path_of) )
  in
  let dynamic_types =
    Specialization.HeapPath.Map.fold
      (fun path typ acc ->
        {Specialized_call_graph_t.path= heap_path_of path; typ= Typ.Name.name typ} :: acc )
      spec.dynamic_types []
  in
  {Specialized_call_graph_t.aliases; dynamic_types}


let loc_of (loc : Location.t) =
  { Specialized_call_graph_t.file= SourceFile.to_string ~force_relative:true loc.file
  ; line= loc.line
  ; col= loc.col }


let direct_callees_of (non_disj : NonDisjDomain.Summary.t) =
  match NonDisjDomain.Summary.get_transitive_info_if_not_top non_disj with
  | None ->
      []
  | Some transitive_info ->
      TransitiveInfo.DirectCallee.Set.fold
        (fun (dc : TransitiveInfo.DirectCallee.t) acc ->
          { Specialized_call_graph_t.callee_name= Procname.to_string dc.proc_name
          ; callee_specialization= specialization_of dc.specialization
          ; call_location= loc_of dc.loc }
          :: acc )
        transitive_info.direct_callees []


let node_of caller_name spec (s : PulseSummary.summary) =
  { Specialized_call_graph_t.caller_name
  ; caller_specialization= specialization_of spec
  ; callees= direct_callees_of s.non_disj }


let nodes_of proc_name (pulse_summary : PulseSummary.t) =
  let caller_name = Procname.to_string proc_name in
  let main = node_of caller_name Specialization.Pulse.bottom pulse_summary.main in
  let specialized =
    Specialization.Pulse.Map.fold
      (fun spec s acc -> node_of caller_name spec s :: acc)
      pulse_summary.specialized []
  in
  main :: specialized


let to_json entries = Specialized_call_graph_j.string_of_call_graph entries

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
