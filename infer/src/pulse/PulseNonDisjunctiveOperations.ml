(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseDomainInterface
open PulseBasicInterface

let is_constructor = function Procname.ObjC_Cpp {kind= CPPConstructor _} -> true | _ -> false

let add_copies location call_exp actuals (flags : CallFlags.t) astates astate_non_disj =
  List.fold astates ~init:astate_non_disj
    ~f:(fun astate_non_disj (exec_state : ExecutionDomain.t) ->
      match (exec_state, (call_exp : Exp.t), actuals) with
      | ( ContinueProgram disjunct
        , (Const (Cfun procname) | Closure {name= procname})
        , (Exp.Lvar pvar, _) :: _ )
        when is_constructor procname && flags.cf_is_copy_ctor ->
          let copied_var = Var.of_pvar pvar in
          if Var.appears_in_source_code copied_var then
            let heap = (disjunct.post :> BaseDomain.t).heap in
            NonDisjDomain.add copied_var (Copied {heap; location}) astate_non_disj
          else astate_non_disj
      | ISLLatentMemoryError _, _, _
      | AbortProgram _, _, _
      | ContinueProgram _, _, _
      | ExitProgram _, _, _
      | LatentAbortProgram _, _, _
      | LatentInvalidAccess _, _, _ ->
          astate_non_disj )


let get_matching_dest_addr_opt (edges_curr, attr_curr) edges_orig : AbstractValue.t list option =
  BaseMemory.Edges.fold edges_curr ~init:(Some []) ~f:(fun acc (access_curr, (addr_curr, _)) ->
      match BaseMemory.Edges.find_opt access_curr edges_orig with
      | Some (addr_orig, _) ->
          if AbstractValue.equal addr_curr addr_orig then
            Option.map acc ~f:(fun acc -> addr_curr :: acc)
          else
            (* mismatch for the addresses on the copy and the
               current heap. *)
            None
      | _ ->
          if Option.is_none (BaseAddressAttributes.get_written_to addr_curr attr_curr) then
            (* address only occurs on the current heap, most likely it has been read since the copy.
               Continue exploring the rest of the edges...*)
            acc
          else (* address is written since copied! *)
            None )


let is_modified_since_copy addr ~current_heap ~current_attrs ~copy_heap
    ~reachable_addresses_from_copy =
  let rec aux ~addr_to_explore ~visited =
    match addr_to_explore with
    | [] ->
        false
    | addr :: addr_to_explore -> (
        if
          AbstractValue.Set.mem addr visited
          || not (AbstractValue.Set.mem addr reachable_addresses_from_copy)
        then aux ~addr_to_explore ~visited
        else
          let copy_edges_opt = BaseMemory.find_opt addr copy_heap in
          let current_edges_opt = BaseMemory.find_opt addr current_heap in
          let visited = AbstractValue.Set.add addr visited in
          match (current_edges_opt, copy_edges_opt) with
          | None, None ->
              aux ~addr_to_explore ~visited
          | Some _, None ->
              let is_written =
                BaseAddressAttributes.get_written_to addr current_attrs |> Option.is_some
              in
              is_written || aux ~addr_to_explore ~visited
          | None, Some _ ->
              aux ~addr_to_explore ~visited
          | Some edges_curr, Some edges_orig ->
              get_matching_dest_addr_opt (edges_curr, current_attrs) edges_orig
              |> Option.value_map ~default:true ~f:(fun matching_addr_list ->
                     aux ~addr_to_explore:(matching_addr_list @ addr_to_explore) ~visited ) )
  in
  aux ~addr_to_explore:[addr] ~visited:AbstractValue.Set.empty


let mark_modified_copy_at ~address var astate (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_copy_as_modified var astate_n ~is_modified:(fun copy_heap ->
      let reachable_addresses_from_copy =
        BaseDomain.reachable_addresses_from
          (Caml.List.to_seq [address])
          (astate.AbductiveDomain.post :> BaseDomain.t)
      in
      let current_heap = (astate.AbductiveDomain.post :> BaseDomain.t).heap in
      let current_attrs = (astate.AbductiveDomain.post :> BaseDomain.t).attrs in
      L.d_printfln_escaped "Current heap  %a" BaseMemory.pp current_heap ;
      L.d_printfln_escaped "Copy heap %a" BaseMemory.pp copy_heap ;
      is_modified_since_copy address ~current_heap ~copy_heap ~current_attrs
        ~reachable_addresses_from_copy )


let mark_modified_copies vars astate astate_n =
  List.fold vars ~init:astate_n ~f:(fun astate_n var ->
      match Stack.find_opt var astate with
      | Some (address, _history) ->
          mark_modified_copy_at ~address var astate astate_n
      | None ->
          astate_n )


let mark_modified_copies vars disjuncts astate_n =
  List.fold disjuncts ~init:astate_n ~f:(fun astate_n (exec_state : ExecutionDomain.t) ->
      match exec_state with
      | ISLLatentMemoryError _
      | AbortProgram _
      | ExitProgram _
      | LatentAbortProgram _
      | LatentInvalidAccess _ ->
          astate_n
      | ContinueProgram astate ->
          mark_modified_copies vars astate astate_n )
