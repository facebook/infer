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

let is_modeled_as_returning_copy proc_name =
  Option.exists Config.pulse_model_returns_copy_pattern ~f:(fun r ->
      let s = Procname.to_string proc_name in
      Str.string_match r s 0 )


let add_copies path location call_exp actuals astates astate_non_disj =
  let aux (copy_check_fn, args_map_fn) init astates =
    List.fold_map astates ~init ~f:(fun astate_non_disj (exec_state : ExecutionDomain.t) ->
        match (exec_state, (call_exp : Exp.t), args_map_fn actuals) with
        | ( ContinueProgram disjunct
          , (Const (Cfun procname) | Closure {name= procname})
          , (Exp.Lvar copy_pvar, _) :: rest_args )
          when copy_check_fn procname ->
            let copied_var = Var.of_pvar copy_pvar in
            if Var.appears_in_source_code copied_var then
              let heap = (disjunct.post :> BaseDomain.t).heap in
              let copied = NonDisjDomain.Copied {heap; location} in
              let disjunct, source_addr_opt =
                match rest_args with
                | (source_arg, _) :: _ -> (
                  match PulseOperations.eval path NoAccess location source_arg disjunct with
                  | Ok (disjunct, (source_addr, _)) ->
                      (disjunct, Some source_addr)
                  | Recoverable _ | FatalError _ ->
                      (disjunct, None) )
                | _ ->
                    (disjunct, None)
              in
              let copy_addr, _ = Option.value_exn (Stack.find_opt copied_var disjunct) in
              let disjunct' =
                Option.value_map source_addr_opt ~default:disjunct ~f:(fun source_addr ->
                    AddressAttributes.add_one source_addr (CopiedVar copied_var) disjunct
                    |> AddressAttributes.add_one copy_addr (SourceOriginOfCopy source_addr) )
              in
              ( NonDisjDomain.add copied_var ~source_addr_opt copied astate_non_disj
              , ExecutionDomain.continue disjunct' )
            else (astate_non_disj, exec_state)
        | ExceptionRaised _, _, _
        | ISLLatentMemoryError _, _, _
        | AbortProgram _, _, _
        | ContinueProgram _, _, _
        | ExitProgram _, _, _
        | LatentAbortProgram _, _, _
        | LatentInvalidAccess _, _, _ ->
            (astate_non_disj, exec_state) )
  in
  let astate_n, astates = aux (Procname.is_copy_ctor, Fn.id) astate_non_disj astates in
  (* For functions that return a copy, the last argument is the assigned copy *)
  aux (is_modeled_as_returning_copy, List.rev) astate_n astates


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
          | Some edges_curr, None ->
              BaseAddressAttributes.get_written_to addr current_attrs |> Option.is_some
              ||
              let addr_to_explore =
                BaseMemory.Edges.fold edges_curr ~init:addr_to_explore ~f:(fun acc (_, (addr, _)) ->
                    addr :: acc )
              in
              aux ~addr_to_explore ~visited
          | None, Some _ ->
              aux ~addr_to_explore ~visited
          | Some edges_curr, Some edges_orig ->
              get_matching_dest_addr_opt (edges_curr, current_attrs) edges_orig
              |> Option.value_map ~default:true ~f:(fun matching_addr_list ->
                     aux ~addr_to_explore:(matching_addr_list @ addr_to_explore) ~visited ) )
  in
  aux ~addr_to_explore:[addr] ~visited:AbstractValue.Set.empty


let mark_modified_address_at ~address ~source_addr_opt ?(is_source = false) ~copied_var astate
    (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_copy_as_modified ~copied_var ~source_addr_opt astate_n
    ~is_modified:(fun copy_heap ->
      let reachable_addresses_from_copy =
        BaseDomain.reachable_addresses_from
          (Caml.List.to_seq [address])
          (astate.AbductiveDomain.post :> BaseDomain.t)
      in
      let current_heap = (astate.AbductiveDomain.post :> BaseDomain.t).heap in
      let current_attrs = (astate.AbductiveDomain.post :> BaseDomain.t).attrs in
      let reachable_from heap =
        BaseMemory.filter
          (fun address _ -> AbstractValue.Set.mem address reachable_addresses_from_copy)
          heap
      in
      if Config.debug_mode then (
        L.d_printfln_escaped "Current reachable heap %a" BaseMemory.pp (reachable_from current_heap) ;
        L.d_printfln_escaped "%s reachable heap %a"
          (if is_source then "Source" else "Copy")
          BaseMemory.pp (reachable_from copy_heap) ) ;
      is_modified_since_copy address ~current_heap ~copy_heap ~current_attrs
        ~reachable_addresses_from_copy )


let mark_modified_copies_with vars ~astate astate_n =
  let mark_modified_copy var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           let source_addr_opt = AddressAttributes.get_source_origin_of_copy address astate in
           mark_modified_address_at ~address ~source_addr_opt ~copied_var:var astate default )
  in
  List.fold vars ~init:astate_n ~f:(fun astate_n var ->
      let res_opt =
        let open IOption.Let_syntax in
        let* source_addr, _ = Stack.find_opt var astate in
        let+ copied_var = AddressAttributes.get_copied_var source_addr astate in
        mark_modified_address_at ~address:source_addr ~source_addr_opt:(Some source_addr)
          ~is_source:true ~copied_var astate astate_n
      in
      match res_opt with Some res -> res | None -> mark_modified_copy var astate_n )


let mark_modified_copies vars disjuncts astate_n =
  let unchecked_vars =
    List.filter vars ~f:(fun var ->
        not (PulseNonDisjunctiveDomain.is_checked_via_dtor var astate_n) )
  in
  List.fold disjuncts ~init:astate_n ~f:(fun astate_n (exec_state : ExecutionDomain.t) ->
      match exec_state with
      | ISLLatentMemoryError _
      | AbortProgram _
      | ExceptionRaised _
      | ExitProgram _
      | LatentAbortProgram _
      | LatentInvalidAccess _ ->
          astate_n
      | ContinueProgram astate ->
          mark_modified_copies_with unchecked_vars ~astate astate_n )
