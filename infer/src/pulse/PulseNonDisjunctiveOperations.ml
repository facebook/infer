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

let get_modeled_as_returning_copy_opt proc_name =
  Option.value_map ~default:None Config.pulse_model_returns_copy_pattern ~f:(fun r ->
      let s = Procname.to_string proc_name in
      if Str.string_match r s 0 then Some Attribute.CopyOrigin.CopyCtor else None )


let get_copied_and_source copy_type path rest_args location from (disjunct : AbductiveDomain.t) =
  let heap = (disjunct.post :> BaseDomain.t).heap in
  let copied = NonDisjDomain.Copied {heap; typ= Typ.strip_ptr copy_type; location; from} in
  let disjunct, source_addr_typ_opt =
    match rest_args with
    | (source_arg, source_typ) :: _ -> (
      match PulseOperations.eval path NoAccess location source_arg disjunct with
      | Ok (disjunct, (source_addr, _)) ->
          (disjunct, Some (source_addr, source_typ))
      | Recoverable _ | FatalError _ ->
          (disjunct, None) )
    | _ ->
        (disjunct, None)
  in
  (copied, disjunct, source_addr_typ_opt)


let add_copies path location call_exp actuals astates astate_non_disj =
  let is_ptr_to_trivially_copyable typ =
    Typ.is_pointer typ && Typ.is_trivially_copyable (Typ.strip_ptr typ).quals
  in
  let aux (copy_check_fn, args_map_fn) init astates =
    List.fold_map astates ~init ~f:(fun astate_non_disj (exec_state : ExecutionDomain.t) ->
        match (exec_state, (call_exp : Exp.t), args_map_fn actuals) with
        | ( ContinueProgram disjunct
          , (Const (Cfun procname) | Closure {name= procname})
          , (Exp.Lvar copy_pvar, copy_type) :: rest_args )
          when not (is_ptr_to_trivially_copyable copy_type) ->
            let default = (astate_non_disj, exec_state) in
            copy_check_fn procname
            |> Option.value_map ~default ~f:(fun from ->
                   let copied_var = Var.of_pvar copy_pvar in
                   if Var.appears_in_source_code copied_var && not (Var.is_global copied_var) then
                     let copied, disjunct, source_addr_typ_opt =
                       get_copied_and_source copy_type path rest_args location from disjunct
                     in
                     let copy_addr, _ = Option.value_exn (Stack.find_opt copied_var disjunct) in
                     let disjunct' =
                       Option.value_map source_addr_typ_opt ~default:disjunct
                         ~f:(fun (source_addr, source_typ) ->
                           AddressAttributes.add_one source_addr
                             (CopiedInto (Attribute.CopiedInto.IntoVar copied_var)) disjunct
                           |> AddressAttributes.add_one copy_addr
                                (SourceOriginOfCopy
                                   { source= source_addr
                                   ; is_const_ref= Typ.is_const_reference source_typ } ) )
                     in
                     ( NonDisjDomain.add_var copied_var
                         ~source_addr_opt:(Option.map source_addr_typ_opt ~f:fst)
                         copied astate_non_disj
                     , ExecutionDomain.continue disjunct' )
                   else default )
        | ( ContinueProgram disjunct
          , (Const (Cfun procname) | Closure {name= procname})
          , ((Exp.Lfield (_, field, _) as exp), copy_type) :: ((_, source_typ) :: _ as rest_args) )
          when Typ.is_rvalue_reference source_typ && not (is_ptr_to_trivially_copyable copy_type) ->
            let default = (astate_non_disj, exec_state) in
            copy_check_fn procname
            |> Option.value_map ~default ~f:(fun from ->
                   let copied, disjunct, source_addr_typ_opt =
                     get_copied_and_source copy_type path rest_args location from disjunct
                   in
                   match PulseOperations.eval path NoAccess location exp disjunct with
                   | Ok (disjunct, (copy_addr, _)) ->
                       let disjunct' =
                         Option.value_map source_addr_typ_opt ~default:disjunct
                           ~f:(fun (source_addr, _) ->
                             AddressAttributes.add_one source_addr
                               (CopiedInto (Attribute.CopiedInto.IntoField {field; from}))
                               disjunct
                             |> AddressAttributes.add_one copy_addr
                                  (SourceOriginOfCopy
                                     { source= source_addr
                                     ; is_const_ref= Typ.is_const_reference source_typ } ) )
                       in
                       ( NonDisjDomain.add_field field from
                           ~source_addr_opt:(Option.map source_addr_typ_opt ~f:fst)
                           copied astate_non_disj
                       , ExecutionDomain.continue disjunct' )
                   | Recoverable _ | FatalError _ ->
                       default )
        | ExceptionRaised _, _, _
        | ISLLatentMemoryError _, _, _
        | AbortProgram _, _, _
        | ContinueProgram _, _, _
        | ExitProgram _, _, _
        | LatentAbortProgram _, _, _
        | LatentInvalidAccess _, _, _ ->
            (astate_non_disj, exec_state) )
  in
  let copy_from_fn pname =
    let open Attribute.CopyOrigin in
    if Procname.is_copy_ctor pname then Some CopyCtor
    else if Procname.is_copy_assignment pname then Some CopyAssignment
    else None
  in
  let astate_n, astates = aux (copy_from_fn, Fn.id) astate_non_disj astates in
  (* For functions that return a copy, the last argument is the assigned copy *)
  aux (get_modeled_as_returning_copy_opt, List.rev) astate_n astates


let add_const_refable_parameters procdesc astates astate_non_disj =
  let proc_parameters = Procdesc.get_passed_by_value_formals procdesc in
  let location = Procdesc.get_loc procdesc in
  List.fold astates ~init:astate_non_disj
    ~f:(fun astate_non_disj ((exec_state : ExecutionDomain.t), _) ->
      match exec_state with
      | ContinueProgram disjunct ->
          List.fold proc_parameters ~init:astate_non_disj ~f:(fun astate_non_disj (pvar, typ) ->
              let is_ptr_to_trivially_copyable typ =
                Typ.is_pointer typ && Typ.is_trivially_copyable (Typ.strip_ptr typ).quals
              in
              let var = Var.of_pvar pvar in
              if
                Var.appears_in_source_code var && Typ.is_reference typ
                && not (is_ptr_to_trivially_copyable typ)
              then
                NonDisjDomain.add_parameter var
                  (NonDisjDomain.Unmodified
                     {heap= (disjunct.post :> BaseDomain.t).heap; typ; location} )
                  astate_non_disj
              else astate_non_disj )
      | ISLLatentMemoryError _
      | AbortProgram _
      | ExceptionRaised _
      | ExitProgram _
      | LatentAbortProgram _
      | LatentInvalidAccess _ ->
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
  BaseAddressAttributes.is_copied_from_const_ref addr current_attrs
  && BaseAddressAttributes.is_std_moved addr current_attrs
  || aux ~addr_to_explore:[addr] ~visited:AbstractValue.Set.empty


let is_modified ?(is_source_opt = None) address astate heap =
  let reachable_addresses_from_copy =
    BaseDomain.reachable_addresses_from (Caml.List.to_seq [address])
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
    match is_source_opt with
    | None ->
        ()
    | Some s ->
        L.d_printfln_escaped "%s reachable heap %a"
          (if s then "Source" else "Copy")
          BaseMemory.pp (reachable_from heap) ) ;
  is_modified_since_copy address ~current_heap ~copy_heap:heap ~current_attrs
    ~reachable_addresses_from_copy


let mark_modified_address_at ~address ~source_addr_opt ?(is_source = false) ~copied_into astate
    (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_copy_as_modified ~copied_into ~source_addr_opt astate_n
    ~is_modified:(is_modified ~is_source_opt:(Some is_source) address astate)


let mark_modified_parameter_at ~address ~var astate (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_parameter_as_modified ~var astate_n
    ~is_modified:(is_modified ~is_source_opt:None address astate)


let mark_modified_copies_and_parameters_with vars ~astate astate_n =
  let mark_modified_copy var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           let source_addr_opt = AddressAttributes.get_source_origin_of_copy address astate in
           mark_modified_address_at ~address ~source_addr_opt
             ~copied_into:(Attribute.CopiedInto.IntoVar var) astate default )
  in
  let mark_modified_parameter var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           mark_modified_parameter_at ~address ~var astate default )
  in
  List.fold vars ~init:astate_n ~f:(fun astate_n var ->
      let astate_n = mark_modified_parameter var astate_n in
      let res_opt =
        let open IOption.Let_syntax in
        let* source_addr, _ = Stack.find_opt var astate in
        let+ copied_into = AddressAttributes.get_copied_into source_addr astate in
        mark_modified_address_at ~address:source_addr ~source_addr_opt:(Some source_addr)
          ~is_source:true ~copied_into astate astate_n
      in
      match res_opt with Some res -> res | None -> mark_modified_copy var astate_n )


let mark_modified_copies_and_parameters vars disjuncts astate_n =
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
          mark_modified_copies_and_parameters_with unchecked_vars ~astate astate_n )
(* let copy_domain = mark_modified_copies_with unchecked_vars ~astate astate_n in
   mark_modified_parameters_with unchecked_vars ~astate copy_domain ) *)
