(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PulseBasicInterface

module AttributesNoRank = struct
  include Attributes

  let pp fmt t : unit = Attributes.pp ?print_rank:None fmt t
end

module Graph = PrettyPrintable.MakePPMonoMap (AbstractValue) (AttributesNoRank)

type t = Graph.t

let compare = Graph.compare AttributesNoRank.compare

let equal = Graph.equal AttributesNoRank.equal

let for_all = Graph.for_all

let yojson_of_t = [%yojson_of: _]

let add_one addr attribute attrs =
  match Graph.find_opt addr attrs with
  | None ->
      Graph.add addr (Attributes.singleton attribute) attrs
  | Some old_attrs ->
      let new_attrs = Attributes.add old_attrs attribute in
      Graph.add addr new_attrs attrs


let remove_one addr attribute attrs =
  match Graph.find_opt addr attrs with
  | None ->
      attrs
  | Some old_attrs ->
      let new_attrs = Attributes.remove attribute old_attrs in
      if Attributes.is_empty new_attrs then Graph.remove addr attrs
      else Graph.add addr new_attrs attrs


let add addr attributes attrs =
  match Graph.find_opt addr attrs with
  | None ->
      Graph.add addr attributes attrs
  | Some old_attrs ->
      let new_attrs = Attributes.union_prefer_left old_attrs attributes in
      Graph.add addr new_attrs attrs


let fold = Graph.fold

let find_opt = Graph.find_opt

let empty = Graph.empty

let filter = Graph.filter

(* for an abstract value v, where f_keep(v) == false, find an abstract value v_keep, where
   f_keep(v_keep) == true and where v_keep has taint propagated from v *)
let mk_transitive_taint_from_subst f_keep memory =
  (* construct map v -> [v1,...,vn], where taint is propagated from v1,...,vn to v *)
  let taints_from_map =
    fold
      (fun addr attrs taint_from_map ->
        match Attributes.get_propagate_taint_from attrs with
        | None ->
            taint_from_map
        | Some taints_in ->
            List.fold ~init:taint_from_map
              ~f:(fun acc {Attribute.v= from} ->
                match AbstractValue.Map.find_opt addr acc with
                | None ->
                    AbstractValue.Map.add addr [from] acc
                | Some taint_from ->
                    AbstractValue.Map.add addr (from :: taint_from) acc )
              taints_in )
      memory AbstractValue.Map.empty
  in
  let rec find_live_and_subst subst addr =
    match AbstractValue.Map.find_opt addr subst with
    | None -> (
        if f_keep addr then subst
        else
          (* to avoid cycles *)
          let subst = AbstractValue.Map.add addr AbstractValue.Set.empty subst in
          match AbstractValue.Map.find_opt addr taints_from_map with
          | None ->
              subst
          | Some taints_from ->
              let subst =
                List.fold taints_from
                  ~f:(fun subst addr -> find_live_and_subst subst addr)
                  ~init:subst
              in
              let new_taints_from =
                List.fold taints_from ~init:AbstractValue.Set.empty ~f:(fun acc addr ->
                    if f_keep addr then AbstractValue.Set.add addr acc
                    else AbstractValue.Set.union (AbstractValue.Map.find addr subst) acc )
              in
              AbstractValue.Map.add addr new_taints_from subst )
    | Some _ ->
        subst
  in
  AbstractValue.Map.fold
    (fun addr _taint_from subst -> find_live_and_subst subst addr)
    taints_from_map AbstractValue.Map.empty


let filter_with_discarded_addrs f_keep memory =
  let taint_from_subst = mk_transitive_taint_from_subst f_keep memory in
  fold
    (fun addr attrs ((memory, discarded) as acc) ->
      if f_keep addr then
        let attrs' =
          Attributes.fold attrs ~init:attrs ~f:(fun attrs' attr ->
              match Attribute.filter_unreachable taint_from_subst f_keep attr with
              | None ->
                  Attributes.remove attr attrs'
              | Some attr' ->
                  if phys_equal attr attr' then attrs'
                  else
                    let attrs' = Attributes.remove attr attrs' in
                    Attributes.add attrs' attr' )
        in
        if phys_equal attrs attrs' then acc
        else
          ( Graph.update addr
              (fun _ -> if Attributes.is_empty attrs' then None else Some attrs')
              memory
          , (* HACK: don't add to the discarded addresses even if we did discard all the attributes
               of the address; this is ok because the list of discarded addresses is only relevant
               to allocation attributes, which are not affected by this filtering... sorry! *)
            discarded )
      else (Graph.remove addr memory, addr :: discarded) )
    memory (memory, [])


let pp = Graph.pp

let invalidate (address, history) invalidation location memory =
  add_one address (Attribute.Invalid (invalidation, Immediate {location; history})) memory


let always_reachable address memory = add_one address Attribute.AlwaysReachable memory

let allocate allocator address location memory =
  add_one address
    (Attribute.Allocated (allocator, Immediate {location; history= ValueHistory.epoch}))
    memory


let java_resource_release address memory = add_one address Attribute.JavaResourceReleased memory

let mark_as_end_of_collection address memory = add_one address Attribute.EndOfCollection memory

let check_valid address attrs =
  L.d_printfln "Checking validity of %a" AbstractValue.pp address ;
  match Graph.find_opt address attrs |> Option.bind ~f:Attributes.get_invalid with
  | None ->
      Ok ()
  | Some invalidation ->
      L.d_printfln ~color:Red "INVALID: %a" Invalidation.pp (fst invalidation) ;
      Error invalidation


let check_initialized address attrs =
  L.d_printfln "Checking if %a is initialized" AbstractValue.pp address ;
  if Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_uninitialized then (
    L.d_printfln ~color:Red "UNINITIALIZED" ;
    Error () )
  else Ok ()


let get_attribute getter address attrs =
  let open Option.Monad_infix in
  Graph.find_opt address attrs >>= getter


let remove_allocation_attr address memory =
  match get_attribute Attributes.get_allocation address memory with
  | Some (allocator, trace) ->
      remove_one address (Attribute.Allocated (allocator, trace)) memory
  | None ->
      memory


let remove_tainted address memory =
  remove_one address (Attribute.Tainted Attribute.TaintedSet.empty) memory


let remove_taint_sanitizer address memory =
  remove_one address (Attribute.TaintSanitized Attribute.TaintSanitizedSet.empty) memory


let remove_propagate_taint_from address memory =
  match get_attribute Attributes.get_propagate_taint_from address memory with
  | Some taint ->
      remove_one address (Attribute.PropagateTaintFrom taint) memory
  | None ->
      memory


let remove_taint_attrs address memory =
  remove_tainted address memory |> remove_taint_sanitizer address
  |> remove_propagate_taint_from address


let remove_isl_abduced_attr address memory =
  match get_attribute Attributes.get_isl_abduced address memory with
  | Some trace ->
      remove_one address (Attribute.ISLAbduced trace) memory
  | None ->
      memory


let remove_must_be_valid_attr address memory =
  match get_attribute Attributes.get_must_be_valid address memory with
  | Some (timestamp, trace, reason) ->
      remove_one address (Attribute.MustBeValid (timestamp, trace, reason)) memory
  | None ->
      memory


let remove_unsuitable_for_summary =
  Graph.filter_map (fun _addr attrs ->
      let new_attrs = Attributes.remove_unsuitable_for_summary attrs in
      if Attributes.is_empty new_attrs then None else Some new_attrs )


let initialize address attrs =
  if Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_uninitialized then
    remove_one address Attribute.Uninitialized attrs
  else attrs


let get_allocation = get_attribute Attributes.get_allocation

let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_copied_into = get_attribute Attributes.get_copied_into

let get_source_origin_of_copy address attrs =
  get_attribute Attributes.get_source_origin_of_copy address attrs |> Option.map ~f:fst


let is_copied_from_const_ref address attrs =
  get_attribute Attributes.get_source_origin_of_copy address attrs
  |> Option.exists ~f:(fun (_, is_const_ref) -> is_const_ref)


let get_invalid = get_attribute Attributes.get_invalid

let get_must_be_valid = get_attribute Attributes.get_must_be_valid

let get_must_not_be_tainted address memory =
  match Graph.find_opt address memory with
  | None ->
      Attribute.TaintSinkSet.empty
  | Some attrs ->
      Attributes.get_must_not_be_tainted attrs


let is_must_be_valid_or_allocated_isl address attrs =
  Option.is_some (get_must_be_valid address attrs)
  || Option.is_some (get_attribute Attributes.get_allocation address attrs)
  || Option.is_some (get_attribute Attributes.get_isl_abduced address attrs)


let get_must_be_initialized = get_attribute Attributes.get_must_be_initialized

let get_written_to = get_attribute Attributes.get_written_to

let add_dynamic_type typ address memory = add_one address (Attribute.DynamicType typ) memory

let get_dynamic_type attrs v = get_attribute Attributes.get_dynamic_type v attrs

let add_ref_counted address memory = add_one address Attribute.RefCounted memory

let is_ref_counted address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_ref_counted


let std_vector_reserve address memory = add_one address Attribute.StdVectorReserve memory

let add_unreachable_at address location memory = add_one address (UnreachableAt location) memory

let is_end_of_collection address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_end_of_collection


let is_java_resource_released adress attrs =
  Graph.find_opt adress attrs |> Option.exists ~f:Attributes.is_java_resource_released


let is_std_moved address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_std_moved


let is_std_vector_reserved address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_std_vector_reserved


let canonicalize_common ~for_summary ~get_var_repr attrs_map =
  (* TODO: merging attributes together can produce contradictory attributes, eg [MustBeValid] +
     [Invalid]. We could detect these and abort execution. This is not really restricted to merging
     as it might be possible to get a contradiction by accident too so maybe here is not the best
     place to detect these. *)
  Graph.fold
    (fun addr attrs g ->
      if Attributes.is_empty attrs then g
      else
        let addr' = get_var_repr addr in
        let attrs' =
          Graph.find_opt addr' g
          |> Option.fold ~init:attrs ~f:(fun attrs attrs' ->
                 (* "merge" attributes if two different values ([addr] and [addr']) are found to be
                    equal after attributes of the same kind were recorded for them. This arbitrarily
                    keeps one of them, with unclear but likely benign consequences. *)
                 Attributes.union_prefer_left attrs' attrs )
        in
        add addr' attrs' g )
    (if for_summary then remove_unsuitable_for_summary attrs_map else attrs_map)
    Graph.empty


let canonicalize ~get_var_repr attrs_map =
  canonicalize_common ~for_summary:true ~get_var_repr attrs_map


let subst_var ~for_summary (v, v') attrs_map =
  if Graph.mem v attrs_map then
    canonicalize_common ~for_summary attrs_map ~get_var_repr:(fun addr ->
        if AbstractValue.equal addr v then v' else addr )
  else attrs_map
