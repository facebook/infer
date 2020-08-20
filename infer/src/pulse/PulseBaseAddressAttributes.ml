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
      Graph.add addr new_attrs attrs


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

let is_empty = Graph.is_empty

let remove = Graph.remove
           
let filter = Graph.filter

let filter_with_discarded_addrs f x =
  fold
    (fun k v ((x, discarded) as acc) -> if f k v then acc else (Graph.remove k x, k :: discarded))
    x (x, [])


let pp = Graph.pp

let invalidate (address, history) invalidation location memory =
  add_one address (Attribute.Invalid (invalidation, Immediate {location; history})) memory


let allocate procname (address, history) location memory =
  add_one address (Attribute.Allocated (procname, Immediate {location; history})) memory


let add_dynamic_type typ address memory = add_one address (Attribute.DynamicType typ) memory

let mark_as_end_of_collection address memory = add_one address Attribute.EndOfCollection memory

let check_valid address attrs =
  L.d_printfln "Checking validity of %a" AbstractValue.pp address ;
  match Graph.find_opt address attrs |> Option.bind ~f:Attributes.get_invalid with
  | None ->
      Ok ()
  | Some invalidation ->
      Error invalidation


let get_attribute getter address attrs =
  let open Option.Monad_infix in
  Graph.find_opt address attrs >>= getter


let remove_allocation_attr address memory =
  match get_attribute Attributes.get_allocation address memory with
  | Some (procname, trace) ->
      remove_one address (Attribute.Allocated (procname, trace)) memory
  | None ->
     memory

let remove_abdallocation_attr address memory =
  match get_attribute Attributes.get_abdallocation address memory with
  | Some (procname, trace) ->
      remove_one address (Attribute.AbdAllocated (procname, trace)) memory
  | None ->
     memory

let remove_must_be_valid_attr address memory =
  match get_attribute Attributes.get_must_be_valid address memory with
  | Some trace ->
      remove_one address (Attribute.MustBeValid trace) memory
  | None ->
      memory


let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_invalid = get_attribute Attributes.get_invalid

exception Found of (AbstractValue.t * Invalidation.t * Trace.t)
                
let find_first_invalid memory=
  try
      Graph.fold (fun addr attrs acc ->
              match Attributes.get_invalid attrs with
                | Some (invalidation, trace) ->
                   raise (Found (addr, invalidation, trace))
                | None ->
                   acc) memory None
  with Found res -> Some res

let get_address_of_stack_variable =
  get_attribute Attributes.get_address_of_stack_variable

let exist_invalid addrs addr_attrs=
  AbstractValue.Set.exists (fun addr -> Option.is_some (get_invalid addr addr_attrs)) addrs

let is_invalid_const address attrs =
  let invalid_attrs = get_attribute Attributes.get_invalid address attrs in
  match invalid_attrs with Some (ConstantDereference _, _) -> true | _ -> false

let get_must_be_valid =
  get_attribute Attributes.get_must_be_valid

let get_must_be_valid_or_allocated address attrs =
  match get_must_be_valid address attrs with
    | Some a -> Some a
    | None -> ( match get_attribute Attributes.get_allocation address attrs with
                  | Some (_, trace) -> Some trace
                  | None -> ( match get_attribute Attributes.get_abdallocation address attrs with
                                | Some (_, trace) -> Some trace
                                | None -> None
                            )
              )

let std_vector_reserve address memory = add_one address Attribute.StdVectorReserve memory

let is_end_of_collection address attrs =
  Graph.find_opt address attrs |> Option.value_map ~default:false ~f:Attributes.is_end_of_collection


let is_std_vector_reserved address attrs =
  Graph.find_opt address attrs
  |> Option.value_map ~default:false ~f:Attributes.is_std_vector_reserved


let is_empty_heap_attrs addresses hattrs=
  not (Graph.exists (fun addr attrs -> AbstractValue.Set.mem addr addresses && (Attributes.is_not_empty_heap attrs)) hattrs)

module GraphS = PrettyPrintable.MakePPMonoMap (AbstractValue) (Attribute.Set)

type st = GraphS.t

let sempty = GraphS.empty

let sfold = GraphS.fold
           
let union addr attributes attrs =
  match GraphS.find_opt addr attrs with
  | None ->
        let new_attrs = Attributes.fold attributes ~init:Attribute.Set.empty ~f:(fun acc attr -> Attribute.Set.add attr acc) in
      GraphS.add addr new_attrs attrs
  | Some old_attrs ->
      let new_attrs = Attributes.fold attributes ~init:old_attrs ~f:(fun acc attr -> Attribute.Set.add attr acc) in
      GraphS.add addr new_attrs attrs
