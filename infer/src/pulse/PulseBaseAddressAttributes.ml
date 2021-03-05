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


let mark_as_end_of_collection address memory = add_one address Attribute.EndOfCollection memory

let check_valid address attrs =
  L.d_printfln "Checking validity of %a" AbstractValue.pp address ;
  match Graph.find_opt address attrs |> Option.bind ~f:Attributes.get_invalid with
  | None ->
      Ok ()
  | Some invalidation ->
      Error invalidation


let check_initialized address attrs =
  L.d_printfln "Checking if %a is initialized" AbstractValue.pp address ;
  if Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_uninitialized then Error ()
  else Ok ()


let get_attribute getter address attrs =
  let open Option.Monad_infix in
  Graph.find_opt address attrs >>= getter


let remove_allocation_attr address memory =
  match get_attribute Attributes.get_allocation address memory with
  | Some (procname, trace) ->
      remove_one address (Attribute.Allocated (procname, trace)) memory
  | None ->
      memory


let remove_isl_abduced_attr address memory =
  match get_attribute Attributes.get_isl_abduced address memory with
  | Some trace ->
      remove_one address (Attribute.ISLAbduced trace) memory
  | None ->
      memory


let remove_must_be_valid_attr address memory =
  match get_attribute Attributes.get_must_be_valid address memory with
  | Some trace ->
      remove_one address (Attribute.MustBeValid trace) memory
  | None ->
      memory


let initialize address attrs =
  if Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_uninitialized then
    remove_one address Attribute.Uninitialized attrs
  else attrs


let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_invalid = get_attribute Attributes.get_invalid

let get_must_be_valid = get_attribute Attributes.get_must_be_valid

let get_must_be_valid_or_allocated_isl address attrs =
  match get_must_be_valid address attrs with
  | Some trace ->
      Some trace
  | None -> (
    match get_attribute Attributes.get_allocation address attrs with
    | Some (_, trace) ->
        Some trace
    | None ->
        get_attribute Attributes.get_isl_abduced address attrs )


let get_must_be_initialized = get_attribute Attributes.get_must_be_initialized

let add_dynamic_type typ address memory = add_one address (Attribute.DynamicType typ) memory

let get_dynamic_type attrs v = get_attribute Attributes.get_dynamic_type v attrs

let std_vector_reserve address memory = add_one address Attribute.StdVectorReserve memory

let is_end_of_collection address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_end_of_collection


let is_std_vector_reserved address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_std_vector_reserved


let canonicalize ~get_var_repr attrs_map =
  (* TODO: merging attributes together can produce contradictory attributes, eg [MustBeValid] +
     [Invalid]. We could detect these and abort execution. This is not really restricted to merging
     as it might be possible to get a contradiction by accident too so maybe here is not the best
     place to detect these. *)
  Graph.fold
    (fun addr attrs g ->
      if Attributes.is_empty attrs then g
      else
        let addr' = get_var_repr addr in
        add addr' attrs g )
    attrs_map Graph.empty


let subst_var (v, v') attrs_map =
  if Graph.mem v attrs_map then
    canonicalize attrs_map ~get_var_repr:(fun addr ->
        if AbstractValue.equal addr v then v' else addr )
  else attrs_map
