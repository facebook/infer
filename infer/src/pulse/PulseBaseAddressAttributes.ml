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

  let pp fmt t : unit = PulseAttribute.Attributes.pp fmt t
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

let partition = Graph.partition

let pp = Graph.pp

let invalidate (address, history) invalidation location memory =
  add_one address (Attribute.Invalid (invalidation, Immediate {location; history})) memory


let allocate (address, history) location memory =
  add_one address (Attribute.Allocated (Immediate {location; history})) memory


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


let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_citv = get_attribute Attributes.get_citv

let get_bo_itv v memory =
  match get_attribute Attributes.get_bo_itv v memory with
  | None ->
      Itv.ItvPure.of_pulse_value v
  | Some itv ->
      itv


let get_must_be_valid = get_attribute Attributes.get_must_be_valid

let std_vector_reserve address memory = add_one address Attribute.StdVectorReserve memory

let is_std_vector_reserved address attrs =
  Graph.find_opt address attrs
  |> Option.value_map ~default:false ~f:Attributes.is_std_vector_reserved
