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

(* {3 Heap domain } *)

module Access = struct
  type t = AbstractValue.t HilExp.Access.t [@@deriving compare]

  let equal = [%compare.equal: t]

  let pp = HilExp.Access.pp AbstractValue.pp
end

module Edges = PrettyPrintable.MakePPMap (Access)

type edges = (AbstractValue.t * ValueHistory.t) Edges.t [@@deriving compare]

let pp_edges =
  Edges.pp ~pp_value:(fun f addr_trace ->
      if Config.debug_level_analysis >= 3 then
        Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp f addr_trace
      else AbstractValue.pp f (fst addr_trace) )


type cell = edges * Attributes.t

module Graph = PrettyPrintable.MakePPMap (AbstractValue)

type t = edges Graph.t * Attributes.t Graph.t

let pp_heap fmt (heap, _) = Graph.pp ~pp_value:pp_edges fmt heap

let pp_attributes fmt (_, attrs) = Graph.pp ~pp_value:Attributes.pp fmt attrs

let register_address addr memory =
  if Graph.mem addr (fst memory) then memory
  else (Graph.add addr Edges.empty (fst memory), snd memory)


(* {3 Helper functions to traverse the two maps at once } *)

let add_edge addr_src access value memory =
  let old_edges = Graph.find_opt addr_src (fst memory) |> Option.value ~default:Edges.empty in
  let new_edges = Edges.add access value old_edges in
  if phys_equal old_edges new_edges then memory
  else (Graph.add addr_src new_edges (fst memory), snd memory)


let find_edge_opt addr access memory =
  let open Option.Monad_infix in
  Graph.find_opt addr (fst memory) >>= Edges.find_opt access


let add_attribute addr attribute memory =
  match Graph.find_opt addr (snd memory) with
  | None ->
      (fst memory, Graph.add addr (Attributes.singleton attribute) (snd memory))
  | Some old_attrs ->
      let new_attrs = Attributes.add old_attrs attribute in
      (fst memory, Graph.add addr new_attrs (snd memory))


let invalidate (address, history) invalidation location memory =
  add_attribute address (Attribute.Invalid (invalidation, Immediate {location; history})) memory


let check_valid address memory =
  L.d_printfln "Checking validity of %a" AbstractValue.pp address ;
  match Graph.find_opt address (snd memory) |> Option.bind ~f:Attributes.get_invalid with
  | Some invalidation ->
      Error invalidation
  | None ->
      Ok ()


let get_attribute getter address memory =
  let open Option.Monad_infix in
  Graph.find_opt address (snd memory) >>= getter


let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_arithmetic = get_attribute Attributes.get_arithmetic

let get_must_be_valid = get_attribute Attributes.get_must_be_valid

let std_vector_reserve address memory = add_attribute address Attribute.StdVectorReserve memory

let is_std_vector_reserved address memory =
  Graph.find_opt address (snd memory)
  |> Option.value_map ~default:false ~f:(fun attributes ->
         Attributes.is_std_vector_reserved attributes )


(* {3 Monomorphic {!PPMap} interface as needed } *)

let empty = (Graph.empty, Graph.empty)

let find_edges_opt addr memory = Graph.find_opt addr (fst memory)

let find_attrs_opt addr memory = Graph.find_opt addr (snd memory)

let find_opt addr memory =
  match (find_edges_opt addr memory, find_attrs_opt addr memory) with
  | None, None ->
      None
  | edges_opt, attrs_opt ->
      let edges = Option.value edges_opt ~default:Edges.empty in
      let attrs = Option.value attrs_opt ~default:Attributes.empty in
      Some (edges, attrs)


let fold_attrs f memory init = Graph.fold f (snd memory) init

let set_attrs addr attrs memory = (fst memory, Graph.add addr attrs (snd memory))

let set_edges addr edges memory = (Graph.add addr edges (fst memory), snd memory)

let set_cell addr (edges, attrs) memory =
  (Graph.add addr edges (fst memory), Graph.add addr attrs (snd memory))


let filter f memory =
  let heap = Graph.filter (fun address _ -> f address) (fst memory) in
  let attrs = Graph.filter (fun address _ -> f address) (snd memory) in
  if phys_equal heap (fst memory) && phys_equal attrs (snd memory) then memory else (heap, attrs)


let filter_heap f memory =
  let heap = Graph.filter f (fst memory) in
  if phys_equal heap (fst memory) then memory else (heap, snd memory)


let mem_edges addr memory = Graph.mem addr (fst memory)
