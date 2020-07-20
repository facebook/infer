(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseISLBasicInterface

(* {3 Heap domain } *)

module Access = struct
  type t = AbstractValue.t HilExp.Access.t [@@deriving compare]

  let equal = [%compare.equal: t]

  let pp = HilExp.Access.pp AbstractValue.pp
end

module AddrTrace = struct
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare]

  let pp fmt addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp fmt addr_trace
    else AbstractValue.pp fmt (fst addr_trace)
end

module Edges =
  RecencyMap.Make (Access) (AddrTrace)
    (struct
      let limit = Config.pil_recency_limit
    end)

module Graph = PrettyPrintable.MakePPMonoMap (AbstractValue) (Edges)

let register_address addr memory =
  if Graph.mem addr memory then memory else Graph.add addr Edges.empty memory

let remove_address addr memory =
   Graph.remove addr memory

let find_edges_opt addr memory =
  Graph.find_opt addr memory
  
let find_edge_opt addr access memory =
  let open Option.Monad_infix in
  find_edges_opt addr memory >>= Edges.find_opt access


let add_edge addr_src access value memory =
  let old_edges = Graph.find_opt addr_src memory |> Option.value ~default:Edges.empty in
  let new_edges = Edges.add access value old_edges in
  if phys_equal old_edges new_edges then memory else Graph.add addr_src new_edges memory

let exist_edge_dest dest_addr=
  Graph.exists (fun _ e ->
     let ne = Edges.filter e ~f:(fun (_, (addr, _)) ->
                      AbstractValue.equal addr dest_addr) in
     not (Edges.is_empty ne)
      )


  
include Graph
