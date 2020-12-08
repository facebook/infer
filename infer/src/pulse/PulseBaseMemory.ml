(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface

(* {3 Heap domain } *)

module Access = struct
  type t = AbstractValue.t HilExp.Access.t [@@deriving compare, yojson_of]

  let equal = [%compare.equal: t]

  let pp = HilExp.Access.pp AbstractValue.pp

  let canonicalize ~get_var_repr (access : t) =
    match access with
    | ArrayAccess (typ, addr) ->
        HilExp.Access.ArrayAccess (typ, get_var_repr addr)
    | FieldAccess _ | TakeAddress | Dereference ->
        access
end

module AddrTrace = struct
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare, yojson_of]

  let pp fmt addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp fmt addr_trace
    else AbstractValue.pp fmt (fst addr_trace)
end

module Edges = struct
  module M =
    RecencyMap.Make (Access) (AddrTrace)
      (struct
        let limit = Config.pulse_recency_limit
      end)

  let yojson_of_t edges = [%yojson_of: (Access.t * AddrTrace.t) list] (M.bindings edges)

  let canonicalize ~get_var_repr edges =
    M.fold ~init:M.empty edges ~f:(fun edges' (access, (addr, hist)) ->
        let addr' = get_var_repr addr in
        let access' = Access.canonicalize ~get_var_repr access in
        M.add access' (addr', hist) edges' )


  include M
end

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



let yojson_of_t g = [%yojson_of: (AbstractValue.t * Edges.t) list] (Graph.bindings g)

let is_allocated memory v =
  Graph.find_opt v memory |> Option.exists ~f:(fun edges -> not (Edges.is_empty edges))


let canonicalize ~get_var_repr memory =
  let exception AliasingContradiction in
  try
    let memory =
      Graph.fold
        (fun addr edges g ->
          if Edges.is_empty edges then g
          else
            let addr' = get_var_repr addr in
            if is_allocated g addr' then (
              L.d_printfln "CONTRADICTION: %a = %a, which is already allocated in %a@\n"
                AbstractValue.pp addr AbstractValue.pp addr' Graph.pp g ;
              raise_notrace AliasingContradiction )
            else
              let edges' = Edges.canonicalize ~get_var_repr edges in
              Graph.add addr' edges' g )
        memory Graph.empty
    in
    Sat memory
  with AliasingContradiction -> Unsat


include Graph
