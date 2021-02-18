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
  type t = AbstractValue.t HilExp.Access.t [@@deriving yojson_of]

  let compare = HilExp.Access.loose_compare AbstractValue.compare

  let equal = [%compare.equal: t]

  let pp = HilExp.Access.pp AbstractValue.pp

  let canonicalize ~get_var_repr (access : t) =
    match access with
    | ArrayAccess (typ, addr) ->
        HilExp.Access.ArrayAccess (typ, get_var_repr addr)
    | FieldAccess _ | TakeAddress | Dereference ->
        access
end

module AccessSet = Caml.Set.Make (Access)

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


  let subst_var (v, v') edges =
    M.map edges ~f:(fun ((addr, hist) as addr_hist) ->
        if AbstractValue.equal addr v then (v', hist) else addr_hist )


  include M
end

module Graph = PrettyPrintable.MakePPMonoMap (AbstractValue) (Edges)

let register_address addr memory =
  if Graph.mem addr memory then memory else Graph.add addr Edges.empty memory


let add_edge addr_src access value memory =
  let old_edges = Graph.find_opt addr_src memory |> Option.value ~default:Edges.empty in
  let new_edges = Edges.add access value old_edges in
  if phys_equal old_edges new_edges then memory else Graph.add addr_src new_edges memory


let find_edge_opt addr access memory =
  let open Option.Monad_infix in
  Graph.find_opt addr memory >>= Edges.find_opt access


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


let subst_var (v, v') memory =
  (* subst in edges *)
  let memory =
    let v_appears_in_edges =
      Graph.exists
        (fun _ edges -> Edges.exists ~f:(fun (_, (dest, _)) -> AbstractValue.equal v dest) edges)
        memory
    in
    if v_appears_in_edges then Graph.map (Edges.subst_var (v, v')) memory else memory
  in
  (* subst in the domain of the graph, already substituted in edges above *)
  match Graph.find_opt v memory with
  | None ->
      Sat memory
  | Some edges -> (
      let memory = Graph.remove v memory in
      match Graph.find_opt v' memory with
      | None ->
          Sat (Graph.add v' edges memory)
      | Some edges' ->
          if Edges.is_empty edges then Sat memory
          else if Edges.is_empty edges' then Sat (Graph.add v' edges memory)
          else (
            (* both set of edges being non-empty means that [v] and [v'] have been treated as
               disjoint memory locations until now, contradicting the fact they are equal *)
            L.d_printfln "CONTRADICTION: %a = %a, which is already allocated in %a@\n"
              AbstractValue.pp v AbstractValue.pp v' Graph.pp memory ;
            Unsat ) )


include Graph

let compare = Graph.compare Edges.compare

let equal = Graph.equal Edges.equal
