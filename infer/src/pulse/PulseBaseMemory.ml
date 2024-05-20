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

module Access = PulseAccess

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
    let edges', changed =
      M.fold edges ~init:(M.empty, false) ~f:(fun (edges', changed) (access, (addr, hist)) ->
          let addr' = get_var_repr addr in
          let access' = Access.canonicalize ~get_var_repr access in
          let changed =
            changed || not (AbstractValue.equal addr addr' && phys_equal access access')
          in
          (M.add access' (addr', hist) edges', changed) )
    in
    if changed then edges' else edges


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


let find_edge_opt ?get_var_repr addr access memory =
  let open Option.Monad_infix in
  Graph.find_opt addr memory
  >>= fun edges ->
  let res = Edges.find_opt access edges in
  match res with
  | Some _ ->
      res
  | None -> (
    match (access, get_var_repr) with
    | Access.ArrayAccess _, Some get_var_repr ->
        let access = Access.canonicalize ~get_var_repr access in
        let edges = Edges.canonicalize ~get_var_repr edges in
        Edges.find_opt access edges
    | _, _ ->
        res )


let has_edge addr access memory = find_edge_opt addr access memory |> Option.is_some

let yojson_of_t g = [%yojson_of: (AbstractValue.t * Edges.t) list] (Graph.bindings g)

let is_allocated memory v =
  Graph.find_opt v memory |> Option.exists ~f:(fun edges -> not (Edges.is_empty edges))


let canonicalize ~get_var_repr memory =
  let exception AliasingContradiction in
  try
    let memory', changed =
      Graph.fold
        (fun addr edges (g, changed) ->
          if Edges.is_empty edges then (g, true)
          else
            let addr' = get_var_repr addr in
            if is_allocated g addr' then (
              L.d_printfln "CONTRADICTION: %a = %a, which is already allocated in %a@\n"
                AbstractValue.pp addr AbstractValue.pp addr' Graph.pp g ;
              raise_notrace AliasingContradiction )
            else
              let edges' = Edges.canonicalize ~get_var_repr edges in
              let changed =
                changed || not (AbstractValue.equal addr addr' && phys_equal edges edges')
              in
              (Graph.add addr' edges' g, changed) )
        memory (Graph.empty, false)
    in
    if changed then Sat memory' else Sat memory
  with AliasingContradiction -> Unsat


let subst_var ~for_summary (v, v') memory =
  (* subst in edges only if we are computing the summary; otherwise avoid this expensive rewriting
     as values are normalized when going out of the edges (on reads) on the fly which is
     functionally equivalent *)
  let memory =
    if for_summary then
      let v_appears_in_edges =
        Graph.exists
          (fun _ edges -> Edges.exists ~f:(fun (_, (dest, _)) -> AbstractValue.equal v dest) edges)
          memory
      in
      if v_appears_in_edges then Graph.map (Edges.subst_var (v, v')) memory else memory
    else memory
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

module type S = sig
  type key

  type in_map_t

  type out_of_map_t

  module Access : Access.S with type key := key

  module Edges : sig
    include RecencyMap.S with type key = Access.t and type value = out_of_map_t

    val mapi : t -> f:(key -> out_of_map_t -> in_map_t) -> t

    val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t
  end

  include PrettyPrintable.PPMonoMap with type key := key and type value = Edges.t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val register_address : key -> t -> t

  val add_edge : key -> Access.t -> in_map_t -> t -> t

  val find_edge_opt :
       ?get_var_repr:(AbstractValue.t -> AbstractValue.t)
    -> key
    -> Access.t
    -> t
    -> out_of_map_t option

  val has_edge : key -> Access.t -> t -> bool

  val is_allocated : t -> key -> bool
end
