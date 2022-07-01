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
        let addr' = get_var_repr addr in
        if AbstractValue.equal addr addr' then access else HilExp.Access.ArrayAccess (typ, addr')
    | FieldAccess _ | TakeAddress | Dereference ->
        access


  let is_strong_access tenv (access : t) =
    let has_weak_or_unretained_or_assign annotations =
      List.exists annotations ~f:(fun (ann : Annot.t) ->
          ( String.equal ann.class_name Config.property_attributes
          || String.equal ann.class_name Config.ivar_attributes )
          && List.exists
               ~f:(fun Annot.{value} ->
                 Annot.has_matching_str_value value ~pred:(fun att ->
                     String.equal Config.unsafe_unret att
                     || String.equal Config.weak att || String.equal Config.assign att ) )
               ann.parameters )
    in
    match access with
    | FieldAccess fieldname -> (
        let classname = Fieldname.get_class_name fieldname in
        let is_fake_capture_field_strong fieldname =
          (* a strongly referencing capture field is a capture field that is not weak *)
          Fieldname.is_fake_capture_field fieldname
          && not (Fieldname.is_fake_capture_field_weak fieldname)
        in
        match Tenv.lookup tenv classname with
        | None when is_fake_capture_field_strong fieldname ->
            (* Strongly referencing captures *)
            true
        | None ->
            (* Can't tell if we have a strong reference. To avoid FP on retain cycles,
               assume weak reference by default *)
            false
        | Some {fields} -> (
          match List.find fields ~f:(fun (name, _, _) -> Fieldname.equal name fieldname) with
          | None ->
              (* Can't tell if we have a strong reference. To avoid FP on retain cycles,
                 assume weak reference by default *)
              false
          | Some (_, typ, anns) -> (
            match typ.Typ.desc with
            | Tptr (_, (Pk_objc_weak | Pk_objc_unsafe_unretained)) ->
                false
            | _ ->
                not (has_weak_or_unretained_or_assign anns) ) ) )
    | _ ->
        true
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


let find_edge_opt addr access memory =
  let open Option.Monad_infix in
  Graph.find_opt addr memory >>= Edges.find_opt access


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
