(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module FiniteBounds = struct
  type t = int

  let leq ~lhs ~rhs = lhs <= rhs

  let join a b = max a b

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.fprintf fmt "%d" astate
end

module BoundsWithTop = struct
  open AbstractDomain.Types
  include AbstractDomain.TopLifted (FiniteBounds)

  let widening_threshold = 5

  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Top, _ | _, Top ->
        Top
    | NonTop prev, NonTop next when num_iters < widening_threshold ->
        NonTop (FiniteBounds.join prev next)
    | NonTop _, NonTop _ (* num_iters >= widening_threshold *) ->
        Top
end

module ResourcesHeld = AbstractDomain.Map (AccessPath) (BoundsWithTop)

(** Make [AccessPath] into an abstract domain to use in the [IdToAccessPath] map from [Ident.t] to
    [AccessPath.t]. We assume all idents will be distinct (mostly true) so we don't need to define
    careful domain operations. *)
module AccessPathDomain = struct
  include AccessPath

  let leq ~lhs ~rhs = equal lhs rhs

  let join access_path1 _access_path2 = access_path1

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module IdToAccessPath = AbstractDomain.Map (Ident) (AccessPathDomain)
module Domain = AbstractDomain.Pair (ResourcesHeld) (IdToAccessPath)
open AbstractDomain.Types

let initial = (ResourcesHeld.empty, IdToAccessPath.empty)

let update_count count n = match count with Top -> Top | NonTop held -> NonTop (held + n)

let incr_count count = update_count count 1

let decr_count count = update_count count (-1)

let find_count access_path held =
  match ResourcesHeld.find_opt access_path held with Some count -> count | None -> NonTop 0


let access_path_of_ident (_, id_map) ident = IdToAccessPath.find_opt ident id_map

let rec path_of_exp ((held, id_map) as astate) ((exp : Exp.t), typ) =
  match exp with
  | Lvar pvar ->
      Some (astate, AccessPath.of_pvar pvar typ)
  | Var ident -> (
    match access_path_of_ident astate ident with
    | Some access_path ->
        (* we know an access path for this ident already, use it *) Some (astate, access_path)
    | None ->
        (* default to an access path that is just the ident in case we need to store a resource for
           it *)
        let access_path = AccessPath.of_id ident typ in
        let id_map = IdToAccessPath.add ident access_path id_map in
        Some ((held, id_map), access_path) )
  | Lfield (exp', field_name, typ') ->
      path_of_exp astate (exp', typ')
      |> Option.map ~f:(fun (astate, access_path') ->
             (astate, AccessPath.append access_path' [FieldAccess field_name]) )
  | Cast (dest_typ, exp') ->
      path_of_exp astate (exp', dest_typ)
  | Lindex _ ->
      (* array access paths are not well supported (eg not constant-index access) *)
      None
  | UnOp _ | BinOp _ | Exn _ | Closure _ | Const _ | Sizeof _ ->
      (* doesn't look like an object, disregard *)
      None


let acquire_resource exp_typ astate =
  match path_of_exp astate exp_typ with
  | None ->
      astate
  | Some ((held, id_map), access_path) ->
      let old_count = find_count access_path held in
      (ResourcesHeld.add access_path (incr_count old_count) held, id_map)


let release_resource exp_typ astate =
  match path_of_exp astate exp_typ with
  | None ->
      astate
  | Some ((held, id_map), access_path) ->
      let old_count = find_count access_path held in
      (ResourcesHeld.add access_path (decr_count old_count) held, id_map)


let load (id, typ) exp astate =
  match path_of_exp astate (exp, typ) with
  | None ->
      astate
  | Some ((held, id_map), exp_access_path) ->
      (held, IdToAccessPath.add id exp_access_path id_map)


let store ~lhs:lhs_exp ~rhs:rhs_exp_typ astate =
  match path_of_exp astate (lhs_exp, snd rhs_exp_typ) with
  | None ->
      astate
  | Some ((held, id_map), lhs_access_path) -> (
      (* First forget the resources that [lhs_access_path] held. We would actually like to report if
         [lhs_access_path] did hold any resources because if so we are potentially leaking them
         here... However we cannot report resource leaks while we are still analyzing the method in
         the current setup so just forget about that case.


         To go further, we could either:

         - make the abstract state accumulate a list of leaks, or

         - hack it! forward the resources of [lhs_access_path] to a fake location (the problem might
         be to retrieve the original access path for reporting...) *)
      let held = ResourcesHeld.remove lhs_access_path held in
      let astate = (held, id_map) in
      match path_of_exp astate rhs_exp_typ with
      | None ->
          astate
      | Some ((held, id_map), rhs_access_path) ->
          (* Arbitrarily decide to keep the more recent access path only assuming that the rest of
             the program will prefer using the latest access path representing the value too. *)
          let held =
            match ResourcesHeld.find_opt rhs_access_path held with
            | None ->
                held
            | Some resources ->
                ResourcesHeld.remove rhs_access_path held
                |> ResourcesHeld.add lhs_access_path resources
          in
          (held, id_map) )


let has_leak formal_map (held, _) =
  (* test if we acquired resources that we do not return to the caller *)
  let is_local_leak access_path count =
    let base, _ = access_path in
    match (count, base) with
    | Top, _ ->
        false
    | NonTop count, _ when count > 1 ->
        true
    | NonTop count, _ when count <= 0 ->
        false
    (* count = 1 *)
    | _, (var, _) when Var.is_global var ->
        false
    | _, (ret, _) when Var.is_return ret ->
        false
    | _, base when FormalMap.is_formal base formal_map ->
        false
    | _ ->
        true
  in
  ResourcesHeld.exists is_local_leak held


module Summary = struct
  module InterfaceAccessPath = struct
    type base = Return | Formal of int [@@deriving compare]

    let pp_base f = function
      | Return ->
          F.pp_print_string f "Return"
      | Formal i ->
          F.fprintf f "Formal(%d)" i


    type t = base * AccessPath.access list [@@deriving compare]

    let pp f = function
      | base, [] ->
          pp_base f base
      | base, accesses ->
          F.fprintf f "%a.%a" pp_base base AccessPath.pp_access_list accesses
  end

  module ResourcesFromFormals = PrettyPrintable.MakePPMap (InterfaceAccessPath)

  type t = BoundsWithTop.t ResourcesFromFormals.t

  let pp = ResourcesFromFormals.pp ~pp_value:BoundsWithTop.pp

  let make formal_map (held, _) =
    let to_interface access_path =
      let base, accesses = access_path in
      match FormalMap.get_formal_index base formal_map with
      | Some i ->
          Some (InterfaceAccessPath.Formal i, accesses)
      | None ->
          if Var.is_return (fst base) then Some (InterfaceAccessPath.Return, accesses) else None
    in
    ResourcesHeld.fold
      (fun access_path count acquired ->
        match to_interface access_path with
        | Some interface_access_path ->
            ResourcesFromFormals.add interface_access_path count acquired
        | None ->
            acquired )
      held ResourcesFromFormals.empty


  let apply ~callee:summary ~return ~actuals astate =
    let return = (Var.of_id (fst return), snd return) in
    let (held, id_map), rev_actuals =
      List.fold actuals ~init:(astate, []) ~f:(fun (astate, rev_actuals_paths) actual_exp_typ ->
          match path_of_exp astate actual_exp_typ with
          | None ->
              (astate, None :: rev_actuals_paths)
          | Some (astate, actual_path) ->
              (astate, Some actual_path :: rev_actuals_paths) )
    in
    let actuals = List.rev rev_actuals in
    let apply_one (base, accesses) callee_count held =
      let access_path_opt =
        match (base : InterfaceAccessPath.base) with
        | Return ->
            Some (return, accesses)
        | Formal i -> (
          match List.nth actuals i with
          | Some (Some actual_path) ->
              Some (AccessPath.append actual_path accesses)
          | Some None | None ->
              None )
      in
      match access_path_opt with
      | None ->
          held
      | Some access_path ->
          let new_count =
            match callee_count with
            | Top ->
                Top
            | NonTop callee_count ->
                let old_count =
                  ResourcesHeld.find_opt access_path held |> Option.value ~default:(NonTop 0)
                in
                update_count old_count callee_count
          in
          ResourcesHeld.add access_path new_count held
    in
    (ResourcesFromFormals.fold apply_one summary held, id_map)
end

type summary = Summary.t

include Domain
