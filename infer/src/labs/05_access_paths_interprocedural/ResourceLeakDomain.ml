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
open AbstractDomain.Types

let initial = ResourcesHeld.empty

let update_count count n = match count with Top -> Top | NonTop held -> NonTop (held + n)

let incr_count count = update_count count 1

let decr_count count = update_count count (-1)

let find_count access_path held =
  match ResourcesHeld.find_opt access_path held with Some count -> count | None -> NonTop 0


let acquire_resource access_path held =
  let old_count = find_count access_path held in
  ResourcesHeld.add access_path (incr_count old_count) held


let release_resource access_path held =
  let old_count = find_count access_path held in
  ResourcesHeld.add access_path (decr_count old_count) held


let assign lhs_access_path rhs_access_path held =
  let one_binding access_path count held =
    match AccessPath.replace_prefix ~prefix:rhs_access_path lhs_access_path access_path with
    | Some base_access_path ->
        ResourcesHeld.add base_access_path count held
    | None ->
        ResourcesHeld.add access_path count held
  in
  ResourcesHeld.fold one_binding held ResourcesHeld.empty


let has_leak formal_map held =
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

  let make formal_map held =
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


  let apply ~summary ~return ~actuals held =
    let apply_one (base, accesses) callee_count held =
      let access_path_opt =
        match (base : InterfaceAccessPath.base) with
        | Return ->
            Some (return, accesses)
        | Formal i -> (
          match List.nth actuals i with
          | Some (HilExp.AccessExpression actual_expr) ->
              Some (AccessPath.append (HilExp.AccessExpression.to_access_path actual_expr) accesses)
          | _ ->
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
    ResourcesFromFormals.fold apply_one summary held
end

type summary = Summary.t

include ResourcesHeld
