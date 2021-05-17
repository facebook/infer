(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hashtbl = Caml.Hashtbl

module LeakList = struct
  include Base.List

  let append_one leakList typeWithLeak = leakList @ [typeWithLeak]
end

(** Resource analysis in loop and branch*)
module FiniteBounds = struct
  type t = int

  let leq ~lhs ~rhs = lhs <= rhs

  let join a b = max a b

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.fprintf fmt "%d" astate
end

(** Resource analysis in loop *)
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

(** Initializes resources to type map *)
let type_map = ref (Hashtbl.create 100)

(** Initializes resources to count map *)
let initial = ResourcesHeld.empty

(** Updates the count of a specific resource *)
let update_count count n = match count with Top -> Top | NonTop held -> NonTop (held + n)

(** Increments the count of a specific resource *)
let incr_count count = update_count count 1

(** Decrements the count of a specific resource *)
let decr_count count = update_count count (-1)

(** Checks the count of a specific resource *)
let find_count access_path held =
  match ResourcesHeld.find_opt access_path held with Some count -> count | None -> NonTop 0


(** Checks the count of resources held if resource exists, otherwise returns false*)
let check_count access_path held =
  let old_count = find_count access_path held in
  match old_count with NonTop count when count > 0 -> true | _ -> false


let get_type_map = !type_map

let reset_type_map = Hashtbl.reset !type_map

(** Adds resources acquired to records *)
let acquire_resource access_path class_name held =
  let add_resource_to_hash =
    match ResourcesHeld.find_opt access_path held with
    | Some _ ->
        ()
    | None ->
        Hashtbl.add !type_map access_path class_name
  in
  add_resource_to_hash ;
  let old_count = find_count access_path held in
  ResourcesHeld.add access_path (incr_count old_count) held


(** Releases acquired resources from records when release function is called*)
let release_resource access_path held =
  let old_count = find_count access_path held in
  let remove_resource_from_hash =
    match old_count with
    | NonTop count when count < 2 ->
        Hashtbl.remove !type_map access_path
    | _ ->
        ()
  in
  remove_resource_from_hash ;
  ResourcesHeld.add access_path (decr_count old_count) held


(** Re-assigns resources when transferred to other objects*)
let assign lhs_access_path rhs_access_path held =
  let add_type_map search_access_path access_path =
    match Hashtbl.find !type_map search_access_path with
    | class_name ->
        Hashtbl.add !type_map access_path class_name ;
        Hashtbl.remove !type_map search_access_path
    | exception Caml.Not_found ->
        ()
  in
  let one_binding access_path count held =
    match
      AccessPath.replace_prefix ~prefix:rhs_access_path ~replace_with:access_path lhs_access_path
    with
    | Some base_access_path ->
        add_type_map access_path base_access_path ;
        ResourcesHeld.add base_access_path count held
    | None ->
        if AccessPath.equal rhs_access_path access_path then (
          add_type_map access_path lhs_access_path ;
          ResourcesHeld.add lhs_access_path count held )
        else ResourcesHeld.add access_path count held
  in
  ResourcesHeld.fold one_binding held ResourcesHeld.empty


(** Checks if there is a resource leak*)
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


(** module for resource leak summary *)
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

  let interface_type_map = ref (Hashtbl.create 100)

  let reset_interface_type_map = Hashtbl.reset !interface_type_map

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
    let add_resource_to_hash interface_access_path class_name =
      Hashtbl.add !interface_type_map interface_access_path class_name
    in
    let add_to_type_map search_access_path interface_access_path =
      match Hashtbl.find !type_map search_access_path with
      | class_name ->
          add_resource_to_hash interface_access_path class_name
      | exception Caml.Not_found ->
          ()
    in
    ResourcesHeld.fold
      (fun access_path count acquired ->
        match to_interface access_path with
        | Some interface_access_path ->
            add_to_type_map access_path interface_access_path ;
            ResourcesFromFormals.add interface_access_path count acquired
        | None ->
            acquired )
      held ResourcesFromFormals.empty


  let apply ~callee:summary ~return ~actuals held =
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
      let add_type_map search_access_path =
        match Hashtbl.find !interface_type_map (base, accesses) with
        | class_name ->
            Hashtbl.add !type_map search_access_path class_name
        | exception Caml.Not_found ->
            ()
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
          let add_resource_to_hash =
            match new_count with NonTop count when count > 0 -> add_type_map access_path | _ -> ()
          in
          add_resource_to_hash ;
          ResourcesHeld.add access_path new_count held
    in
    ResourcesFromFormals.fold apply_one summary held
end

type summary = Summary.t

include ResourcesHeld
