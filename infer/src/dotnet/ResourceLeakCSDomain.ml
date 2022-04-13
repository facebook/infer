(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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

module TypeNameInfo = AbstractDomain.Flat (Typ.Name)
module ResourceInfo = AbstractDomain.Pair (BoundsWithTop) (TypeNameInfo)
module ResourcesHeld = AbstractDomain.Map (AccessPath) (ResourceInfo)
open AbstractDomain.Types

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
  match ResourcesHeld.find_opt access_path held with Some (count, _) -> count | None -> NonTop 0


(** Adds resources acquired to records *)
let acquire_resource access_path type_name held =
  let old_count = find_count access_path held in
  ResourcesHeld.add access_path (incr_count old_count, TypeNameInfo.v type_name) held


(** Releases acquired resources from records when release function is called*)
let release_resource access_path held =
  match ResourcesHeld.find_opt access_path held with
  | Some (count, type_name) ->
      ResourcesHeld.add access_path (decr_count count, type_name) held
  | None ->
      ResourcesHeld.add access_path (Top, TypeNameInfo.top) held


(** Re-assigns resources when transferred to other objects*)
let assign lhs_access_path rhs_access_path held =
  let equal_base (base1, _) (base2, _) = AccessPath.equal_base base1 base2 in
  let one_binding access_path (count, type_name) held =
    match
      AccessPath.replace_prefix ~prefix:rhs_access_path ~replace_with:access_path lhs_access_path
    with
    | Some base_access_path ->
        ResourcesHeld.add base_access_path (count, type_name) held
    | None ->
        if AccessPath.equal rhs_access_path access_path then
          ResourcesHeld.add lhs_access_path (count, type_name) held
        else if equal_base rhs_access_path access_path then
          match
            AccessPath.replace_prefix ~prefix:rhs_access_path ~replace_with:lhs_access_path
              access_path
          with
          | Some new_access_path ->
              ResourcesHeld.add new_access_path (count, type_name) held
          | None ->
              ResourcesHeld.add access_path (count, type_name) held
        else ResourcesHeld.add access_path (count, type_name) held
  in
  ResourcesHeld.fold one_binding held ResourcesHeld.empty


(** Checks if there is a resource leak*)
let has_leak formal_map held =
  (* test if we acquired resources that we do not return to the caller *)
  let is_local_leak access_path (count, _) =
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

  type t = (BoundsWithTop.t * TypeNameInfo.t) ResourcesFromFormals.t

  let pp = ResourcesFromFormals.pp ~pp_value:ResourceInfo.pp

  (** Return leaked resources and types as string*)
  let resource_and_type_to_str held debug =
    let check_count access_path held =
      let old_count =
        match ResourcesHeld.find_opt access_path held with
        | Some (count, _) ->
            count
        | None ->
            NonTop 0
      in
      match old_count with NonTop count when count > 0 -> true | _ -> false
    in
    let text = ref [] in
    let concat_text =
      ResourcesHeld.iter
        (fun x (_, type_name) ->
          if check_count x held && debug then
            let type_name_str = TypeNameInfo.get type_name in
            match type_name_str with
            | Some type_str ->
                text :=
                  F.asprintf "Leaked resource %a of type %a" ResourcesHeld.pp_key x
                    Typ.Name.pp_name_only type_str
                  :: !text
            | _ ->
                ()
          else if check_count x held then
            let type_name_str = TypeNameInfo.get type_name in
            let temp_var_regexp = Re.Str.regexp " n\\$[0-9]+ " in
            match type_name_str with
            | Some type_str ->
                text :=
                  Re.Str.global_replace temp_var_regexp " "
                    (F.asprintf "Leaked resource %a of type %a" ResourcesHeld.pp_key x
                       Typ.Name.pp_name_only type_str )
                  :: !text
            | _ ->
                () )
        held
    in
    concat_text ;
    String.concat ~sep:", " !text


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
      (fun access_path (count, type_name) acquired ->
        match to_interface access_path with
        | Some interface_access_path ->
            ResourcesFromFormals.add interface_access_path (count, type_name) acquired
        | None ->
            acquired )
      held ResourcesFromFormals.empty


  let apply ~callee:summary ~return ~actuals held =
    let apply_one (base, accesses) (callee_count, type_name) held =
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
                  match ResourcesHeld.find_opt access_path held with
                  | Some (count, _) ->
                      count
                  | None ->
                      NonTop 0
                in
                update_count old_count callee_count
          in
          ResourcesHeld.add access_path (new_count, type_name) held
    in
    ResourcesFromFormals.fold apply_one summary held
end

type summary = Summary.t

include ResourcesHeld
