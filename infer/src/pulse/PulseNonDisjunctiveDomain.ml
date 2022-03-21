(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BaseMemory = PulseBaseMemory

(** Unnecessary copies are tracked in two places:

    - In non-disjunctive domain (here), we keep track of a map from (copy var, source address
      (optional)) -> Modified/ Copied heap snapshot
    - In attributes ({!PulseBaseAddressAttributes}) of each disjunct where we keep track of
    - source address -> copy var ({!CopiedVar})
    - copy address -> source address ({!SourceOriginOfCopy})

    In order to determine if a source/copy variable is modified,

    - When a source variable goes out of scope, we first lookup the CopiedVar from the attributes of
      source address and then lookup the snapshot from the non-disjunctive domain
    - When a copy variable goes out of scope, we first lookup the corresponding source address which
      we can then use to loopkup the snapshot from the non-disjunctive domain

    Then we compare the snapshot heap with the current heap (see {!PulseNonDisjunctiveOperations}.) *)

type copy_spec_t = Copied of {location: Location.t; heap: BaseMemory.t} | Modified
[@@deriving equal]

module CopySpec = struct
  type t = copy_spec_t [@@deriving equal]

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Copied _, _ ->
        true
    | Modified, Copied _ ->
        false
    | Modified, Modified ->
        true


  let join x y = if leq ~lhs:x ~rhs:y then y else x

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Copied {heap; location} ->
        Format.fprintf fmt " copied at %a with heap= %a" Location.pp location BaseMemory.pp heap
    | Modified ->
        Format.fprintf fmt "modified"
end

module CopyVar = struct
  type t = {copied_var: Var.t; source_addr_opt: PulseAbstractValue.t option} [@@deriving compare]

  let pp fmt {copied_var; source_addr_opt} =
    match source_addr_opt with
    | Some source_addr ->
        Format.fprintf fmt "%a copied with source_addr %a" Var.pp copied_var PulseAbstractValue.pp
          source_addr
    | None ->
        Format.fprintf fmt "%a copied" Var.pp copied_var
end

module DestructorChecked = AbstractDomain.FiniteSet (Var)
module CopyMap = AbstractDomain.Map (CopyVar) (CopySpec)
include AbstractDomain.Pair (CopyMap) (DestructorChecked)

let bottom = (CopyMap.empty, DestructorChecked.empty)

let is_bottom (astate_copy, astate_d) =
  CopyMap.is_bottom astate_copy && DestructorChecked.is_bottom astate_d


let mark_copy_as_modified ~is_modified ~copied_var ~source_addr_opt (astate_copy, astate_d) =
  let copy_var = CopyVar.{copied_var; source_addr_opt} in
  let astate_copy =
    match CopyMap.find_opt copy_var astate_copy with
    | Some (Copied {heap= copy_heap}) when is_modified copy_heap ->
        Logging.d_printfln_escaped "Copy/source modified!" ;
        CopyMap.add copy_var Modified astate_copy
    | _ ->
        astate_copy
  in
  (astate_copy, astate_d)


let checked_via_dtor var (astate_copy, astate_d) = (astate_copy, DestructorChecked.add var astate_d)

let get_copied (astate_copy, _astate_d) =
  let modified =
    CopyMap.fold
      (fun CopyVar.{copied_var} (copy_spec : CopySpec.t) acc ->
        match copy_spec with Modified -> Var.Set.add copied_var acc | Copied _ -> acc )
      astate_copy Var.Set.empty
  in
  CopyMap.fold
    (fun CopyVar.{copied_var} (copy_spec : CopySpec.t) acc ->
      match copy_spec with
      | Modified ->
          acc
      | Copied {location} ->
          if Var.Set.mem copied_var modified then acc else (copied_var, location) :: acc )
    astate_copy []


let add copied_var ~source_addr_opt (res : copy_spec_t) (astate_copy, astate_d) =
  (CopyMap.add {copied_var; source_addr_opt} res astate_copy, astate_d)


let is_checked_via_dtor var (_, astate_d) = DestructorChecked.mem var astate_d
