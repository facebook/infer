(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
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

module Captured = AbstractDomain.FiniteSet (struct
  include Pvar

  let pp = Pvar.pp Pp.text
end)

module CopyMap = AbstractDomain.Map (CopyVar) (CopySpec)

type t = {copy_map: CopyMap.t; destructor_checked: DestructorChecked.t; captured: Captured.t}

let pp f {copy_map; destructor_checked; captured} =
  F.fprintf f "@[@[copy map: %a@],@ @[destructor checked: %a@],@ @[captured: %a@]@]" CopyMap.pp
    copy_map DestructorChecked.pp destructor_checked Captured.pp captured


let leq ~lhs ~rhs =
  CopyMap.leq ~lhs:lhs.copy_map ~rhs:rhs.copy_map
  && DestructorChecked.leq ~lhs:lhs.destructor_checked ~rhs:rhs.destructor_checked
  && Captured.leq ~lhs:lhs.captured ~rhs:rhs.captured


let join x y =
  { copy_map= CopyMap.join x.copy_map y.copy_map
  ; destructor_checked= DestructorChecked.join x.destructor_checked y.destructor_checked
  ; captured= Captured.join x.captured y.captured }


let widen ~prev ~next ~num_iters =
  { copy_map= CopyMap.widen ~prev:prev.copy_map ~next:next.copy_map ~num_iters
  ; destructor_checked=
      DestructorChecked.widen ~prev:prev.destructor_checked ~next:next.destructor_checked ~num_iters
  ; captured= Captured.widen ~prev:prev.captured ~next:next.captured ~num_iters }


let bottom =
  {copy_map= CopyMap.empty; destructor_checked= DestructorChecked.empty; captured= Captured.empty}


let is_bottom {copy_map; destructor_checked; captured} =
  CopyMap.is_bottom copy_map
  && DestructorChecked.is_bottom destructor_checked
  && Captured.is_bottom captured


let mark_copy_as_modified ~is_modified ~copied_var ~source_addr_opt ({copy_map} as astate) =
  let copy_var = CopyVar.{copied_var; source_addr_opt} in
  let copy_map =
    match CopyMap.find_opt copy_var copy_map with
    | Some (Copied {heap= copy_heap}) when is_modified copy_heap ->
        Logging.d_printfln_escaped "Copy/source modified!" ;
        CopyMap.add copy_var Modified copy_map
    | _ ->
        copy_map
  in
  {astate with copy_map}


let checked_via_dtor var astate =
  {astate with destructor_checked= DestructorChecked.add var astate.destructor_checked}


let get_copied {copy_map; captured} =
  let modified =
    CopyMap.fold
      (fun CopyVar.{copied_var} (copy_spec : CopySpec.t) acc ->
        match copy_spec with Modified -> Var.Set.add copied_var acc | Copied _ -> acc )
      copy_map Var.Set.empty
  in
  let is_captured var =
    match (var : Var.t) with ProgramVar pvar -> Captured.mem pvar captured | LogicalVar _ -> false
  in
  CopyMap.fold
    (fun CopyVar.{copied_var} (copy_spec : CopySpec.t) acc ->
      match copy_spec with
      | Modified ->
          acc
      | Copied {location} ->
          if Var.Set.mem copied_var modified || is_captured copied_var then acc
          else (copied_var, location) :: acc )
    copy_map []


let add copied_var ~source_addr_opt (res : copy_spec_t) astate =
  {astate with copy_map= CopyMap.add {copied_var; source_addr_opt} res astate.copy_map}


let is_checked_via_dtor var {destructor_checked} = DestructorChecked.mem var destructor_checked

let set_captured_variables exp astate =
  match exp with
  | Exp.Closure {captured_vars} ->
      List.fold captured_vars ~init:astate ~f:(fun astate (_, pvar, _, _) ->
          {astate with captured= Captured.add pvar astate.captured} )
  | _ ->
      astate
