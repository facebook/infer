(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BaseMemory = PulseBaseMemory

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

module CopyMap = AbstractDomain.Map (Var) (CopySpec)
module CopyVar = AbstractDomain.Flat (Var)
module SourceMap = AbstractDomain.Map (PulseAbstractValue) (CopyVar)
include AbstractDomain.Pair (CopyMap) (SourceMap)

let bottom = (CopyMap.empty, SourceMap.empty)

let is_bottom (astate_copy, astate_source) =
  CopyMap.is_bottom astate_copy && SourceMap.is_bottom astate_source


let mark_copy_as_modified ~is_modified var ((astate_copy, astate_source) as astate) =
  match CopyMap.find_opt var astate_copy with
  | Some (Copied {heap= copy_heap}) when is_modified copy_heap ->
      Logging.d_printfln_escaped "Copy modified!" ;
      (CopyMap.add var Modified astate_copy, astate_source)
  | _ ->
      astate


let get_copied (astate, _) =
  CopyMap.fold
    (fun var (copy_spec : CopySpec.t) acc ->
      match copy_spec with Modified -> acc | Copied {location} -> (var, location) :: acc )
    astate []


let add ~source_opt copy_var res (astate_copy, astate_source) =
  ( CopyMap.add copy_var res astate_copy
  , Option.value_map source_opt ~default:astate_source ~f:(fun source ->
        SourceMap.add source (CopyVar.v copy_var) astate_source ) )


let find_copy ~source (_, astate_source) =
  let open IOption.Let_syntax in
  let* copy_val = SourceMap.find_opt source astate_source in
  let+ copy_var = CopyVar.get copy_val in
  copy_var
