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

include AbstractDomain.Map (Var) (CopySpec)

let bottom = empty

let yojson_of_t = [%yojson_of: _]

let mark_copy_as_modified ~is_modified var astate =
  match find_opt var astate with
  | Some (Copied {heap= copy_heap}) when is_modified copy_heap ->
      Logging.d_printfln_escaped "Copy modified!" ;
      add var Modified astate
  | _ ->
      astate


let get_copied astate =
  fold
    (fun var (copy_spec : CopySpec.t) acc ->
      match copy_spec with Modified -> acc | Copied {location} -> (var, location) :: acc )
    astate []
