(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

(** Access path + its parent procedure *)
module LocalAccessPath = struct
  type t = {access_path: AccessPath.t; parent: Typ.Procname.t} [@@deriving compare]

  let equal = [%compare.equal : t]

  let make access_path parent = {access_path; parent}

  let is_rooted_in_footprint {access_path= (base_var, _), _} = Var.is_footprint base_var

  let to_formal_option {access_path= ((_, base_typ) as base), accesses; parent} formal_map =
    match FormalMap.get_formal_index base formal_map with
    | Some formal_index ->
        Some (make ((Var.of_formal_index formal_index, base_typ), accesses) parent)
    | None ->
        None


  let pp fmt t = AccessPath.pp fmt t.access_path
end

(** Called procedure + it's receiver *)
module MethodCall = struct
  type t = {receiver: LocalAccessPath.t; procname: Typ.Procname.t} [@@deriving compare]

  let pp fmt {receiver; procname} =
    F.fprintf fmt "%a.%a" LocalAccessPath.pp receiver Typ.Procname.pp procname
end

module CallSet = AbstractDomain.FiniteSet (MethodCall)
include AbstractDomain.Map (LocalAccessPath) (CallSet)

let substitute ~(f_sub: LocalAccessPath.t -> LocalAccessPath.t option) astate =
  fold
    (fun original_access_path call_set acc ->
      let access_path' =
        match f_sub original_access_path with
        | Some access_path ->
            access_path
        | None ->
            original_access_path
      in
      let call_set' =
        CallSet.fold
          (fun ({procname} as call) call_set_acc ->
            let receiver =
              match f_sub call.receiver with Some receiver' -> receiver' | None -> call.receiver
            in
            CallSet.add {receiver; procname} call_set_acc )
          call_set CallSet.empty
      in
      add access_path' call_set' acc )
    astate empty
