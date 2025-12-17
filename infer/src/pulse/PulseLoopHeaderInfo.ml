(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type iteration_info = {timestamp: Timestamp.t; path_stamp: Formula.path_stamp}
[@@deriving compare, equal]

type loop_info = {local_path_condition: Formula.t; stack: iteration_info list}
[@@deriving compare, equal]

type t = loop_info Procdesc.IdMap.t [@@deriving compare, equal]

type id = Procdesc.IdMap.key

let pp_iteration_info fmt (idx, {timestamp; path_stamp}) =
  F.fprintf fmt "@[<hv>#%d:{@[<v>t= %a@;path_stamp=%a@]}@]" idx Timestamp.pp timestamp
    Formula.pp_path_stamp path_stamp


let pp_loop_info fmt {stack} =
  let n = List.length stack in
  let l_with_idx = List.mapi stack ~f:(fun i info -> (n - i - 1, info)) in
  F.fprintf fmt "{%a}" (Pp.semicolon_seq pp_iteration_info) l_with_idx


let pp fmt map = F.fprintf fmt "{@[<v>%a@]}" (Procdesc.IdMap.pp ~pp_value:pp_loop_info) map

let empty = Procdesc.IdMap.empty

let mem id map = Procdesc.IdMap.mem id map

let get id map =
  Procdesc.IdMap.find_opt id map
  |> Option.value ~default:{local_path_condition= Formula.ttrue; stack= []}


let has_previous_iteration_same_path_stamp id map =
  match (get id map).stack with
  | info_current :: q ->
      List.exists q ~f:(fun info ->
          Formula.equal_path_stamp info_current.path_stamp info.path_stamp )
  | _ ->
      false


let is_current_iteration_empty_path_stamp id map =
  match (get id map).stack with
  | {path_stamp} :: _ :: _ ->
      (* this is at minima the 2nd time we loop into this node and the path stamp is empty *)
      Formula.is_empty_path_stamp path_stamp
  | _ ->
      false


let map_formulas map ~f =
  Procdesc.IdMap.map
    (fun ({local_path_condition} as info) ->
      let new_condition = f local_path_condition in
      if phys_equal new_condition local_path_condition then info
      else {info with local_path_condition= new_condition} )
    map


let push_loop_info id timestamp map =
  let {local_path_condition; stack} = get id map in
  let path_stamp = Formula.extract_path_stamp local_path_condition in
  let info = {timestamp; path_stamp} in
  let stack = info :: stack in
  Procdesc.IdMap.add id {local_path_condition; stack} map


let init_loop_info id map =
  Procdesc.IdMap.add id {local_path_condition= Formula.ttrue; stack= []} map


let remove_loop_info id map = Procdesc.IdMap.remove id map

let get_iteration_index id map = (get id map).stack |> List.length
