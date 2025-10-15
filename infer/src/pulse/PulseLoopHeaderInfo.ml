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

type loop_info = iteration_info list [@@deriving compare, equal]

type t = loop_info Procdesc.IdMap.t [@@deriving compare, equal]

type id = Procdesc.IdMap.key

let pp_iteration_info fmt (idx, {timestamp; path_stamp}) =
  F.fprintf fmt "@[<hv>#%d:{@[<v>t= %a@;path_stamp=%a@]}@]" idx Timestamp.pp timestamp
    Formula.pp_path_stamp path_stamp


let pp_loop_info fmt l =
  let n = List.length l in
  let l_with_idx = List.mapi l ~f:(fun i info -> (n - i - 1, info)) in
  F.fprintf fmt "{%a}" (Pp.semicolon_seq pp_iteration_info) l_with_idx


let pp fmt map = F.fprintf fmt "{@[<v>%a@]}" (Procdesc.IdMap.pp ~pp_value:pp_loop_info) map

let empty = Procdesc.IdMap.empty

let mem id map = Procdesc.IdMap.mem id map

let has_previous_iteration_same_path_stamp id map =
  match Procdesc.IdMap.find id map with
  | info_current :: q ->
      List.exists q ~f:(fun info ->
          Formula.equal_path_stamp info_current.path_stamp info.path_stamp )
  | _ ->
      false


let push_loop_info id timestamp formula map =
  let path_stamp = Formula.extract_path_stamp formula in
  let info = {timestamp; path_stamp} in
  let infos = Procdesc.IdMap.find_opt id map |> Option.value ~default:[] in
  Procdesc.IdMap.add id (info :: infos) map


let get_iteration_index id map =
  Procdesc.IdMap.find_opt id map |> Option.value_map ~default:0 ~f:List.length
