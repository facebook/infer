(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type iteration_info = {timestamp: Timestamp.t} [@@deriving compare, equal]

type loop_info = iteration_info list [@@deriving compare, equal]

type t = loop_info Procdesc.IdMap.t [@@deriving compare, equal]

type id = Procdesc.IdMap.key

let pp_iteration_info fmt (idx, {timestamp}) =
  F.fprintf fmt "#%d:{t= %a}" idx Timestamp.pp timestamp


let pp_loop_info fmt l =
  let n = List.length l in
  let l_with_idx = List.mapi l ~f:(fun i info -> (n - i - 1, info)) in
  F.fprintf fmt "{%a}" (Pp.semicolon_seq pp_iteration_info) l_with_idx


let pp fmt map =
  let bindings = Procdesc.IdMap.bindings map in
  let pp fmt (id, info) = F.fprintf fmt "node%a:%a" Procdesc.Node.pp_id id pp_loop_info info in
  F.fprintf fmt "{@[<v>%a@]}" (Pp.semicolon_seq pp) bindings


let empty = Procdesc.IdMap.empty

let mem id map = Procdesc.IdMap.mem id map

let get_loop_info = Procdesc.IdMap.find

let push_loop_info id info map =
  let infos = Procdesc.IdMap.find_opt id map |> Option.value ~default:[] in
  Procdesc.IdMap.add id (info :: infos) map
