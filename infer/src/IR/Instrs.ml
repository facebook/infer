(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = Sil.instr list

let empty = []

let single instr = [instr]

let append_list = List.append

let prepend_one instr instrs = instr :: instrs

let reverse_order = List.rev

let is_empty = List.is_empty

let exists = List.exists

let for_all = List.for_all

let count = List.length

let nth_exists instrs index = IList.drop instrs index |> List.is_empty |> not

let nth_exn = List.nth_exn

let last = List.last

let find_map = List.find_map

let pp pe fmt instrs =
  List.iter instrs ~f:(fun instr -> F.fprintf fmt "%a;@\n" (Sil.pp_instr pe) instr)


let filter_map = List.filter_map

let fold = List.fold

let iter = List.iter

let map_changed ~equal instrs ~f = IList.map_changed ~equal instrs ~f

let of_list instrs = instrs

let of_rev_list instrs = List.rev instrs
