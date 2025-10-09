(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! NS
module F = Format

let seq ?sep:(sep_text = " ") pp =
  let rec aux f = function
    | [] ->
        ()
    | [x] ->
        pp f x
    | x :: l ->
        let sep = sep_text in
        F.fprintf f "%a%s%a" pp x sep aux l
  in
  aux


let comma_seq pp f l = seq ~sep:"," pp f l

let semicolon_seq pp f l = seq ~sep:"; " pp f l

let option pp fmt = function
  | None ->
      F.pp_print_string fmt "None"
  | Some x ->
      F.fprintf fmt "%a" pp x


let array pp f array =
  let list = Array.to_list array in
  comma_seq pp f list


let iarray pp f arr =
  let arr = IArray.to_array arr in
  array pp f arr
