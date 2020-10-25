(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
module Array = ContainersLabels.Array
include Array

type 'a t = 'a array [@@deriving compare, equal, sexp]

let is_empty = function [||] -> true | _ -> false
let map xs ~f = map ~f xs
let map_endo xs ~f = map_endo map xs ~f

let combine_exn xs ys =
  let len = length xs in
  if len <> length ys then invalid_arg "Array.combine_exn" ;
  init len ~f:(fun i -> (xs.(i), ys.(i)))

let combine xs ys =
  try Some (combine_exn xs ys) with Invalid_argument _ -> None

let fold xs ~init ~f = fold ~f ~init xs
let to_list_rev_map xs ~f = fold ~f:(fun ys x -> f x :: ys) ~init:[] xs
let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
