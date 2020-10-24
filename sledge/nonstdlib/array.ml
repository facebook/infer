(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0
include Base.Array

let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
let map_endo xs ~f = map_endo map xs ~f

let fold_map_inplace a ~init ~f =
  let s = ref init in
  let f x =
    let s', x' = f !s x in
    s := s' ;
    x'
  in
  map_inplace a ~f ;
  !s

let to_list_rev_map xs ~f = fold ~f:(fun ys x -> f x :: ys) ~init:[] xs
