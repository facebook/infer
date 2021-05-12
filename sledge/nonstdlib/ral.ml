(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include CCRAL

let pp ?pre ?suf sep pp_elt fs ral =
  List.pp ?pre ?suf sep pp_elt fs (to_list ral)

let fold l s ~f = fold ~f:(fun s x -> f x s) ~x:s l
