(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
include List

let empty = []

let to_list = rev

let of_list = rev

let append l1 l2 = append l2 l1

let rev_partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] ->
        (fst, snd)
    | x :: t -> (
      match (f x : _ Either.t) with
      | First y ->
          loop t (y :: fst) snd
      | Second y ->
          loop t fst (y :: snd) )
  in
  loop t [] []
