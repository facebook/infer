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


let rev_concat_map rev ~f =
  let rec aux acc = function [] -> acc | hd :: tl -> aux (rev_append (f hd) acc) tl in
  aux [] rev


let rev_append2 = rev_append

let rec to_rev_seq rev =
  match rev with [] -> fun () -> Seq.Nil | hd :: tl -> fun () -> Seq.Cons (hd, to_rev_seq tl)
