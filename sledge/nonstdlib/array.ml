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

let of_ x = [|x|]
let of_iter = Iter.to_array

let of_list_rev = function
  | [] -> [||]
  | hd :: tl ->
      let len = 1 + List.length tl in
      let a = make len hd in
      let rec back_fill i = function
        | [] -> a
        | hd :: tl ->
            a.(i) <- hd ;
            back_fill (i - 1) tl
      in
      back_fill (len - 2) tl

let is_empty = function [||] -> true | _ -> false
let map xs ~f = map ~f xs
let mapi xs ~f = mapi ~f xs
let map_endo xs ~f = map_endo map xs ~f

let reduce_adjacent xs ~f =
  let n = length xs - 1 in
  let rec reduce_adjacent_ j i xs =
    if i < n then (
      match f xs.(i - j) xs.(i + 1) with
      | None ->
          if j != 0 then xs.(i + 1 - j) <- xs.(i + 1) ;
          reduce_adjacent_ j (i + 1) xs
      | Some x ->
          let xs = if j = 0 then copy xs else xs in
          xs.(i - j) <- x ;
          reduce_adjacent_ (j + 1) (i + 1) xs )
    else if j = 0 then xs
    else sub xs ~pos:0 ~len:(n + 1 - j)
  in
  reduce_adjacent_ 0 0 xs

let split xys =
  let n = length xys in
  if n = 0 then ([||], [||])
  else
    let x0, y0 = xys.(0) in
    let xs = make n x0 in
    let ys = make n y0 in
    for i = 1 to n - 1 do
      let xI, yI = xys.(i) in
      xs.(i) <- xI ;
      ys.(i) <- yI
    done ;
    (xs, ys)

let combine_exn xs ys =
  let len = length xs in
  if len <> length ys then invalid_arg "Array.combine_exn" ;
  init len ~f:(fun i -> (xs.(i), ys.(i)))

let combine xs ys =
  try Some (combine_exn xs ys) with Invalid_argument _ -> None

let mem x xs ~eq = mem ~eq x xs
let iter xs ~f = iter ~f xs
let iteri xs ~f = iteri ~f xs
let exists xs ~f = exists ~f xs
let for_all xs ~f = for_all ~f xs
let fold xs init ~f = fold ~f:(fun s x -> f x s) ~init xs
let fold_right xs init ~f = fold_right ~f ~init xs

let fold_map xs init ~f =
  Pair.swap (fold_map ~f:(fun s x -> Pair.swap (f x s)) ~init xs)

let fold_map_until xs s ~f ~finish =
  let l = length xs in
  if l = 0 then finish ([||], s)
  else
    match f xs.(0) s with
    | `Stop r -> r
    | `Continue (y, s) ->
        let ys = make l y in
        let rec fold_map_until_ s i =
          if i = l then finish (ys, s)
          else
            match f xs.(i) s with
            | `Stop r -> r
            | `Continue (y, s) ->
                ys.(i) <- y ;
                fold_map_until_ s (i + 1)
        in
        fold_map_until_ s 1

let for_all2_exn xs ys ~f = for_all2 ~f xs ys
let fold2_exn xs ys init ~f = fold2 ~f:(fun s x y -> f x y s) ~init xs ys
let to_list_rev_map xs ~f = fold ~f:(fun x ys -> f x :: ys) xs []
let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
