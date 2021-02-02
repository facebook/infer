(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include ContainersLabels.List

type 'a t = 'a list [@@deriving compare, equal, hash, sexp]

let hd_exn = hd
let hd = function [] -> None | hd :: _ -> Some hd
let tl_exn = tl
let tl = function [] -> None | _ :: tl -> Some tl
let pop_exn = function x :: xs -> (x, xs) | [] -> raise Not_found
let mem elt seq ~eq = mem ~eq elt seq
let iter xs ~f = iter ~f xs
let exists xs ~f = exists ~f xs
let for_all xs ~f = for_all ~f xs
let find_exn xs ~f = find ~f xs
let find xs ~f = find_opt ~f xs
let find_map xs ~f = find_map ~f xs
let find_map_exn xs ~f = Option.get_exn (find_map xs ~f)

let remove_one_exn ~eq x xs =
  let rec remove_ ys = function
    | [] -> raise Not_found
    | z :: xs -> if eq x z then rev_append ys xs else remove_ (z :: ys) xs
  in
  remove_ [] xs

let remove_one ~eq x xs =
  try Some (remove_one_exn ~eq x xs) with Not_found -> None

let remove ~eq x xs = remove ~eq ~key:x xs
let filter xs ~f = filter ~f xs
let partition xs ~f = partition ~f xs
let map xs ~f = map ~f xs
let map_endo t ~f = map_endo map t ~f

let rev_map_split xs ~f =
  fold_left xs ~init:([], []) ~f:(fun (ys, zs) x ->
      let y, z = f x in
      (y :: ys, z :: zs) )

let combine_exn = combine

let combine xs ys =
  try Some (combine_exn xs ys) with Invalid_argument _ -> None

let group_by seq ~hash ~eq = group_by ~hash ~eq seq
let join_by ~eq ~hash k1 k2 ~merge = join_by ~eq ~hash k1 k2 ~merge
let join_all_by ~eq ~hash k1 k2 ~merge = join_all_by ~eq ~hash k1 k2 ~merge
let group_join_by ~eq ~hash = group_join_by ~eq ~hash
let fold xs init ~f = fold_left ~f:(fun s x -> f x s) ~init xs
let fold_left xs init ~f = fold_left ~f ~init xs
let fold_right xs init ~f = fold_right ~f ~init xs

let reduce xs ~f =
  match xs with [] -> None | x :: xs -> Some (fold ~f xs x)

let fold_map xs init ~f =
  Pair.swap (fold_map ~f:(fun s x -> Pair.swap (f x s)) ~init xs)

let fold2_exn xs ys init ~f =
  fold_left2 ~f:(fun s x y -> f x y s) ~init xs ys

let group_succ ~eq xs = group_succ ~eq:(fun y x -> eq x y) xs

let symmetric_diff ~cmp xs ys =
  let rec symmetric_diff_ xxs yys : _ Either.t list =
    match (xxs, yys) with
    | x :: xs, y :: ys ->
        let ord = cmp x y in
        if ord = 0 then symmetric_diff_ xs ys
        else if ord < 0 then Left x :: symmetric_diff_ xs yys
        else Right y :: symmetric_diff_ xxs ys
    | xs, [] -> map ~f:Either.left xs
    | [], ys -> map ~f:Either.right ys
  in
  symmetric_diff_ (sort ~cmp xs) (sort ~cmp ys)

let rec pp ?pre ?suf sep pp_elt fs = function
  | [] -> ()
  | x :: xs ->
      Option.iter ~f:(Format.fprintf fs) pre ;
      pp_elt fs x ;
      ( match xs with
      | [] -> ()
      | xs -> Format.fprintf fs "%( %)%a" sep (pp sep pp_elt) xs ) ;
      Option.iter ~f:(Format.fprintf fs) suf

let pp_diff ~cmp ?pre ?suf sep pp_elt fs (xs, ys) =
  let pp_diff_elt fs (elt : _ Either.t) =
    match elt with
    | Left x -> Format.fprintf fs "-- %a" pp_elt x
    | Right y -> Format.fprintf fs "++ %a" pp_elt y
  in
  pp ?pre ?suf sep pp_diff_elt fs (symmetric_diff ~cmp xs ys)

module Assoc = struct
  include Assoc

  let mem elt seq ~eq = mem ~eq elt seq
end

let mem_assoc elt seq ~eq = mem_assoc ~eq elt seq
