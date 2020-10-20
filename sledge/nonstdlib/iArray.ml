(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** IArray - Immutable view of an array *)

open! NS0
include Core_kernel.Perms.Export

include (
  Core_kernel.Array.Permissioned :
    module type of Core_kernel.Array.Permissioned
      with type ('a, 'p) t := ('a, 'p) Core_kernel.Array.Permissioned.t )

type 'a t = ('a, immutable) Core_kernel.Array.Permissioned.t

let a2i (a : 'a array) : 'a t = Obj.magic a
let i2a (a : 'a t) : 'a array = Obj.magic a
let compare compare_elt = compare compare_elt compare_immutable

let hash_fold_t hash_fold_elt s (a : _ t) =
  Hash.Builtin.hash_fold_array_frozen hash_fold_elt s (i2a a)

let t_of_sexp elt_of_sexp = t_of_sexp elt_of_sexp immutable_of_sexp
let sexp_of_t sexp_of_elt = sexp_of_t sexp_of_elt sexp_of_immutable

module Import = struct
  type 'a iarray = 'a t [@@deriving compare, equal, hash, sexp]
end

let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
let empty = Obj.magic [||]
let of_ x = create ~len:1 x
let of_array = a2i

let contains_dup ~compare xs =
  let equal x y = compare x y = 0 in
  Option.is_some
    (find_consecutive_duplicate ~equal (sorted_copy ~compare xs))

let fold_map xs ~init ~f =
  let a, ys = Array.fold_map (i2a xs) ~init ~f in
  (a, a2i ys)

let fold_map_until xs ~init ~f ~finish =
  With_return.with_return (fun {return} ->
      finish
        (fold_map xs ~init ~f:(fun s x ->
             match f s x with Continue x -> x | Stop x -> return x )) )

let map_endo xs ~f = map_endo map xs ~f

let combine_adjacent ~f xs =
  let xs = i2a xs in
  let n = Array.length xs - 1 in
  let rec combine_adjacent_ j i xs =
    if i < n then (
      match f xs.(i - j) xs.(i + 1) with
      | None ->
          if j != 0 then xs.(i + 1 - j) <- xs.(i + 1) ;
          combine_adjacent_ j (i + 1) xs
      | Some x ->
          let xs = if j = 0 then Array.copy xs else xs in
          xs.(i - j) <- x ;
          combine_adjacent_ (j + 1) (i + 1) xs )
    else if j = 0 then xs
    else Array.sub xs ~pos:0 ~len:(n + 1 - j)
  in
  a2i (combine_adjacent_ 0 0 xs)
