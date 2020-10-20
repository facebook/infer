(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Multiset - Set with multiplicity for each element *)

open! NS0
include Multiset_intf

module Make
    (Mul : MULTIPLICITY) (Elt : sig
      type t [@@deriving compare, sexp_of]
    end) =
struct
  module M = Map.Make (Elt)

  type mul = Mul.t
  type elt = Elt.t
  type t = Mul.t M.t

  let compare = M.compare Mul.compare
  let equal = M.equal Mul.equal

  let hash_fold_t hash_fold_elt s m =
    let hash_fold_mul s i = Hash.fold_int s (Mul.hash i) in
    M.fold m
      ~init:(Hash.fold_int s (M.length m))
      ~f:(fun ~key ~data state ->
        hash_fold_mul (hash_fold_elt state key) data )

  let sexp_of_t s =
    List.sexp_of_t
      (Sexplib.Conv.sexp_of_pair Elt.sexp_of_t Mul.sexp_of_t)
      (M.to_alist s)

  let t_of_sexp elt_of_sexp sexp =
    List.fold_left
      ~f:(fun m (key, data) -> M.add_exn m ~key ~data)
      ~init:M.empty
      (List.t_of_sexp
         (Sexplib.Conv.pair_of_sexp elt_of_sexp Mul.t_of_sexp)
         sexp)

  let pp sep pp_elt fs s = List.pp sep pp_elt fs (M.to_alist s)
  let empty = M.empty
  let of_ x i = if Mul.equal Mul.zero i then empty else M.singleton x i
  let if_nz i = if Mul.equal Mul.zero i then None else Some i

  let add m x i =
    M.change m x ~f:(function
      | Some j -> if_nz (Mul.add i j)
      | None -> if_nz i )

  let remove m x = M.remove m x
  let find_and_remove = M.find_and_remove
  let union m n = M.union m n ~f:(fun _ i j -> if_nz (Mul.add i j))

  let diff m n =
    M.merge m n ~f:(fun ~key:_ -> function
      | `Both (i, j) -> if_nz (Mul.sub i j)
      | `Left i -> Some i
      | `Right j -> Some (Mul.neg j) )

  let partition = M.partition

  let map m ~f =
    let m' = empty in
    let m, m' =
      M.fold m ~init:(m, m') ~f:(fun ~key:x ~data:i (m, m') ->
          let x', i' = f x i in
          if x' == x then
            if Mul.equal i' i then (m, m') else (M.set m ~key:x ~data:i', m')
          else (M.remove m x, add m' x' i') )
    in
    union m m'

  let map_counts m ~f = M.map ~f m
  let mapi_counts m ~f = M.mapi ~f:(fun ~key ~data -> f key data) m

  let flat_map m ~f =
    let m' = empty in
    let m, m' =
      M.fold m ~init:(m, m') ~f:(fun ~key:x ~data:i (m, m') ->
          let d = f x i in
          match M.only_binding d with
          | Some (x', i') ->
              if x' == x then
                if Mul.equal i' i then (m, m')
                else (M.set m ~key:x ~data:i', m')
              else (M.remove m x, union m' d)
          | None -> (M.remove m x, union m' d) )
    in
    union m m'

  let is_empty = M.is_empty
  let is_singleton = M.is_singleton
  let length m = M.length m
  let count m x = match M.find m x with Some q -> q | None -> Mul.zero
  let only_elt = M.only_binding
  let classify = M.classify
  let choose = M.choose
  let choose_exn = M.choose_exn
  let pop = M.pop
  let min_elt = M.min_binding
  let pop_min_elt = M.pop_min_binding
  let to_list m = M.to_alist m
  let iter m ~f = M.iteri ~f:(fun ~key ~data -> f key data) m
  let exists m ~f = M.existsi ~f:(fun ~key ~data -> f key data) m
  let for_all m ~f = M.for_alli ~f:(fun ~key ~data -> f key data) m
  let fold m ~init ~f = M.fold ~f:(fun ~key ~data -> f key data) m ~init
end
