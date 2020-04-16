(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Qset - Set with (signed) rational multiplicity for each element *)

open Import0
include Qset_intf

module Make (Elt : sig
  type t [@@deriving compare, sexp_of]
end) =
struct
  module M = Map.Make (Elt)

  type elt = Elt.t
  type t = Q.t M.t

  let compare = M.compare Q.compare
  let equal = M.equal Q.equal

  let hash_fold_t hash_fold_elt s m =
    let hash_fold_q s q = Hash.fold_int s (Hashtbl.hash q) in
    M.fold m
      ~init:(Hash.fold_int s (M.length m))
      ~f:(fun ~key ~data state -> hash_fold_q (hash_fold_elt state key) data)

  let sexp_of_t s =
    let sexp_of_q q = Sexp.Atom (Q.to_string q) in
    List.sexp_of_t
      (Sexplib.Conv.sexp_of_pair Elt.sexp_of_t sexp_of_q)
      (M.to_alist s)

  let t_of_sexp elt_of_sexp sexp =
    let q_of_sexp = function
      | Sexp.Atom s -> Q.of_string s
      | _ -> assert false
    in
    List.fold_left
      ~f:(fun m (key, data) -> M.add_exn m ~key ~data)
      ~init:M.empty
      (List.t_of_sexp
         (Sexplib.Conv.pair_of_sexp elt_of_sexp q_of_sexp)
         sexp)

  let pp sep pp_elt fs s = List.pp sep pp_elt fs (M.to_alist s)
  let empty = M.empty
  let of_ = M.singleton
  let if_nz q = if Q.equal Q.zero q then None else Some q

  let add m x i =
    M.change m x ~f:(function Some j -> if_nz Q.(i + j) | None -> if_nz i)

  let remove m x = M.remove m x
  let find_and_remove = M.find_and_remove

  let union m n =
    M.merge m n ~f:(fun ~key:_ -> function
      | `Both (i, j) -> if_nz Q.(i + j) | `Left i | `Right i -> Some i )

  let map m ~f =
    let m' = empty in
    let m, m' =
      M.fold m ~init:(m, m') ~f:(fun ~key:x ~data:i (m, m') ->
          let x', i' = f x i in
          if x' == x then
            if Q.equal i' i then (m, m') else (M.set m ~key:x ~data:i', m')
          else (M.remove m x, add m' x' i') )
    in
    M.fold m' ~init:m ~f:(fun ~key:x ~data:i m -> add m x i)

  let map_counts m ~f = M.mapi ~f:(fun ~key ~data -> f key data) m
  let is_empty = M.is_empty
  let length m = M.length m
  let count m x = match M.find m x with Some q -> q | None -> Q.zero
  let choose = M.choose
  let pop = M.pop
  let min_elt = M.min_elt
  let pop_min_elt = M.pop_min_elt

  let classify s =
    match pop s with
    | None -> `Zero
    | Some (elt, q, s') when is_empty s' -> `One (elt, q)
    | _ -> `Many

  let to_list m = M.to_alist m
  let iter m ~f = M.iteri ~f:(fun ~key ~data -> f key data) m
  let exists m ~f = M.existsi ~f:(fun ~key ~data -> f key data) m
  let for_all m ~f = M.for_alli ~f:(fun ~key ~data -> f key data) m
  let fold m ~f ~init = M.fold ~f:(fun ~key ~data -> f key data) m ~init
end
