(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Qset - Set with (signed) rational multiplicity for each element *)

open Base

type ('elt, 'cmp) t = ('elt, Q.t, 'cmp) Map.t

module M (Elt : sig
  type t
  type comparator_witness
end) =
struct
  type nonrec t = (Elt.t, Elt.comparator_witness) t
end

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

module type M_of_sexp = sig
  type t [@@deriving of_sexp]

  include Comparator.S with type t := t
end

module type Compare_m = sig end
module type Hash_fold_m = Hasher.S

let sexp_of_q q = Sexp.Atom (Q.to_string q)
let q_of_sexp = function Sexp.Atom s -> Q.of_string s | _ -> assert false
let hash_fold_q state q = Hash.fold_int state (Hashtbl.hash q)

let sexp_of_m__t (type elt) (module Elt : Sexp_of_m with type t = elt) t =
  Map.sexp_of_m__t (module Elt) sexp_of_q t

let m__t_of_sexp (type elt cmp)
    (module Elt : M_of_sexp
      with type t = elt
       and type comparator_witness = cmp) sexp =
  Map.m__t_of_sexp (module Elt) q_of_sexp sexp

let compare_m__t (module Elt : Compare_m) = Map.compare_direct Q.compare
let equal_m__t (module Elt : Compare_m) = Map.equal Q.equal

let hash_fold_m__t (type elt) (module Elt : Hash_fold_m with type t = elt)
    state =
  Map.hash_fold_m__t (module Elt) hash_fold_q state

let hash_m__t (type elt) (module Elt : Hash_fold_m with type t = elt) =
  Hash.of_fold (hash_fold_m__t (module Elt))

type ('elt, 'cmp) comparator =
  (module Comparator.S with type t = 'elt and type comparator_witness = 'cmp)

let empty cmp = Map.empty cmp
let if_nz q = if Q.equal Q.zero q then None else Some q

let add m x i =
  Map.change m x ~f:(function Some j -> if_nz Q.(i + j) | None -> if_nz i)

let remove m x = Map.remove m x

let union m n =
  Map.merge m n ~f:(fun ~key:_ -> function
    | `Both (i, j) -> if_nz Q.(i + j) | `Left i | `Right i -> Some i )

let length m = Map.length m
let count m x = match Map.find m x with Some q -> q | None -> Q.zero

let count_and_remove m x =
  let found = ref Q.zero in
  let m =
    Map.change m x ~f:(function
      | None -> None
      | Some i ->
          found := i ;
          None )
  in
  if Q.equal !found Q.zero then None else Some (!found, m)

let min_elt = Map.min_elt
let min_elt_exn = Map.min_elt_exn
let fold m ~f ~init = Map.fold m ~f:(fun ~key ~data s -> f key data s) ~init

let map m ~f =
  let m' = Map.empty (Map.comparator_s m) in
  let m, m' =
    fold m ~init:(m, m') ~f:(fun x i (m, m') ->
        let x', i' = f x i in
        if phys_equal x' x then
          if Q.equal i' i then (m, m') else (Map.set m ~key:x ~data:i', m')
        else (Map.remove m x, add m' x' i') )
  in
  fold m' ~init:m ~f:(fun x i m -> add m x i)

let fold_map m ~f ~init:s =
  let m' = Map.empty (Map.comparator_s m) in
  let m, m', s =
    fold m ~init:(m, m', s) ~f:(fun x i (m, m', s) ->
        let x', i', s = f x i s in
        if phys_equal x' x then
          if Q.equal i' i then (m, m', s)
          else (Map.set m ~key:x ~data:i', m', s)
        else (Map.remove m x, add m' x' i', s) )
  in
  (fold m' ~init:m ~f:(fun x i m -> add m x i), s)

let for_all m ~f = Map.for_alli m ~f:(fun ~key ~data -> f key data)
let map_counts m ~f = Map.mapi m ~f:(fun ~key ~data -> f key data)
let iter m ~f = Map.iteri m ~f:(fun ~key ~data -> f key data)
let to_list m = Map.to_alist m
