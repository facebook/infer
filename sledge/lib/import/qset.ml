(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Qset - Set with (signed) rational multiplicity for each element *)

open Import0
include Qset_intf

module Make (Elt : OrderedType) = struct
  module M = Stdlib.Map.Make (Elt)

  type elt = Elt.t
  type t = Q.t M.t

  let compare = M.compare Q.compare
  let equal = M.equal Q.equal

  let hash_fold_t hash_fold_elt s m =
    let hash_fold_q s q = Hash.fold_int s (Hashtbl.hash q) in
    M.fold
      (fun key data state -> hash_fold_q (hash_fold_elt state key) data)
      m
      (Hash.fold_int s (M.cardinal m))

  let sexp_of_t s =
    let sexp_of_q q = Sexp.Atom (Q.to_string q) in
    List.sexp_of_t
      (Sexplib.Conv.sexp_of_pair Elt.sexp_of_t sexp_of_q)
      (M.bindings s)

  let t_of_sexp elt_of_sexp sexp =
    let q_of_sexp = function
      | Sexp.Atom s -> Q.of_string s
      | _ -> assert false
    in
    List.fold_left
      ~f:(fun m (k, v) -> M.add k v m)
      ~init:M.empty
      (List.t_of_sexp
         (Sexplib.Conv.pair_of_sexp elt_of_sexp q_of_sexp)
         sexp)

  let pp sep pp_elt fs s = List.pp sep pp_elt fs (M.bindings s)
  let empty = M.empty
  let if_nz q = if Q.equal Q.zero q then None else Some q

  let add m x i =
    M.update x (function Some j -> if_nz Q.(i + j) | None -> if_nz i) m

  let remove m x = M.remove x m

  let union m n =
    M.merge
      (fun _ m_q n_q ->
        match (m_q, n_q) with
        | Some i, Some j -> if_nz Q.(i + j)
        | Some i, None | None, Some i -> Some i
        | None, None -> None )
      m n

  let map m ~f =
    let m' = M.empty in
    let m, m' =
      M.fold
        (fun x i (m, m') ->
          let x', i' = f x i in
          if x' == x then
            if Q.equal i' i then (m, m') else (M.add x i' m, m')
          else (M.remove x m, add m' x' i') )
        m (m, m')
    in
    M.fold (fun x i m -> add m x i) m' m

  let map_counts m ~f = M.mapi f m
  let length m = M.cardinal m
  let count m x = try M.find x m with Not_found -> Q.zero
  let min_elt_exn = M.min_binding
  let min_elt = M.min_binding_opt
  let to_list m = M.bindings m
  let iter m ~f = M.iter f m
  let exists m ~f = M.exists f m
  let fold m ~f ~init = M.fold f m init
end
