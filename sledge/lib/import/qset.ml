(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Qset - Set with (signed) rational multiplicity for each element *)

open Import0

module type S = sig
  type elt
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash_fold_t : elt Hash.folder -> t Hash.folder
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
  val pp : (unit, unit) fmt -> (elt * Q.t) pp -> t pp

  val empty : t
  (** The empty multiset over the provided order. *)

  val add : t -> elt -> Q.t -> t
  (** Add to multiplicity of single element. [O(log n)] *)

  val remove : t -> elt -> t
  (** Set the multiplicity of an element to zero. [O(log n)] *)

  val union : t -> t -> t
  (** Sum multiplicities pointwise. [O(n + m)] *)

  val length : t -> int
  (** Number of elements with non-zero multiplicity. [O(1)]. *)

  val count : t -> elt -> Q.t
  (** Multiplicity of an element. [O(log n)]. *)

  val map : t -> f:(elt -> Q.t -> elt * Q.t) -> t
  (** Map over the elements in ascending order. Preserves physical equality
      if [f] does. *)

  val map_counts : t -> f:(elt -> Q.t -> Q.t) -> t
  (** Map over the multiplicities of the elements in ascending order. *)

  val fold : t -> f:(elt -> Q.t -> 's -> 's) -> init:'s -> 's
  (** Fold over the elements in ascending order. *)

  val iter : t -> f:(elt -> Q.t -> unit) -> unit
  (** Iterate over the elements in ascending order. *)

  val exists : t -> f:(elt -> Q.t -> bool) -> bool
  (** Search for an element satisfying a predicate. *)

  val min_elt : t -> (elt * Q.t) option
  (** Minimum element. *)

  val min_elt_exn : t -> elt * Q.t
  (** Minimum element. *)

  val to_list : t -> (elt * Q.t) list
  (** Convert to a list of elements in ascending order. *)
end

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

  let length m = M.cardinal m
  let count m x = try M.find x m with Not_found -> Q.zero
  let fold m ~f ~init = M.fold (fun key data s -> f key data s) m init

  let map m ~f =
    let m' = M.empty in
    let m, m' =
      fold m ~init:(m, m') ~f:(fun x i (m, m') ->
          let x', i' = f x i in
          if x' == x then
            if Q.equal i' i then (m, m') else (M.add x i' m, m')
          else (M.remove x m, add m' x' i') )
    in
    fold m' ~init:m ~f:(fun x i m -> add m x i)

  let map_counts m ~f = M.mapi (fun key data -> f key data) m
  let iter m ~f = M.iter (fun key data -> f key data) m
  let exists m ~f = M.exists (fun key data -> f key data) m
  let min_elt = M.min_binding_opt
  let min_elt_exn = M.min_binding
  let to_list m = M.bindings m
end
