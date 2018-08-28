(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Set = sig
  type elt [@@deriving compare]

  type t

  val create : elt -> t

  val compare_size : t -> t -> int

  val merge : from:t -> to_:t -> unit
end

module Make (Set : Set) = struct
  module H = struct
    include Caml.Hashtbl

    let fold : (('a, 'b) t, 'a * 'b, 'accum) Container.fold =
     fun h ~init ~f ->
      let f' k v accum = f accum (k, v) in
      fold f' h init
  end

  module Repr : sig
    (* Sort-of abstracting away the fact that a representative is just an element itself.
      This ensures that the [Sets] hashtable is accessed with representative only. *)

    type t = private Set.elt

    val equal : t -> t -> bool

    val of_elt : Set.elt -> t

    val is_simpler_than : t -> t -> bool
  end = struct
    type t = Set.elt [@@deriving compare]

    let equal = [%compare.equal: t]

    let of_elt e = e

    let is_simpler_than r1 r2 = compare r1 r2 <= 0
  end

  module Reprs = struct
    type t = (Set.elt, Repr.t) H.t

    let create () = H.create 1

    let is_a_repr (t : t) e = not (H.mem t e)

    let rec find (t : t) e : Repr.t =
      match H.find_opt t e with
      | None ->
          Repr.of_elt e
      | Some r ->
          let r' = find t (r :> Set.elt) in
          if not (phys_equal r r') then H.replace t e r' ;
          r'


    let merge (t : t) ~(from : Repr.t) ~(to_ : Repr.t) = H.replace t (from :> Set.elt) to_
  end

  module Sets = struct
    type t = (Repr.t, Set.t) H.t

    let create () = H.create 1

    let find t r = H.find_opt t r

    let find_create t (r : Repr.t) =
      match H.find_opt t r with
      | Some set ->
          set
      | None ->
          let set = Set.create (r :> Set.elt) in
          H.replace t r set ; set


    let fold = H.fold

    let remove_now t r = H.remove t r
  end

  (**
    Data-structure for disjoint sets.
    [reprs] is the mapping element -> representative
    [sets] is the mapping representative -> set

    It implements path-compression and union by size, hence find and union are amortized O(1)-ish.

    [nb_iterators] and [to_remove] are used to defer removing elements to avoid iterator invalidation during fold.
  *)
  type t = {reprs: Reprs.t; sets: Sets.t; mutable nb_iterators: int; mutable to_remove: Repr.t list}

  let create () = {reprs= Reprs.create (); sets= Sets.create (); nb_iterators= 0; to_remove= []}

  let find t e = Reprs.find t.reprs e

  let do_merge t ~from_r ~from_set ~to_r ~to_set =
    Reprs.merge t.reprs ~from:from_r ~to_:to_r ;
    Set.merge ~from:from_set ~to_:to_set ;
    if t.nb_iterators <= 0 then Sets.remove_now t.sets from_r
    else t.to_remove <- from_r :: t.to_remove


  let find_create_set t repr = Sets.find_create t.sets repr

  let union t e1 e2 =
    let repr1 = find t e1 in
    let repr2 = find t e2 in
    if Repr.equal repr1 repr2 then None
    else
      let set1 = find_create_set t repr1 in
      let set2 = find_create_set t repr2 in
      let cmp_size = Set.compare_size set1 set2 in
      if cmp_size < 0 || (Int.equal cmp_size 0 && Repr.is_simpler_than repr2 repr1) then (
        (* A desired side-effect of using [is_simpler_than] is that the representative for a set will always be a [`Node]. For now. *)
        do_merge t ~from_r:repr1 ~from_set:set1 ~to_r:repr2 ~to_set:set2 ;
        Some (e1, e2) )
      else (
        do_merge t ~from_r:repr2 ~from_set:set2 ~to_r:repr1 ~to_set:set1 ;
        Some (e2, e1) )


  let is_still_a_repr t ((repr : Repr.t), _) = Reprs.is_a_repr t.reprs (repr :> Set.elt)

  let after_fold t =
    let new_nb_iterators = t.nb_iterators - 1 in
    t.nb_iterators <- new_nb_iterators ;
    if new_nb_iterators <= 0 && not (List.is_empty t.to_remove) then (
      List.iter t.to_remove ~f:(Sets.remove_now t.sets) ;
      t.to_remove <- [] )


  let find_set t r = Sets.find t.sets r

  let fold_sets t ~init ~f =
    t.nb_iterators <- t.nb_iterators + 1 ;
    match IContainer.filter ~fold:Sets.fold ~filter:(is_still_a_repr t) t.sets ~init ~f with
    | result ->
        after_fold t ; result
    | exception e ->
        (* Ensures [nb_iterators] is correct *)
        IExn.reraise_after ~f:(fun () -> after_fold t) e
end
