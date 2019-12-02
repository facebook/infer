(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for Propositions (i.e., Symbolic Heaps) *)

(** {2 Sets of Propositions} *)

module PropSet = Caml.Set.Make (struct
  type t = Prop.normal Prop.t

  let compare = Prop.compare_prop
end)

let compare = PropSet.compare

(** Sets of propositions. The invariant is maintaned that Prop.prop_rename_primed_footprint_vars is
    called on any prop added to the set. *)
type t = PropSet.t

let add tenv p pset =
  let ps = Prop.prop_expand tenv p in
  List.fold
    ~f:(fun pset' p' -> PropSet.add (Prop.prop_rename_primed_footprint_vars tenv p') pset')
    ~init:pset ps


(** Singleton set. *)
let singleton tenv p = add tenv p PropSet.empty

(** Set union. *)
let union = PropSet.union

(** Set membership *)
let mem p = PropSet.mem p

(** Set intersection *)
let inter = PropSet.inter

(** Set difference. *)
let diff = PropSet.diff

let empty = PropSet.empty

(** Set emptiness check. *)
let is_empty = PropSet.is_empty

(** Size of the set *)
let size = PropSet.cardinal

let filter = PropSet.filter

let from_proplist tenv plist = List.fold ~f:(fun pset p -> add tenv p pset) ~init:empty plist

let to_proplist pset = PropSet.elements pset

(** Apply function to all the elements of [propset], removing those where it returns [None]. *)
let map_option tenv f pset =
  let plisto = List.map ~f (to_proplist pset) in
  let plisto = List.filter ~f:(function Some _ -> true | None -> false) plisto in
  let plist = List.map ~f:(function Some p -> p | None -> assert false) plisto in
  from_proplist tenv plist


(** Apply function to all the elements of [propset]. *)
let map tenv f pset = from_proplist tenv (List.map ~f (to_proplist pset))

(** [fold f pset a] computes [f (... (f (f a p1) p2) ...) pn] where [p1 ... pN] are the elements of
    pset, in increasing order. *)
let fold f a pset =
  let l = to_proplist pset in
  List.fold ~f ~init:a l


(** [iter f pset] computes (f p1;f p2;..;f pN) where [p1 ... pN] are the elements of pset, in
    increasing order. *)
let iter = PropSet.iter

let subseteq = PropSet.subset

let partition = PropSet.partition

(** {2 Pretty print} *)

let d p ps =
  let plist = to_proplist ps in
  Propgraph.d_proplist p plist
