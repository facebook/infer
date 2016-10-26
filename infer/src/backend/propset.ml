(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Functions for Propositions (i.e., Symbolic Heaps) *)

module L = Logging
module F = Format

(** {2 Sets of Propositions} *)

module PropSet =
  Set.Make(struct
    type t = Prop.normal Prop.t
    let compare = Prop.prop_compare
  end)

let compare = PropSet.compare

(** Sets of propositions.
    The invariant is maintaned that Prop.prop_rename_primed_footprint_vars is called on any prop added to the set. *)
type t = PropSet.t

let add tenv p pset =
  let ps = Prop.prop_expand tenv p in
  IList.fold_left (fun pset' p' ->
      PropSet.add (Prop.prop_rename_primed_footprint_vars tenv p') pset'
    ) pset ps

(** Singleton set. *)
let singleton tenv p =
  add tenv p PropSet.empty

(** Set union. *)
let union = PropSet.union

(** Set membership *)
let mem p =
  PropSet.mem p

(** Set intersection *)
let inter = PropSet.inter

(** Set difference. *)
let diff =
  PropSet.diff

let empty = PropSet.empty

(** Set emptiness check. *)
let is_empty = PropSet.is_empty

(** Size of the set *)
let size = PropSet.cardinal

let filter = PropSet.filter

let from_proplist tenv plist =
  IList.fold_left (fun pset p -> add tenv p pset) empty plist

let to_proplist pset =
  PropSet.elements pset

(** Apply function to all the elements of [propset], removing those where it returns [None]. *)
let map_option tenv f pset =
  let plisto = IList.map f (to_proplist pset) in
  let plisto = IList.filter (function | Some _ -> true | None -> false) plisto in
  let plist = IList.map (function Some p -> p | None -> assert false) plisto in
  from_proplist tenv plist

(** Apply function to all the elements of [propset]. *)
let map tenv f pset =
  from_proplist tenv (IList.map f (to_proplist pset))

(** [fold f pset a] computes [f (... (f (f a p1) p2) ...) pn]
    where [p1 ... pN] are the elements of pset, in increasing order. *)
let fold f a pset =
  let l = to_proplist pset in
  IList.fold_left f a l

(** [iter f pset] computes (f p1;f p2;..;f pN)
    where [p1 ... pN] are the elements of pset, in increasing order. *)
let iter =
  PropSet.iter

let subseteq =
  PropSet.subset

let partition =
  PropSet.partition

(** {2 Pretty print} *)

(** Pretty print a set of propositions, obtained from the given prop. *)
let pp pe prop f pset =
  let plist = to_proplist pset in
  (Propgraph.pp_proplist pe "PROP" (prop, false)) f plist

let d p ps =
  let plist = to_proplist ps in
  Propgraph.d_proplist p plist
