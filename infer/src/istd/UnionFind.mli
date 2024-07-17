(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

(** A union-find data structure. *)

module type Element = sig
  type t [@@deriving compare, equal]

  val is_simpler_than : t -> t -> bool
  (** will be used to choose a "simpler" representative for a given equivalence class when possible *)
end

module Make
    (X : Element)
    (XSet : Caml.Set.S with type elt = X.t)
    (XMap : Caml.Map.S with type key = X.t) : sig
  type t [@@deriving compare, equal]

  val pp : (F.formatter -> X.t -> unit) -> F.formatter -> t -> unit

  type repr = private X.t

  val empty : t

  val is_empty : t -> bool

  val union : t -> X.t -> X.t -> t * (X.t * repr) option
  (** return the optional new equality added between the old representatives of the two items in the
      form of "old representative = new representative", [None] if they were already in the same
      congruence class *)

  val find : t -> X.t -> repr
  (** return the element given if it wasn't found in the relation *)

  val fold_congruences : (t, repr * XSet.t, 'acc) Container.fold
  (** fold over the equivalence classes of the relation, singling out the representative for each
      class *)

  val reorient : should_keep:(X.t -> bool) -> t -> X.t XMap.t
  (** the relation [x -> x'] derived from the equality relation that relates all [x], [x'] such that
      [¬(should_keep x)], [should_keep x'], and [x=x'], as well as [y -> y'] when no element in the
      equivalence class of [y] satisfies [should_keep] and [y'] is the representative of the class *)

  val apply_subst : _ XMap.t -> t -> t
  (** [apply_subst subst uf] eliminate all variables in the domain of [subst] from [uf], keeping the
      smallest representative not in the domain of [subst] for each class. Classes without any such
      elements are kept intact. *)

  val filter : f:(X.t -> bool) -> t -> t
  (** only keep items satisfying [f] *)

  val fold_elements : (t, X.t, 'acc) Container.fold
end
