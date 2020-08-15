(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A union-find data structure. *)

module type Element = sig
  type t [@@deriving compare]

  val is_simpler_than : t -> t -> bool
  (** will be used to choose a "simpler" representative for a given equivalence class when possible *)
end

module Make (X : Element) : sig
  type t

  type repr = private X.t

  module Set : Caml.Set.S with type elt = X.t

  val empty : t

  val union : t -> X.t -> X.t -> t

  val find_opt : t -> X.t -> repr option

  val find : t -> X.t -> repr
  (** like [find_opt] but returns the element given if it wasn't found in the relation *)

  val fold_congruences : (t, repr * Set.t, 'acc) Container.fold
  (** fold over the equivalence classes of the relation, singling out the representative for each
      class *)
end
