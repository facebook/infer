(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Renaming Substitutions: injective maps from variables to variables *)

module type VAR = sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp

  module Map : sig
    include Map.S with type key := t

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  end

  module Set : Set.S with type elt := t

  val freshen : t -> wrt:Set.t -> t * Set.t
end

module type S = sig
  type var
  type set
  type t [@@deriving compare, equal, sexp]
  type x = {sub: t; dom: set; rng: set}

  val pp : t pp
  val empty : t
  val freshen : set -> wrt:set -> x * set
  val invert : t -> t

  val restrict_dom : t -> set -> x
  (** restrict the domain of a substitution to a set, and yield the range of
      the unrestricted substitution *)

  val is_empty : t -> bool
  val domain : t -> set
  val range : t -> set
  val fold : t -> 's -> f:(var -> var -> 's -> 's) -> 's
  val apply : t -> var -> var
end
