(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Variables *)
module type S = sig
  type t [@@deriving compare, equal, sexp]
  type strength = t -> [`Universal | `Existential | `Anonymous] option

  val ppx : strength -> t pp
  val pp : t pp

  module Map : sig
    include NS.Map.S with type key := t

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  end

  module Set : sig
    include NS.Set.S with type elt := t

    val t_of_sexp : Sexp.t -> t
    val ppx : strength -> t pp
    val pp : t pp
    val pp_xs : t pp
    val pp_diff : (t * t) pp
  end

  val id : t -> int
  val name : t -> string
  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Create a variable identified by [id]. The [id] uniquely identifies the
      variable, and must be positive. *)

  (** Renaming Substitutions: injective maps from variables to variables *)
  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> x * Set.t
    val invert : t -> t

    val restrict_dom : t -> Set.t -> x
    (** restrict the domain of a substitution to a set, and yield the range
        of the unrestricted substitution *)

    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val fold : t -> 's -> f:(var -> var -> 's -> 's) -> 's
    val apply : t -> var -> var
  end
end
