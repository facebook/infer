(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** A union-find data structure. *)

module type Element = sig
  type t [@@deriving compare]

  val is_simpler_than : t -> t -> bool
  (** will be used to choose a "simpler" representative for a given equivalence class when possible *)
end

module Make (X : Element) (XSet : Caml.Set.S with type elt = X.t) : sig
  type t

  val pp :
    pp_empty:(F.formatter -> unit) -> (F.formatter -> X.t -> unit) -> F.formatter -> t -> unit

  type repr = private X.t

  val empty : t

  val is_empty : t -> bool

  val union : t -> X.t -> X.t -> t * (X.t * repr) option
  (** return the optional new equality added between the old representatives of the two items in the
      form of "old representative = new representative", [None] if they were already in the same
      congruence class *)

  val find_opt : t -> X.t -> repr option

  val find : t -> X.t -> repr
  (** return the element given if it wasn't found in the relation *)

  val fold_congruences : (t, repr * XSet.t, 'acc) Container.fold
  (** fold over the equivalence classes of the relation, singling out the representative for each
      class *)

  val filter_not_in_closed_set : keep:XSet.t -> t -> t
  (** only keep items in [keep], assuming that [keep] is closed under the relation, i.e. that if an
      item [x] is in [keep] then so are all the [y] such that [x=y] according to the relation *)
end
