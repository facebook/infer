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

module Make (Set : Set) : sig
  module Repr : sig
    type t = private Set.elt
  end

  type t

  val create : unit -> t

  val find : t -> Set.elt -> Repr.t

  val union : t -> Set.elt -> Set.elt -> (Set.elt * Set.elt) option
  (** [union t e1 e2] returns [None] if [e1] and [e2] were already in the same set, [Some (a, b)] if [a] is merged into [b] (were [(a, b)] is either [(e1, e2)] or [(e2, e1)]). *)

  val find_create_set : t -> Repr.t -> Set.t

  val find_set : t -> Repr.t -> Set.t option

  val fold_sets : (t, Repr.t * Set.t, 'accum) Container.fold
  (** It is safe to call [find] or [union] while folding. *)
end
