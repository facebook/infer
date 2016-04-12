(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

(** Wrappers for making pretty-printable modules *)

module type SetOrderedType = sig
  type t
  val compare : t -> t -> int
  val pp_element : F.formatter -> t -> unit
end

module type MapOrderedType = sig
  type t
  val compare : t -> t -> int
  val pp_key : F.formatter -> t -> unit
end

module type PPSet = sig
  include Set.S
  val pp_element : F.formatter -> elt -> unit
  val pp : F.formatter -> t -> unit
end

module type PPMap = sig
  include Map.S
  val pp_key : F.formatter -> key -> unit
  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

module MakePPSet : functor (Ord : SetOrderedType) -> sig
  include Set.S with type elt = Ord.t
  val pp_element : F.formatter -> Ord.t -> unit
  val pp : F.formatter -> t -> unit
end

module MakePPMap : functor (Ord : MapOrderedType) -> sig
  include Map.S with type key = Ord.t
  val pp_key : F.formatter -> Ord.t -> unit
  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end
