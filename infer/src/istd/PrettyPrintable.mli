(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Wrappers for making pretty-printable modules *)

val pp_collection : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit

module type PrintableOrderedType = sig
  include Caml.Set.OrderedType

  val pp : F.formatter -> t -> unit
end

module type PPSet = sig
  include Caml.Set.S

  val is_singleton_or_more : t -> elt IContainer.singleton_or_more

  val pp_element : F.formatter -> elt -> unit

  val pp : F.formatter -> t -> unit
end

module type PPMap = sig
  include Caml.Map.S

  val is_singleton_or_more : 'a t -> (key * 'a) IContainer.singleton_or_more

  val pp_key : F.formatter -> key -> unit

  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

module MakePPSet (Ord : PrintableOrderedType) : PPSet with type elt = Ord.t

module MakePPMap (Ord : PrintableOrderedType) : PPMap with type key = Ord.t
