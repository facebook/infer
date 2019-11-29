(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Wrappers for making pretty-printable modules *)

val pp_collection : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit

module type PrintableType = sig
  type t

  val pp : F.formatter -> t -> unit
end

module type PrintableEquatableType = sig
  include PrintableType

  val equal : t -> t -> bool
end

module type PrintableOrderedType = sig
  include Caml.Set.OrderedType

  include PrintableType with type t := t
end

module type PPSet = sig
  include Caml.Set.S

  val is_singleton_or_more : t -> elt IContainer.singleton_or_more

  include PrintableType with type t := t

  val pp_element : F.formatter -> elt -> unit
end

module type MonoMap = sig
  type key

  type value

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : key -> t -> bool

  val add : key -> value -> t -> t

  val update : key -> (value option -> value option) -> t -> t

  val singleton : key -> value -> t

  val remove : key -> t -> t

  val merge : (key -> value option -> value option -> value option) -> t -> t -> t

  val union : (key -> value -> value -> value option) -> t -> t -> t

  val compare : (value -> value -> int) -> t -> t -> int

  val equal : (value -> value -> bool) -> t -> t -> bool

  val iter : (key -> value -> unit) -> t -> unit

  val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (key -> value -> bool) -> t -> bool

  val exists : (key -> value -> bool) -> t -> bool

  val filter : (key -> value -> bool) -> t -> t

  val partition : (key -> value -> bool) -> t -> t * t

  val cardinal : t -> int

  val bindings : t -> (key * value) list

  val min_binding : t -> key * value

  val min_binding_opt : t -> (key * value) option

  val max_binding : t -> key * value

  val max_binding_opt : t -> (key * value) option

  val choose : t -> key * value

  val choose_opt : t -> (key * value) option

  val split : key -> t -> t * value option * t

  val find : key -> t -> value

  val find_opt : key -> t -> value option

  val find_first : (key -> bool) -> t -> key * value

  val find_first_opt : (key -> bool) -> t -> (key * value) option

  val find_last : (key -> bool) -> t -> key * value

  val find_last_opt : (key -> bool) -> t -> (key * value) option

  val map : (value -> value) -> t -> t

  val mapi : (key -> value -> value) -> t -> t

  val is_singleton_or_more : t -> (key * value) IContainer.singleton_or_more
end

module type PPMap = sig
  include Caml.Map.S

  val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t

  val is_singleton_or_more : 'a t -> (key * 'a) IContainer.singleton_or_more

  val pp_key : F.formatter -> key -> unit

  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

module type PPMonoMap = sig
  include MonoMap

  include PrintableType with type t := t

  val pp_key : F.formatter -> key -> unit
end

module MakePPSet (Ord : PrintableOrderedType) : PPSet with type elt = Ord.t

module MakePPMap (Ord : PrintableOrderedType) : PPMap with type key = Ord.t

module PPMonoMapOfPPMap (M : PPMap) (Val : PrintableType) :
  PPMonoMap with type key = M.key and type value = Val.t and type t = Val.t M.t

module MakePPMonoMap (Ord : PrintableOrderedType) (Val : PrintableType) :
  PPMonoMap with type key = Ord.t and type value = Val.t

module type PrintableRankedType = sig
  include PrintableType

  val equal : t -> t -> bool

  val to_rank : t -> int
end

(** set where at most one element of a given rank can be present *)
module type PPUniqRankSet = sig
  type t

  type elt

  val add : t -> elt -> t

  val empty : t

  val equal : t -> t -> bool

  val find_rank : t -> int -> elt option

  val fold : t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum

  val fold_map : t -> init:'accum -> f:('accum -> elt -> 'accum * elt) -> 'accum * t

  val is_empty : t -> bool

  val is_singleton : t -> bool

  val is_subset : t -> of_:t -> bool

  val map : t -> f:(elt -> elt) -> t

  val singleton : elt -> t

  val union_prefer_left : t -> t -> t
  (** in case an element with the same rank is present both in [lhs] and [rhs], keep the one from
     [lhs] in [union_prefer_left lhs rhs] *)

  val pp : ?print_rank:bool -> F.formatter -> t -> unit
end

module MakePPUniqRankSet (Val : PrintableRankedType) : PPUniqRankSet with type elt = Val.t
