(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Types : sig
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate

  type ('below, 'above) below_above = Below of 'below | Above of 'above
end

open! Types

exception Stop_analysis
(** This exception can be raised by abstract interpreters to stop the analysis early without
    triggering further errors. Clients who raise this exception should catch it eventually. *)

(** Abstract domains and domain combinators *)

module type NoJoin = sig
  include PrettyPrintable.PrintableType

  val leq : lhs:t -> rhs:t -> bool
  (** the implication relation: [lhs <= rhs] means [lhs |- rhs] *)
end

module type S = sig
  include NoJoin

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t
end

include (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
  sig
  [@@@warning "-60"]

  module Empty : S with type t = unit
  (** a trivial domain *)
end

(** A domain with an explicit bottom value *)
module type WithBottom = sig
  include S

  val bottom : t
  (** The bottom value of the domain. *)

  val is_bottom : t -> bool
  (** Return true if this is the bottom value *)
end

(** A domain with an explicit top value *)
module type WithTop = sig
  include S

  val top : t

  val is_top : t -> bool
end

(** Create a domain with Bottom element from a pre-domain *)
module BottomLifted (Domain : S) : sig
  include WithBottom with type t = Domain.t bottom_lifted

  val map : f:(Domain.t -> Domain.t) -> t -> t
end

(** Create a domain with Top element from a pre-domain *)
module TopLifted (Domain : S) : WithTop with type t = Domain.t top_lifted

module TopLiftedUtils : sig
  val pp_top : Format.formatter -> unit
end

(** Cartesian product of two domains. *)
module Pair (Domain1 : S) (Domain2 : S) : S with type t = Domain1.t * Domain2.t

(** Flat abstract domain: Bottom, Top, and non-comparable elements in between *)
module Flat (V : PrettyPrintable.PrintableEquatableType) : sig
  include WithBottom

  include WithTop with type t := t

  val v : V.t -> t

  val get : t -> V.t option
end

include sig
  [@@@warning "-60"]

  (** Stacked abstract domain: tagged union of [Below] and [Above] domains where all elements of [Below] are strictly smaller than elements of [Above] *)
  module Stacked (Below : S) (Above : S) : S with type t = (Below.t, Above.t) below_above
end

module StackedUtils : sig
  val leq :
       leq_below:(lhs:'b -> rhs:'b -> bool)
    -> leq_above:(lhs:'a -> rhs:'a -> bool)
    -> lhs:('b, 'a) below_above
    -> rhs:('b, 'a) below_above
    -> bool

  val compare :
       ('b, 'a) below_above
    -> ('b, 'a) below_above
    -> cmp_below:('b -> 'b -> int)
    -> cmp_above:('a -> 'a -> int)
    -> int

  val pp :
       pp_below:(Format.formatter -> 'b -> unit)
    -> pp_above:(Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> ('b, 'a) below_above
    -> unit

  val combine :
       dir:[`Increasing | `Decreasing]
    -> ('b, 'a) below_above
    -> ('b, 'a) below_above
    -> f_below:('b -> 'b -> 'b)
    -> f_above:('a -> 'a -> 'a)
    -> ('b, 'a) below_above

  val map :
    ('b, 'a) below_above -> f_below:('b -> 'b2) -> f_above:('a -> 'a2) -> ('b2, 'a2) below_above
end

(** Abstracts a set of [Element]s by keeping its smallest representative only.
  The widening is terminating only if the order fulfills the descending chain condition. *)
module MinReprSet (Element : PrettyPrintable.PrintableOrderedType) : sig
  type elt = Element.t

  include Caml.Set.OrderedType

  include WithBottom with type t := t

  val singleton : elt -> t

  val min_elt : t -> elt option

  val add : elt -> t -> t

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val exists : (elt -> bool) -> t -> bool
end

module type FiniteSetS = sig
  include PrettyPrintable.PPSet

  include WithBottom with type t := t
end

include sig
  [@@@warning "-60"]

  (** Lift a PPSet to a powerset domain ordered by subset. The elements of the set should be drawn from
    a *finite* collection of possible values, since the widening operator here is just union. *)
  module FiniteSetOfPPSet (PPSet : PrettyPrintable.PPSet) : FiniteSetS with type elt = PPSet.elt
end

(** Lift a set to a powerset domain ordered by subset. The elements of the set should be drawn from
    a *finite* collection of possible values, since the widening operator here is just union. *)
module FiniteSet (Element : PrettyPrintable.PrintableOrderedType) :
  FiniteSetS with type elt = Element.t

module type InvertedSetS = sig
  include PrettyPrintable.PPSet

  include WithTop with type t := t
end

(** Lift a set to a powerset domain ordered by superset, so the join operator is intersection *)
module InvertedSet (Element : PrettyPrintable.PrintableOrderedType) :
  InvertedSetS with type elt = Element.t

module type MapS = sig
  include PrettyPrintable.PPMonoMap

  include WithBottom with type t := t
end

include sig
  [@@@warning "-60"]

  (** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
    Every element implicitly maps to bottom unless it is explicitly bound to something else.
    Uses PPMap as the underlying map *)
  module MapOfPPMap (PPMap : PrettyPrintable.PPMap) (ValueDomain : S) :
    MapS with type key = PPMap.key and type value = ValueDomain.t and type t = ValueDomain.t PPMap.t
end

(** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
    Every element implicitly maps to bottom unless it is explicitly bound to something else *)
module Map (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) :
  MapS with type key = Key.t and type value = ValueDomain.t

module type InvertedMapS = sig
  include PrettyPrintable.PPMonoMap

  include WithTop with type t := t
end

(** Map domain ordered by intersection over the set of bindings, so the top element is the empty
    map. Every element implictly maps to top unless it is explicitly bound to something else *)
module InvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) :
  InvertedMapS with type key = Key.t and type value = ValueDomain.t

(** Similar to [InvertedMap] but it guarantees that it has a canonical form. For example, both [{a
   -> top_v}] and [empty] represent the same abstract value [top] in [InvertedMap], but in this
   implementation, [top] is always implemented as [empty] by not adding the [top_v] explicitly. *)
module SafeInvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : WithTop) :
  InvertedMapS with type key = Key.t and type value = ValueDomain.t

(* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)

include sig
  [@@@warning "-60"]

  module FiniteMultiMap
      (Key : PrettyPrintable.PrintableOrderedType)
      (Value : PrettyPrintable.PrintableOrderedType) : sig
    include WithBottom

    val add : Key.t -> Value.t -> t -> t [@@warning "-32"]

    val mem : Key.t -> t -> bool [@@warning "-32"]

    val remove : Key.t -> Value.t -> t -> t [@@warning "-32"]
  end
end

module BooleanAnd : S with type t = bool
(** Boolean domain ordered by p || ~q. Useful when you want a boolean that's true only when it's
    true in both conditional branches. *)

module BooleanOr : WithBottom with type t = bool
(** Boolean domain ordered by ~p || q. Useful when you want a boolean that's true only when it's
    true in one conditional branch. *)

module type MaxCount = sig
  val max : int
  (** must be positive *)
end

(** Domain keeping a non-negative count with a bounded maximum value. The count can be only
    incremented and decremented. *)
module CountDomain (MaxCount : MaxCount) : sig
  include WithBottom with type t = private int

  include WithTop with type t := t
  (** top is maximum value *)

  val increment : t -> t
  (** bump the count by one if it is less than the max *)

  val decrement : t -> t
  (** descrease the count by one if it is greater than 0 *)

  val add : t -> t -> t
  (** capped sum of two states *)
end

(** Domain keeping a non-negative count with a bounded maximum value. 
    [join] is minimum and [top] is zero. *)
module DownwardIntDomain (MaxCount : MaxCount) : sig
  include WithTop with type t = private int
  (** top is zero *)

  include WithBottom with type t := t
  (** bottom is the provided maximum *)

  val increment : t -> t
  (** bump the count by one if this won't cross the maximum *)

  val decrement : t -> t
  (** decrease the count by one if it is greater than 0 *)
end
