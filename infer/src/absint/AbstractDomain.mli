(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {1 Abstract domains and domain combinators} *)

module Types : sig
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate [@@deriving equal]

  type ('below, 'astate, 'above) below_above = Below of 'below | Above of 'above | Val of 'astate
end

open! Types

module type Comparable = sig
  include PrettyPrintable.PrintableType

  val leq : lhs:t -> rhs:t -> bool
  (** the implication relation: [lhs <= rhs] means [lhs |- rhs] *)
end

module type Disjunct = sig
  include Comparable

  val equal_fast : t -> t -> bool
  (** [equal_fast x y] must imply [x <=> y]; it's a good idea for this function to be "fast", e.g.
      not depend on the size of its input *)

  val is_normal : t -> bool
  (** test if the abstract state represents exactly concrete states *)

  val is_exceptional : t -> bool
  (** test if the abstract state represents exactly exceptional concrete states *)

  val is_executable : t -> bool
  (** test if the abstract state represents executable states, e.g. [ContinueProgram] or
      [ExceptionRaised]. *)

  val exceptional_to_normal : t -> t
  (** convert all exceptional states into normal states (used when reaching a handler) *)
end

module type S = sig
  include Comparable

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t
end

include (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
  sig
  [@@@warning "-unused-module"]

  type empty = |

  module Empty : S with type t = empty

  (** a trivial domain *)
  module Unit : S with type t = unit
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

(** A domain with an explicit bottom and top values *)
module type WithBottomTop = sig
  include S

  val bottom : t

  val is_bottom : t -> bool

  val top : t

  val is_top : t -> bool
end

(** Create a domain with Bottom element from a pre-domain *)
module BottomLifted (Domain : S) : sig
  include WithBottom with type t = Domain.t bottom_lifted

  val map : f:(Domain.t -> Domain.t) -> t -> t
end

module BottomLiftedUtils : sig
  val pp_bottom : Format.formatter -> unit
end

(** Create a domain with Top element from a pre-domain *)
module TopLifted (Domain : S) : sig
  include WithTop with type t = Domain.t top_lifted

  val map : (Domain.t -> Domain.t) -> t -> t

  val get : default:'a -> (Domain.t -> 'a) -> t -> 'a
end

module TopLiftedUtils : sig
  val pp_top : Format.formatter -> unit
end

(** Create a domain with Bottom and Top elements from a pre-domain *)
module BottomTopLifted (Domain : S) : WithBottomTop

(** Cartesian product of two domains. *)
module Pair (Domain1 : S) (Domain2 : S) : S with type t = Domain1.t * Domain2.t

module PairWithBottom (Domain1 : WithBottom) (Domain2 : WithBottom) :
  WithBottom with type t = Domain1.t * Domain2.t

module PairWithTop (Domain1 : WithTop) (Domain2 : WithTop) :
  WithTop with type t = Domain1.t * Domain2.t

module PairDisjunct (Domain1 : Disjunct) (Domain2 : Disjunct) :
  Disjunct with type t = Domain1.t * Domain2.t

(** Flat abstract domain: Bottom, Top, and non-comparable elements in between *)
module Flat (V : PrettyPrintable.PrintableEquatableType) : sig
  include WithBottom

  include WithTop with type t := t

  val v : V.t -> t

  val get : t -> V.t option
end

include sig
  [@@@warning "-unused-module"]

  (** Stacked abstract domain: tagged union of [Below], [Val], and [Above] domains where all
      elements of [Below] are strictly smaller than all elements of [Val] which are strictly smaller
      than all elements of [Above] *)
  module Stacked (Below : S) (Val : S) (Above : S) :
    S with type t = (Below.t, Val.t, Above.t) below_above
end

module StackedUtils : sig
  val leq :
       leq_below:(lhs:'b -> rhs:'b -> bool)
    -> leq:(lhs:'v -> rhs:'v -> bool)
    -> leq_above:(lhs:'a -> rhs:'a -> bool)
    -> lhs:('b, 'v, 'a) below_above
    -> rhs:('b, 'v, 'a) below_above
    -> bool

  val compare :
       ('b, 'v, 'a) below_above
    -> ('b, 'v, 'a) below_above
    -> cmp_below:('b -> 'b -> int)
    -> cmp:('v -> 'v -> int)
    -> cmp_above:('a -> 'a -> int)
    -> int

  val pp :
       pp_below:(Format.formatter -> 'b -> unit)
    -> pp:(Format.formatter -> 'v -> unit)
    -> pp_above:(Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> ('b, 'v, 'a) below_above
    -> unit

  val combine :
       dir:[`Increasing | `Decreasing]
    -> ('b, 'v, 'a) below_above
    -> ('b, 'v, 'a) below_above
    -> f_below:('b -> 'b -> 'b)
    -> f:('v -> 'v -> 'v)
    -> f_above:('a -> 'a -> 'a)
    -> ('b, 'v, 'a) below_above

  val map :
       ('b, 'v, 'a) below_above
    -> f_below:('b -> 'b2)
    -> f:('v -> 'v2)
    -> f_above:('a -> 'a2)
    -> ('b2, 'v2, 'a2) below_above
end

(** Abstracts a set of [Element]s by keeping its smallest representative only. The widening is
    terminating only if the order fulfills the descending chain condition. *)
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
  [@@@warning "-unused-module"]

  (** Lift a PPSet to a powerset domain ordered by subset. The elements of the set should be drawn
      from a *finite* collection of possible values, since the widening operator here is just union. *)
  module FiniteSetOfPPSet (PPSet : PrettyPrintable.PPSet) :
    FiniteSetS with type t = PPSet.t with type elt = PPSet.elt
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
  [@@@warning "-unused-module"]

  (** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
      Every element implicitly maps to bottom unless it is explicitly bound to something else. Uses
      PPMap as the underlying map *)
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

(** Similar to [InvertedMap] but it guarantees that it has a canonical form. For example, both
    [{a -> top_v}] and [empty] represent the same abstract value [top] in [InvertedMap], but in this
    implementation, [top] is always implemented as [empty] by not adding the [top_v] explicitly. *)
module SafeInvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : WithTop) :
  InvertedMapS with type key = Key.t and type value = ValueDomain.t

(* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)

include sig
  [@@@warning "-unused-module"]

  module FiniteMultiMap
      (Key : PrettyPrintable.PrintableOrderedType)
      (Value : PrettyPrintable.PrintableOrderedType) : sig
    include WithBottom

    val singleton : Key.t -> Value.t -> t [@@warning "-unused-value-declaration"]

    val add : Key.t -> Value.t -> t -> t [@@warning "-unused-value-declaration"]

    val set_to_single_value : Key.t -> Value.t -> t -> t
    (** [set_to_single_value k v m] is equivalent (but faster) to [add k v (remove_all k m)]. *)

    val mem : Key.t -> t -> bool [@@warning "-unused-value-declaration"]

    val remove : Key.t -> Value.t -> t -> t [@@warning "-unused-value-declaration"]

    val remove_all : Key.t -> t -> t [@@warning "-unused-value-declaration"]

    val find_all : Key.t -> t -> Value.t list

    val find_fold : (Value.t -> 'a -> 'a) -> Key.t -> t -> 'a -> 'a
    (** Fold over the values associated to one key *)

    val get_all_keys : t -> Key.t list

    val exists : (Key.t -> Value.t -> bool) -> t -> bool

    val fold : (Key.t -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a

    val filter : (Key.t -> Value.t -> bool) -> t -> t
  end
end

(** Boolean domain ordered by p || ~q. Useful when you want a boolean that's true only when it's
    true in both conditional branches. *)
module BooleanAnd : S with type t = bool

(** Boolean domain ordered by ~p || q. Useful when you want a boolean that's true only when it's
    true in one conditional branch. *)
module BooleanOr : WithBottom with type t = bool

module type MaxCount = sig
  val max : int
  (** must be positive *)
end

(** Domain keeping a non-negative count with a bounded maximum value. The count can be only
    incremented and decremented. *)
module CountDomain (MaxCount : MaxCount) : sig
  include WithBottom with type t = private int

  (** top is maximum value *)
  include WithTop with type t := t

  val increment : t -> t
  (** bump the count by one if it is less than the max *)

  val decrement : t -> t
  (** descrease the count by one if it is greater than 0 *)

  val add : t -> t -> t
  (** capped sum of two states *)
end

(** Domain keeping a non-negative count with a bounded maximum value. [join] is minimum and [top] is
    zero. *)
module DownwardIntDomain (MaxCount : MaxCount) : sig
  (** top is zero *)
  include WithTop with type t = private int

  (** bottom is the provided maximum *)
  include WithBottom with type t := t

  val increment : t -> t
  (** bump the count by one if this won't cross the maximum *)

  val decrement : t -> t
  (** decrease the count by one if it is greater than 0 *)
end
