(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Types : sig
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate
end

open! Types

(** This exception can be raised by abstract interpreters to stop the analysis early without
    triggering further errors. Clients who raise this exception should catch it eventually. *)
exception Stop_analysis

(** Abstract domains and domain combinators *)

module type S = sig
  include PrettyPrintable.PrintableType

  val ( <= ) : lhs:t -> rhs:t -> bool
  (** the partial order induced by join *)

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t
end

include
  (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
  sig
    [@@@warning "-60"]

    (** a trivial domain *)
    module Empty : S with type t = unit
end

(** A domain with an explicit bottom value *)
module type WithBottom = sig
  include S

  val empty : t
  (** The bottom value of the domain.
      Naming it empty instead of bottom helps to bind the empty
      value for sets and maps to the natural definition for bottom *)

  val is_empty : t -> bool
  (** Return true if this is the bottom value *)
end

(** A domain with an explicit top value *)
module type WithTop = sig
  include S

  val top : t

  val is_top : t -> bool
end

(** Lift a pre-domain to a domain *)
module BottomLifted (Domain : S) : sig
  include WithBottom with type t = Domain.t bottom_lifted

  val map : f:(Domain.t -> Domain.t) -> t -> t
end

(* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
include
  sig
    [@@@warning "-60"]

    (** Create a domain with Top element from a pre-domain *)
    module TopLifted (Domain : S) : WithTop with type t = Domain.t top_lifted
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

module type FiniteSetS = sig
  include PrettyPrintable.PPSet

  include WithBottom with type t := t
end

(** Lift a PPSet to a powerset domain ordered by subset. The elements of the set should be drawn from
    a *finite* collection of possible values, since the widening operator here is just union. *)
module FiniteSetOfPPSet (PPSet : PrettyPrintable.PPSet) : FiniteSetS with type elt = PPSet.elt

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

(** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
    Every element implicitly maps to bottom unless it is explicitly bound to something else.
    Uses PPMap as the underlying map *)
module MapOfPPMap (PPMap : PrettyPrintable.PPMap) (ValueDomain : S) :
  MapS with type key = PPMap.key and type value = ValueDomain.t

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

(* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)

include
  sig
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
    incremented and decremented *)
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

(** Domain whose members are stacks of elements (lists, last pushed is head of the list),
    partially ordered by the prefix relation ([c;b;a] <= [b;a]), and whose join computes the
    longest common prefix (so [c;b;a] join [f;g;b;c;a] = [a]), so the top element is the empty
    stack. *)
module StackDomain (Element : PrettyPrintable.PrintableOrderedType) : sig
  include WithTop with type t = Element.t list

  val push : Element.t -> t -> t

  val pop : t -> t
  (** throws exception on empty/top *)
end
