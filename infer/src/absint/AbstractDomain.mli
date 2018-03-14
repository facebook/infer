(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

module Types : sig
  type 'astate bottom_lifted = Bottom | NonBottom of 'astate

  type 'astate top_lifted = Top | NonTop of 'astate
end

open! Types

(** Abstract domains and domain combinators *)

module type S = sig
  type astate

  val ( <= ) : lhs:astate -> rhs:astate -> bool
  (** the partial order induced by join *)

  val join : astate -> astate -> astate

  val widen : prev:astate -> next:astate -> num_iters:int -> astate

  val pp : F.formatter -> astate -> unit
end

(** A domain with an explicit bottom value *)
module type WithBottom = sig
  include S

  val empty : astate
  (** The bottom value of the domain.
      Naming it empty instead of bottom helps to bind the empty
      value for sets and maps to the natural definition for bottom *)

  val is_empty : astate -> bool
  (** Return true if this is the bottom value *)
end

(** A domain with an explicit top value *)
module type WithTop = sig
  include S

  val top : astate
end

(** Lift a pre-domain to a domain *)
module BottomLifted (Domain : S) : sig
  type astate = Domain.astate bottom_lifted

  include WithBottom with type astate := astate
end

(** Create a domain with Top element from a pre-domain *)
module TopLifted (Domain : S) : sig
  type astate = Domain.astate top_lifted

  include WithTop with type astate := astate
end

(** Cartesian product of two domains. *)
module Pair (Domain1 : S) (Domain2 : S) : S with type astate = Domain1.astate * Domain2.astate

(** Lift a set to a powerset domain ordered by subset. The elements of the set should be drawn from
    a *finite* collection of possible values, since the widening operator here is just union. *)
module FiniteSet (Element : PrettyPrintable.PrintableOrderedType) : sig
  include module type of PrettyPrintable.MakePPSet (Element)

  include WithBottom with type astate = t
end

(** Lift a set to a powerset domain ordered by superset, so the join operator is intersection *)
module InvertedSet (Element : PrettyPrintable.PrintableOrderedType) : sig
  include module type of PrettyPrintable.MakePPSet (Element)

  include S with type astate = t
end

(** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
    Every element implicitly maps to bottom unless it is explicitly bound to something else *)
module Map (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) : sig
  include module type of PrettyPrintable.MakePPMap (Key)

  include WithBottom with type astate = ValueDomain.astate t
end

(** Map domain ordered by intersection over the set of bindings, so the top element is the empty
    map. Every element implictly maps to top unless it is explicitly bound to something else *)
module InvertedMap (Key : PrettyPrintable.PrintableOrderedType) (ValueDomain : S) : sig
  include module type of PrettyPrintable.MakePPMap (Key)

  include S with type astate = ValueDomain.astate t
end

(** Boolean domain ordered by p || ~q. Useful when you want a boolean that's true only when it's
    true in both conditional branches. *)
module BooleanAnd : S with type astate = bool

(** Boolean domain ordered by ~p || q. Useful when you want a boolean that's true only when it's
    true in one conditional branch. *)
module BooleanOr : WithBottom with type astate = bool

module type MaxCount = sig
  val max : int
  (** must be positive *)
end

(** Domain keeping a non-negative count with a bounded maximum value. The count can be only
    incremented and decremented *)
module CountDomain (MaxCount : MaxCount) : sig
  include WithBottom with type astate = private int

  val top : astate  [@@warning "-32"]
  (** maximum value *)

  val is_top : astate -> bool  [@@warning "-32"]
  (** return true if this is the maximum value *)

  val increment : astate -> astate
  (** bump the count by one if it is less than the max *)

  val decrement : astate -> astate
  (** descrease the count by one if it is greater than 0 *)

  val add : astate -> astate -> astate
  (** capped sum of two states *)
end

(** Domain whose members are stacks of elements (lists, last pushed is head of the list),
    partially ordered by the prefix relation ([c;b;a] <= [b;a]), and whose join computes the
    longest common prefix (so [c;b;a] join [f;g;b;c;a] = [a]), so the top element is the empty
    stack. *)
module StackDomain (Element : PrettyPrintable.PrintableOrderedType) : sig
  include S with type astate = Element.t list

  val push : Element.t -> astate -> astate

  val pop : astate -> astate
  (** throws exception on empty *)

  val empty : astate

  val is_empty : astate -> bool
end
