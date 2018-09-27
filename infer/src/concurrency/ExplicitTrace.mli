(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A powerset domain of traces, with bottom = empty and join = union *)
module type FiniteSet = sig
  include PrettyPrintable.PPSet

  include AbstractDomain.WithBottom with type astate = t

  val with_callsite : astate -> CallSite.t -> astate
  (** Push given callsite onto all traces in set. Cf [TraceElem.with_callsite] *)
end

module type TraceElem = sig
  type elem_t

  (** An [elem] which occured at [loc], after the chain of steps (usually calls) in [trace].
      The [compare] function ignores traces, meaning any two traces leading to the same [elem] and
      [loc] are equal.  This has consequences on the powerset domain. *)
  type t = private {elem: elem_t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val make : elem_t -> Location.t -> t

  val get_loc : t -> Location.t
  (** Starting location of the trace: this is either [loc] if [trace==[]], or the head of [trace]. *)

  val make_loc_trace : ?nesting:int -> t -> Errlog.loc_trace

  val with_callsite : t -> CallSite.t -> t
  (** Push given callsite onto trace, extending the call chain by one. *)

  (** A powerset of traces, where there is at most one trace for each dinstinct pair of [elem] and
      [loc].  The traces in the set have priority over those [add]ed.  [join] is non-deterministic
      as to which representative is kept (due to the implementation of [Set.union]). *)
  module FiniteSet : FiniteSet with type elt = t
end

module MakeTraceElem (Elem : PrettyPrintable.PrintableOrderedType) :
  TraceElem with type elem_t = Elem.t
