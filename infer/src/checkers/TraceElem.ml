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

module type Kind = sig
  type t [@@deriving compare]

  val matches : caller:t -> callee:t -> bool
  (** Return true if the [caller] element kind matches the [callee] element kind. Used during trace
      expansion; we will only consider expanding the trace from caller into callee if this
      evaluates to true. This can normally just be [equal], but something fuzzier may be required
      if [t] is a type that contains identifiers from the caller/callee *)

  val pp : F.formatter -> t -> unit
end

module type S = sig
  type t [@@deriving compare]

  module Kind : Kind

  val call_site : t -> CallSite.t

  val kind : t -> Kind.t

  val make : ?indexes:IntSet.t -> Kind.t -> CallSite.t -> t

  val with_callsite : t -> CallSite.t -> t

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t
end
