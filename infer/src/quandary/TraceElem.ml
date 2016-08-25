(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

module type S = sig
  type kind
  type t

  val call_site : t -> CallSite.t
  val kind : t -> kind

  val make : kind -> CallSite.t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp_kind : F.formatter -> kind -> unit
  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t
end
