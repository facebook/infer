(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type S = sig
  include TraceElem.S

  val is_footprint : t -> bool

  val make_footprint : AccessPath.t -> CallSite.t -> t

  val get_footprint_access_path: t -> AccessPath.t option

  (** return Some (kind) if the call site is a taint source, None otherwise *)
  val get : CallSite.t -> t option
end

module Dummy : S with type t = unit
