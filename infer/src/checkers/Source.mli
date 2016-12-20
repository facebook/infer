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

  (** return Some (source) if the call site is a taint source, None otherwise *)
  val get : CallSite.t -> t option

  (** return each formal of the function paired with either Some(source) if the formal is a taint
      source, or None if the formal is not a taint source *)
  val get_tainted_formals : Procdesc.t -> (Mangled.t * Typ.t * t option) list
end

module Dummy : S with type t = unit
