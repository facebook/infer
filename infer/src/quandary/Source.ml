(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module type S = sig
  include TraceElem.S

  val is_footprint : t -> bool

  val make_footprint : AccessPath.t -> CallSite.t -> t

  val get_footprint_access_path: t -> AccessPath.t option

  val to_return : t -> CallSite.t -> t

  (** ith return value * ith sink kind *)
  val get : CallSite.t -> (int * t) list
end
