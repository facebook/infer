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

module Dummy = struct
  type t = unit [@@deriving compare]

  let call_site _ = CallSite.dummy

  let kind t = t

  let make kind _ = kind

  let pp _ () = ()

  let is_footprint _ = false

  let make_footprint _ _ = assert false
  let get_footprint_access_path _ = assert false
  let get _ = None

  module Kind = struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)

  let with_callsite t _ = t
end
