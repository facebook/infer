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

  val to_callee : t -> CallSite.t -> t

  (** ith param * ith source kind *)
  val get : CallSite.t -> (int * t) list
end
