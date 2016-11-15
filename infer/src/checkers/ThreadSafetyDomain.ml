(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
module PPrawpath = PrettyPrintable.MakePPSet(struct
    type t = AccessPath.raw
    let compare = AccessPath.raw_compare
    let pp_element = AccessPath.pp_raw
  end)

module LocksDomain =  AbstractDomain.FiniteSet(Utils.StringPPSet)

module PathDomain =  AbstractDomain.FiniteSet(PPrawpath)

module ReadWriteDomain = AbstractDomain.Pair (PathDomain) (PathDomain)

include AbstractDomain.Pair (LocksDomain) (ReadWriteDomain)
(* This is the ThreadSafety abstract domain *)
(* a typical element is (){locked}, {vars and fields})  *)
