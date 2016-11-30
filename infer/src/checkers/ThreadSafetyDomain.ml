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
    let compare = AccessPath.compare_raw
    let pp_element = AccessPath.pp_raw
  end)

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain = AbstractDomain.BooleanAnd

module PathDomain =  AbstractDomain.FiniteSet(PPrawpath)

module ReadWriteDomain = AbstractDomain.Pair (PathDomain) (PathDomain)

include AbstractDomain.Pair (LocksDomain) (ReadWriteDomain)
(* This is the ThreadSafety abstract domain *)
(* a typical element is (){locked}, {vars and fields})  *)
