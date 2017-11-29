(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

module CallWithReceiver = struct
  type t = {receiver: AccessPath.t; procname: Typ.Procname.t} [@@deriving compare]

  let pp fmt {receiver; procname} =
    F.fprintf fmt "%a.%a" AccessPath.pp receiver Typ.Procname.pp procname

end

module CallSet = AbstractDomain.FiniteSet (CallWithReceiver)
include AbstractDomain.Map (AccessPath) (CallSet)
