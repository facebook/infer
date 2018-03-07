(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** mapping of ids to raw access paths. useful for id-normalizing access paths *)

module IdMap = Var.Map

type astate = AccessExpression.t IdMap.t

include module type of IdMap

include AbstractDomain.WithBottom with type astate := astate
