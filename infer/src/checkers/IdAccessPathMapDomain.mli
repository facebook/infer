(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** mapping of ids to raw access paths. useful for id-normalizing access paths *)

module IdMap = Var.Map

type astate = AccessExpression.t IdMap.t

include module type of IdMap

include AbstractDomain.WithBottom with type astate := astate
