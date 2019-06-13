(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** mapping of ids to raw access paths. useful for id-normalizing access paths *)

include AbstractDomain.MapS with type key = Var.t and type value = HilExp.AccessExpression.t
