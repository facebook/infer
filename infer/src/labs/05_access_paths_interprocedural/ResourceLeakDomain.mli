(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val initial : t

val acquire_resource : AccessPath.t -> t -> t

val release_resource : AccessPath.t -> t -> t

val assign : AccessPath.t -> AccessPath.t -> t -> t

val has_leak : FormalMap.t -> t -> bool

type summary

module Summary : sig
  val apply : summary:summary -> return:AccessPath.base -> actuals:HilExp.t list -> t -> t

  val make : FormalMap.t -> t -> summary

  val pp : Format.formatter -> summary -> unit
end
