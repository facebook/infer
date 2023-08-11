(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val initial : t

val acquire_resource : Exp.t * Typ.t -> t -> t

val release_resource : Exp.t * Typ.t -> t -> t

val load : Ident.t * Typ.t -> Exp.t -> t -> t

val store : lhs:Exp.t -> rhs:Exp.t * Typ.t -> t -> t

val has_leak : FormalMap.t -> t -> bool

type summary

module Summary : sig
  val apply : callee:summary -> return:Ident.t * Typ.t -> actuals:(Exp.t * Typ.t) list -> t -> t

  val make : FormalMap.t -> t -> summary

  val pp : Format.formatter -> summary -> unit

  type t = summary
end
