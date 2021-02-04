(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val initial : t

module LeakList : sig
  include module type of Base.List

  val append_one : 'a list -> 'a -> 'a list
end

val check_count : AccessPath.t -> t -> bool

val get_type_map : (AccessPath.t, string) Caml.Hashtbl.t

val reset_type_map : unit

val acquire_resource : AccessPath.t -> string -> t -> t

val release_resource : AccessPath.t -> t -> t

val assign : AccessPath.t -> AccessPath.t -> t -> t

val has_leak : FormalMap.t -> t -> bool

type summary

module Summary : sig
  val apply : callee:summary -> return:AccessPath.base -> actuals:HilExp.t list -> t -> t

  val reset_interface_type_map : unit

  val make : FormalMap.t -> t -> summary

  val pp : Format.formatter -> summary -> unit

  type t = summary
end
