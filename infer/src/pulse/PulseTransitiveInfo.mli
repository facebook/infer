(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Callees : sig
  (** for each call site, we remember which resolution was performed *)

  include AbstractDomain.WithBottom

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  type call_kind = Static | Virtual | Closure

  type resolution = ResolvedUsingDynamicType | ResolvedUsingStaticType | Unresolved

  val record :
       caller_name:string
    -> caller_loc:Location.t
    -> callsite_loc:Location.t
    -> call_kind
    -> resolution
    -> t
    -> t

  type item =
    { callsite_loc: Location.t
    ; caller_name: string
    ; caller_loc: Location.t
    ; kind: call_kind
    ; resolution: resolution }

  val report_as_extra_info : t -> item list
end
