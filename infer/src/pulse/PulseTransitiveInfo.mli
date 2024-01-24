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

type t =
  { accesses: PulseTrace.Set.t  (** record specific accesses inter-procedurally *)
  ; callees: Callees.t  (** record all call resolutions that were transitively performed *)
  ; missed_captures: Typ.Name.Set.t
        (** record types that were missing during name resolution (fields/methods) while analysing
            this function and its transitive callees *) }
[@@deriving compare, equal]

include AbstractDomain.WithBottom with type t := t

val apply_summary : callee_pname:Procname.t -> call_loc:Location.t -> summary:t -> t -> t

val remember_dropped_elements : dropped:t -> t -> t

val transfer_transitive_info_to_caller :
  caller:t -> Procname.t -> Location.t -> callee_summary:t -> t
