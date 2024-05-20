(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface

type trace = WrittenTo of Trace.t | Invalid of Invalidation.t * Trace.t

module ModifiedAccess : sig
  type t =
    { ordered_access_list: unit Access.access list
          (** list of ordered accesses that are oblivious to modified array indices *)
    ; trace: trace }
end

module ModifiedVarMap : sig
  type t

  val bottom : t

  val add : Pvar.t -> ModifiedAccess.t -> t -> t

  val fold : (Pvar.t -> ModifiedAccess.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Exited = AbstractDomain.BooleanOr

type t =
  { modified_params: ModifiedVarMap.t
  ; modified_globals: ModifiedVarMap.t
  ; skipped_calls: SkippedCalls.t
  ; exited: Exited.t }

val pure : t

val is_pure : t -> bool

type param_source = Formal | Global

val add_to_errlog :
     nesting:int
  -> param_source
  -> Pvar.t
  -> ModifiedAccess.t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list

val join : t -> t -> t

val get_modified_immutables_opt : Tenv.t -> t -> (ModifiedVarMap.t * ModifiedVarMap.t) option
