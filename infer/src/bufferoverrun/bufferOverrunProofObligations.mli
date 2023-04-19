(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module ItvPure = Itv.ItvPure

module Condition : sig
  type t
end

module ConditionTrace : sig
  type t

  val get_report_location : t -> Location.t

  val get_val_traces : t -> BufferOverrunTrace.Issue.t
end

module ConditionSet : sig
  type checked_t

  type summary_t

  val empty : checked_t

  val pp : Format.formatter -> checked_t -> unit

  val pp_summary : Format.formatter -> summary_t -> unit

  val add_array_access :
       Location.t
    -> offset:ItvPure.t
    -> idx:ItvPure.t
    -> size:ItvPure.t
    -> last_included:bool
    -> idx_traces:BufferOverrunTrace.Set.t
    -> arr_traces:BufferOverrunTrace.Set.t
    -> latest_prune:BufferOverrunDomain.LatestPrune.t
    -> checked_t
    -> checked_t

  val add_alloc_size :
       Location.t
    -> can_be_zero:bool
    -> length:ItvPure.t
    -> BufferOverrunTrace.Set.t
    -> BufferOverrunDomain.LatestPrune.t
    -> checked_t
    -> checked_t

  val add_binary_operation :
       IntegerWidths.t
    -> Location.t
    -> Procname.t
    -> Binop.t
    -> lhs:ItvPure.t
    -> rhs:ItvPure.t
    -> lhs_traces:BufferOverrunTrace.Set.t
    -> rhs_traces:BufferOverrunTrace.Set.t
    -> latest_prune:BufferOverrunDomain.LatestPrune.t
    -> checked_t
    -> checked_t

  val join : checked_t -> checked_t -> checked_t

  val subst :
       summary_t
    -> (mode:BufferOverrunSemantics.eval_mode -> BufferOverrunDomain.eval_sym_trace)
    -> Procname.t
    -> Location.t
    -> BufferOverrunDomain.LatestPrune.t
    -> checked_t

  val report_errors :
    report:(Condition.t -> ConditionTrace.t -> IssueType.t -> unit) -> checked_t -> unit

  val for_summary : checked_t -> summary_t
end

val description : markup:bool -> Condition.t -> ConditionTrace.t -> string
