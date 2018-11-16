(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module ItvPure = Itv.ItvPure
module Relation = BufferOverrunDomainRelation
module ValTraceSet = BufferOverrunTrace.Set

module Condition : sig
  type t
end

module ConditionTrace : sig
  type t

  val get_report_location : t -> Location.t

  val get_val_traces : t -> ValTraceSet.t
end

module ConditionSet : sig
  type t

  type summary_t

  val empty : t

  val pp : Format.formatter -> t -> unit

  val pp_summary : Format.formatter -> summary_t -> unit

  val add_array_access :
       Location.t
    -> offset:ItvPure.astate
    -> idx:ItvPure.astate
    -> size:ItvPure.astate
    -> is_collection_add:bool
    -> idx_sym_exp:Relation.SymExp.t option
    -> size_sym_exp:Relation.SymExp.t option
    -> relation:Relation.astate
    -> ValTraceSet.t
    -> t
    -> t

  val add_alloc_size : Location.t -> length:ItvPure.astate -> ValTraceSet.t -> t -> t

  val add_binary_operation :
       Typ.IntegerWidths.t
    -> Location.t
    -> Binop.t
    -> lhs:ItvPure.astate
    -> rhs:ItvPure.astate
    -> ValTraceSet.t
    -> t
    -> t

  val merge : t -> t -> t

  val subst :
       summary_t
    -> Bounds.Bound.eval_sym * (Symb.Symbol.t -> ValTraceSet.t)
    -> Relation.SubstMap.t
    -> Relation.astate
    -> Typ.Procname.t
    -> Location.t
    -> t

  val check_all : report:(Condition.t -> ConditionTrace.t -> IssueType.t -> unit) -> t -> t
  (** Check the conditions, call [report] on those that trigger an issue, returns those that needs to be propagated to callers. *)

  val for_summary : t -> summary_t

  val forget_locs : AbsLoc.PowLoc.t -> t -> t
end

val description : Condition.t -> ConditionTrace.t -> string
