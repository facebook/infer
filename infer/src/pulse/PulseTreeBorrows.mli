(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue

(** Abstract state of the Tree Borrows checker. *)

module Operand : sig
  (** how a value is designated in an instruction: through a temporary identifier and/or an access
      path of memory cells *)
  type t

  val untracked : t

  val of_place : AbstractValue.t -> t

  val of_temp : Ident.t -> AbstractValue.t option -> t

  val extend : t -> AbstractValue.t -> t

  val last_cell : t -> AbstractValue.t option
end

type state [@@deriving compare, equal]

val start : unit -> state

val pp : F.formatter -> state -> unit

val exec_retag :
     dst:Operand.t
  -> src:Operand.t
  -> is_mut:bool
  -> protected:bool
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val exec_load :
     id:Ident.t
  -> typ:Typ.t
  -> src:Operand.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val exec_store :
     lhs:Operand.t
  -> rhs:Operand.t
  -> typ:Typ.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val canonicalize : f:(AbstractValue.t -> AbstractValue.t) -> state -> state

val init_formals :
     (Pvar.t * Typ.t) list
  -> cell_of:(Pvar.t -> AbstractValue.t option)
  -> borrowed_cell_of:(Pvar.t -> AbstractValue.t option)
  -> tree_borrows:Specialization.Pulse.TreeBorrows.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> state
  -> state

val entry_pre : state -> Specialization.Pulse.TreeBorrows.t

val precondition_of_actuals : state -> Operand.t list -> Specialization.Pulse.TreeBorrows.t

val perm_spec_needed : formals:(Pvar.t * Typ.t) list -> Specialization.Pulse.TreeBorrows.t -> bool

val exec_call :
     callee_state:state
  -> callee_pdesc:Procdesc.t
  -> subst:(AbstractValue.t -> AbstractValue.t option)
  -> callee_edges:(AbstractValue.t * AbstractValue.t) list
  -> callee_ret_cell:AbstractValue.t option
  -> args:Operand.t list
  -> ret_id:Ident.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val report_errors : Procdesc.t -> Errlog.t -> state -> unit
