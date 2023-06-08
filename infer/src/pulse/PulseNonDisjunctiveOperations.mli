(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

val has_copy_in : string -> bool
(** Check if the string includes "copy" in case-insensitive *)

val init_const_refable_parameters :
     Procdesc.t
  -> IntegerWidths.t
  -> Tenv.t
  -> ExecutionDomain.t list
  -> NonDisjDomain.t
  -> NonDisjDomain.t
(** Initialize candidate parameters for const refable in the non-disj domain *)

val call :
     IntegerWidths.t
  -> Tenv.t
  -> Procdesc.t
  -> PathContext.t
  -> Location.t
  -> call_exp:Exp.t
  -> actuals:(Exp.t * Typ.t) list
  -> astates_before:AbductiveDomain.t list
  -> ExecutionDomain.t list
  -> NonDisjDomain.t
  -> NonDisjDomain.t * ExecutionDomain.t list
(** Abstract semantics for a funcation call *)

val mark_modified_copies_and_parameters :
  Var.t list -> ExecutionDomain.t list -> NonDisjDomain.t -> NonDisjDomain.t
(** Mark if copies and parameters has been modified so far *)

val mark_modified_copies_and_parameters_on_abductive :
  Var.t list -> AbductiveDomain.t -> NonDisjDomain.t -> NonDisjDomain.t
(** Similar to [mark_modified_copies_and_parameters], but given by [PulseAbductiveDomain.t] instead *)
