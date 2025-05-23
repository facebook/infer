(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** State of symbolic execution *)

val get_node : unit -> Procdesc.Node.t option
(** Get last node seen in symbolic execution *)

val get_remaining_disjuncts : unit -> int option
(** Get number of remaining disjuncts in the transfer function for Pulse *)

val set_instr : Sil.instr -> unit
(** Set last instruction seen in symbolic execution *)

val set_node : Procdesc.Node.t -> unit
(** Set last node seen in symbolic execution *)

val set_remaining_disjuncts : int -> unit
(** Set number of remaining disjuncts in the transfer function for Pulse *)

val set_session : int -> unit
(** Set last session seen in symbolic execution *)
