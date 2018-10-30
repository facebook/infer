(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type retain_cycle_node = {rc_node_exp: Exp.t; rc_node_typ: Typ.t}

type retain_cycle_field = {rc_field_name: Typ.Fieldname.t; rc_field_inst: Sil.inst}

type retain_cycle_edge_obj = {rc_from: retain_cycle_node; rc_field: retain_cycle_field}

type retain_cycle_edge = Object of retain_cycle_edge_obj | Block of Typ.Procname.t * Pvar.t

(** A retain cycle is a non-empty list of paths. It also contains a pointer to the head of the list
to model the cycle structure. The next element from the end of the list is the head. *)
type t = {rc_head: retain_cycle_edge; rc_elements: retain_cycle_edge list}

(** Set for retain cycles. *)
module Set : Caml.Set.S with type elt = t

val d_retain_cycle : t -> unit

val create_cycle : retain_cycle_edge list -> t option
(** Creates a cycle if the list is non-empty *)

val pp_dotty : Format.formatter -> t -> unit

val write_dotty_to_file : string -> t -> unit
