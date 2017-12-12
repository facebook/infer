(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type retain_cycle_node = {rc_node_exp: Exp.t; rc_node_typ: Typ.t}

type retain_cycle_field_objc = {rc_field_name: Typ.Fieldname.t; rc_field_inst: Sil.inst}

type retain_cycle_edge = {rc_from: retain_cycle_node; rc_field: retain_cycle_field_objc}

(** A retain cycle is a non-empty list of paths. It also contains a pointer to the head of the list 
to model the cycle structure. The next element from the end of the list is the head. *)
type t = {rc_elements: retain_cycle_edge list; rc_head: retain_cycle_edge}

val retain_cycle_to_string : t -> string

val print_cycle : t -> unit

val create_cycle : retain_cycle_edge list -> t option
(** Creates a cycle if the list is non-empty *)
