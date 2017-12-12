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

type t = {rc_elements: retain_cycle_edge list; rc_head: retain_cycle_edge}

let create_cycle cycle =
  match cycle with hd :: _ -> Some {rc_elements= cycle; rc_head= hd} | [] -> None


let retain_cycle_node_to_string (node: retain_cycle_node) =
  Format.sprintf "%s : %s" (Exp.to_string node.rc_node_exp) (Typ.to_string node.rc_node_typ)


let retain_cycle_field_to_string (field: retain_cycle_field_objc) =
  Format.sprintf "%s[%s]"
    (Typ.Fieldname.to_string field.rc_field_name)
    (Sil.inst_to_string field.rc_field_inst)


let retain_cycle_edge_to_string (edge: retain_cycle_edge) =
  Format.sprintf "%s ->{%s}"
    (retain_cycle_node_to_string edge.rc_from)
    (retain_cycle_field_to_string edge.rc_field)


let retain_cycle_to_string cycle =
  "Cycle= \n\t"
  ^ String.concat ~sep:"->" (List.map ~f:retain_cycle_edge_to_string cycle.rc_elements)


let print_cycle cycle = Logging.d_strln (retain_cycle_to_string cycle)
