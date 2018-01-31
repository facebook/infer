(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

type retain_cycle_node = {rc_node_exp: Exp.t; rc_node_typ: Typ.t} [@@deriving compare]

type retain_cycle_field_objc =
  {rc_field_name: Typ.Fieldname.t; rc_field_inst: Sil.inst}
  [@@deriving compare]

type retain_cycle_edge_objc =
  {rc_from: retain_cycle_node; rc_field: retain_cycle_field_objc}
  [@@deriving compare]

type retain_cycle_edge = Object of retain_cycle_edge_objc | Block [@@deriving compare]

type t = {rc_elements: retain_cycle_edge list; rc_head: retain_cycle_edge} [@@deriving compare]

let is_inst_rearrange node =
  match node with
  | Object obj -> (
    match obj.rc_field.rc_field_inst with Sil.Irearrange _ -> true | _ -> false )
  | Block ->
      false


let create_cycle cycle =
  let sorted_cycle = List.sort ~cmp:compare_retain_cycle_edge cycle in
  match sorted_cycle with
  | [hd] ->
      if is_inst_rearrange hd then (* cycles of length 1 created at rearrange are not real *)
        None
      else Some {rc_elements= sorted_cycle; rc_head= hd}
  | hd :: _ ->
      Some {rc_elements= sorted_cycle; rc_head= hd}
  | [] ->
      None


let retain_cycle_node_to_string (node: retain_cycle_node) =
  Format.sprintf "%s : %s" (Exp.to_string node.rc_node_exp) (Typ.to_string node.rc_node_typ)


let retain_cycle_field_to_string (field: retain_cycle_field_objc) =
  Format.sprintf "%s[%s]"
    (Typ.Fieldname.to_string field.rc_field_name)
    (Sil.inst_to_string field.rc_field_inst)


let retain_cycle_edge_to_string (edge: retain_cycle_edge) =
  match edge with
  | Object obj ->
      Format.sprintf "%s ->{%s}"
        (retain_cycle_node_to_string obj.rc_from)
        (retain_cycle_field_to_string obj.rc_field)
  | Block ->
      Format.sprintf "a block"


let retain_cycle_to_string cycle =
  "Cycle= \n\t"
  ^ String.concat ~sep:"->" (List.map ~f:retain_cycle_edge_to_string cycle.rc_elements)


let print_cycle cycle = Logging.d_strln (retain_cycle_to_string cycle)
