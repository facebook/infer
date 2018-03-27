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

type retain_cycle_edge =
  | Object of retain_cycle_edge_objc
  | Block of Typ.Procname.t * Pvar.t
  [@@deriving compare]

let retain_cycle_edge_equal = [%compare.equal : retain_cycle_edge]

type t = {rc_head: retain_cycle_edge; rc_elements: retain_cycle_edge list} [@@deriving compare]

module Set = Caml.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

let is_inst_rearrange node =
  match node with
  | Object obj -> (
    match obj.rc_field.rc_field_inst with Sil.Irearrange _ -> true | _ -> false )
  | Block _ ->
      false


let _retain_cycle_node_to_string (node: retain_cycle_node) =
  Format.sprintf "%s : %s" (Exp.to_string node.rc_node_exp) (Typ.to_string node.rc_node_typ)


let retain_cycle_field_to_string (field: retain_cycle_field_objc) =
  Format.sprintf "%s[%s]"
    (Typ.Fieldname.to_string field.rc_field_name)
    (Sil.inst_to_string field.rc_field_inst)


let _retain_cycle_edge_to_string (edge: retain_cycle_edge) =
  match edge with
  | Object obj ->
      Format.sprintf "%s ->{%s}"
        (_retain_cycle_node_to_string obj.rc_from)
        (retain_cycle_field_to_string obj.rc_field)
  | Block _ ->
      Format.sprintf "a block"


let _retain_cycle_to_string cycle =
  "Cycle= \n\t"
  ^ String.concat ~sep:"->" (List.map ~f:_retain_cycle_edge_to_string cycle.rc_elements)


let print_cycle cycle = Logging.d_strln (_retain_cycle_to_string cycle)

let find_minimum_element cycle =
  List.reduce_exn cycle.rc_elements ~f:(fun el1 el2 ->
      if compare_retain_cycle_edge el1 el2 < 0 then el1 else el2 )


let shift cycle head : t =
  let rec shift_elements rev_tail elements =
    match elements with
    | hd :: rest when not (retain_cycle_edge_equal hd head) ->
        shift_elements (hd :: rev_tail) rest
    | _ ->
        elements @ List.rev rev_tail
  in
  let new_elements = shift_elements [] cycle.rc_elements in
  {rc_elements= new_elements; rc_head= List.hd_exn new_elements}


let normalize_cycle cycle =
  let min = find_minimum_element cycle in
  shift cycle min


let create_cycle cycle =
  match cycle with
  | [hd] ->
      if is_inst_rearrange hd then (* cycles of length 1 created at rearrange are not real *)
        None
      else Some (normalize_cycle {rc_elements= cycle; rc_head= hd})
  | hd :: _ ->
      Some (normalize_cycle {rc_elements= cycle; rc_head= hd})
  | [] ->
      None


let pp_dotty fmt cycle =
  let pp_dotty_obj fmt element =
    match element with
    | Object obj ->
        Format.fprintf fmt "Object: %s" (Typ.to_string obj.rc_from.rc_node_typ)
    | Block _ ->
        Format.fprintf fmt "Block"
  in
  let pp_dotty_id fmt element =
    match element with
    | Object obj ->
        Format.fprintf fmt "%s_%a"
          (Typ.to_string obj.rc_from.rc_node_typ)
          Typ.Fieldname.pp obj.rc_field.rc_field_name
    | Block (name, _) ->
        Format.fprintf fmt "%s" (Typ.Procname.to_unique_id name)
  in
  let pp_dotty_field fmt element =
    match element with
    | Object obj ->
        Typ.Fieldname.pp fmt obj.rc_field.rc_field_name
    | Block _ ->
        Format.fprintf fmt ""
  in
  let pp_dotty_element fmt element =
    Format.fprintf fmt "\t%a [label = \"%a | %a \"]@\n" pp_dotty_id element pp_dotty_obj element
      pp_dotty_field element
  in
  let rec pp_dotty_edges fmt edges =
    match edges with
    | edge1 :: edge2 :: rest ->
        Format.fprintf fmt "\t%a -> %a [color=\"blue\"];@\n" pp_dotty_id edge1 pp_dotty_id edge2 ;
        pp_dotty_edges fmt (edge2 :: rest)
    | [edge] ->
        Format.fprintf fmt "\t%a -> %a [color=\"blue\"];@\n" pp_dotty_id edge pp_dotty_id
          cycle.rc_head
    | [] ->
        ()
  in
  Format.fprintf fmt "@\n@\n@\ndigraph main { @\n\tnode [shape=record]; @\n" ;
  Format.fprintf fmt "@\n\trankdir =LR; @\n" ;
  Format.fprintf fmt "@\n" ;
  List.iter ~f:(pp_dotty_element fmt) cycle.rc_elements ;
  Format.fprintf fmt "@\n" ;
  pp_dotty_edges fmt cycle.rc_elements ;
  Format.fprintf fmt "@\n}"


let write_dotty_to_file fname cycle =
  let chan = Out_channel.create fname in
  let fmt = Format.formatter_of_out_channel chan in
  pp_dotty fmt cycle ; Out_channel.close chan
