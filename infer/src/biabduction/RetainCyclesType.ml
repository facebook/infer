(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type retain_cycle_node = {rc_node_exp: Exp.t; rc_node_typ: Typ.t}

type retain_cycle_field = {rc_field_name: Fieldname.t; rc_field_inst: Predicates.inst}

type retain_cycle_edge_obj = {rc_from: retain_cycle_node; rc_field: retain_cycle_field}

type retain_cycle_edge = Object of retain_cycle_edge_obj | Block of Procname.t * Pvar.t

type t = {rc_head: retain_cycle_edge; rc_elements: retain_cycle_edge list}

let compare_retain_cycle_node (node1 : retain_cycle_node) (node2 : retain_cycle_node) =
  Typ.compare node1.rc_node_typ node2.rc_node_typ


let compare_retain_cycle_field (node1 : retain_cycle_field) (node2 : retain_cycle_field) =
  Fieldname.compare node1.rc_field_name node2.rc_field_name


let compare_retain_cycle_edge_obj (obj1 : retain_cycle_edge_obj) (obj2 : retain_cycle_edge_obj) =
  let obj1_pair = Tuple.T2.create obj1.rc_from obj1.rc_field in
  let obj2_pair = Tuple.T2.create obj2.rc_from obj2.rc_field in
  Tuple.T2.compare ~cmp1:compare_retain_cycle_node ~cmp2:compare_retain_cycle_field obj1_pair
    obj2_pair


let compare_retain_cycle_edge (edge1 : retain_cycle_edge) (edge2 : retain_cycle_edge) =
  match (edge1, edge2) with
  | Object edge_obj1, Object edge_obj2 ->
      compare_retain_cycle_edge_obj edge_obj1 edge_obj2
  | Block (procname1, _), Block (procname2, _) ->
      Procname.compare procname1 procname2
  | Object _, Block _ ->
      1
  | Block _, Object _ ->
      -1


let equal_retain_cycle_edge = [%compare.equal: retain_cycle_edge]

let compare (rc1 : t) (rc2 : t) =
  List.compare compare_retain_cycle_edge rc1.rc_elements rc2.rc_elements


module Set = Caml.Set.Make (struct
  type nonrec t = t [@@deriving compare]
end)

let is_inst_rearrange node =
  match node with
  | Object obj -> (
    match obj.rc_field.rc_field_inst with Predicates.Irearrange _ -> true | _ -> false )
  | Block _ ->
      false


let is_isa_field node =
  match node with
  | Object obj ->
      String.equal (Fieldname.to_string obj.rc_field.rc_field_name) "isa"
  | Block _ ->
      false


let is_modelled_type node =
  let modelled_types =
    [ "NSArray"
    ; "NSData"
    ; "NSDictionary"
    ; "NSMutableArray"
    ; "NSMutableDictionary"
    ; "NSNumber"
    ; "NSString" ]
  in
  match node with
  | Object obj ->
      List.exists
        ~f:(fun model -> String.equal model (Typ.to_string obj.rc_from.rc_node_typ))
        modelled_types
  | Block _ ->
      false


let is_exp_null node =
  match node with Object obj -> Exp.is_null_literal obj.rc_from.rc_node_exp | Block _ -> false


let pp_retain_cycle_node f (node : retain_cycle_node) =
  Format.fprintf f "%a : %a" Exp.pp node.rc_node_exp (Typ.pp_full Pp.text) node.rc_node_typ


let pp_retain_cycle_field f (field : retain_cycle_field) =
  Format.fprintf f "%a[%a]" Fieldname.pp field.rc_field_name Predicates.pp_inst field.rc_field_inst


let pp_retain_cycle_edge f (edge : retain_cycle_edge) =
  match edge with
  | Object obj ->
      Format.fprintf f "%a ->{%a}" pp_retain_cycle_node obj.rc_from pp_retain_cycle_field
        obj.rc_field
  | Block _ ->
      Format.pp_print_string f "a block"


let d_retain_cycle cycle =
  Logging.d_strln "Cycle=" ;
  Logging.d_printfln "\t%a" (Pp.seq ~sep:"->" pp_retain_cycle_edge) cycle.rc_elements


let find_minimum_element cycle =
  List.reduce_exn cycle.rc_elements ~f:(fun el1 el2 ->
      if compare_retain_cycle_edge el1 el2 < 0 then el1 else el2 )


let shift cycle head : t =
  let rec shift_elements rev_tail elements =
    match elements with
    | hd :: rest when not (equal_retain_cycle_edge hd head) ->
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
  (*isa is an internal field not accessible or writable, so it doesn't make sense in a cycle *)
  if List.exists ~f:is_isa_field cycle then None
    (* The modelled types, where the models are meant to catch NPEs or Memory Leaks, include fields
       that don't necessarily reflect the real code, so potential retain cycles including them are
       probably wrong. *)
  else if List.exists ~f:is_modelled_type cycle then None
    (* There are some false positives where we report on null expressions, we can eliminate them here *)
  else if List.exists ~f:is_exp_null cycle then None
  else
    match cycle with
    | [hd] ->
        if is_inst_rearrange hd then None (* cycles of length 1 created at rearrange are not real *)
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
        Format.pp_print_string fmt "Block"
  in
  let pp_dotty_id fmt element =
    match element with
    | Object obj ->
        Format.fprintf fmt "%s_%a"
          (Typ.to_string obj.rc_from.rc_node_typ)
          Fieldname.pp obj.rc_field.rc_field_name
    | Block (name, _) ->
        Procname.pp_unique_id fmt name
  in
  let pp_dotty_field fmt element =
    match element with
    | Object obj ->
        Fieldname.pp fmt obj.rc_field.rc_field_name
    | Block _ ->
        Format.fprintf fmt ""
  in
  let pp_dotty_element fmt element =
    Format.fprintf fmt "\t\"%a\" [label = \"%a | %a \"]@\n" pp_dotty_id element pp_dotty_obj element
      pp_dotty_field element
  in
  let rec pp_dotty_edges fmt edges =
    match edges with
    | edge1 :: edge2 :: rest ->
        Format.fprintf fmt "\t\"%a\" -> \"%a\" [color=\"blue\"];@\n" pp_dotty_id edge1 pp_dotty_id
          edge2 ;
        pp_dotty_edges fmt (edge2 :: rest)
    | [edge] ->
        Format.fprintf fmt "\t\"%a\" -> \"%a\" [color=\"blue\"];@\n" pp_dotty_id edge pp_dotty_id
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
  pp_dotty fmt cycle ;
  Out_channel.close chan
