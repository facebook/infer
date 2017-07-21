(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Propositions seen as graphs *)

module F = Format
module L = Logging

type t = Prop.normal Prop.t

type node = Exp.t

type sub_entry = Ident.t * Exp.t

type edge = Ehpred of Sil.hpred | Eatom of Sil.atom | Esub_entry of sub_entry

let from_prop p = p

(** Return [true] if root node *)
let rec is_root = function
  | Exp.Var id
   -> Ident.is_normal id
  | Exp.Exn _ | Exp.Closure _ | Exp.Const _ | Exp.Lvar _
   -> true
  | Exp.Cast (_, e)
   -> is_root e
  | Exp.UnOp _ | Exp.BinOp _ | Exp.Lfield _ | Exp.Lindex _ | Exp.Sizeof _
   -> false

(** Return [true] if the nodes are connected. Used to compute reachability. *)
let nodes_connected n1 n2 = Exp.equal n1 n2

(* Implemented as equality for now, later it might contain offset by a constant *)

(** Return [true] if the edge is an hpred, and [false] if it is an atom *)
let edge_is_hpred = function Ehpred _ -> true | Eatom _ -> false | Esub_entry _ -> false

(** Return the source of the edge *)
let edge_get_source = function
  | Ehpred Sil.Hpointsto (e, _, _)
   -> Some e
  | Ehpred Sil.Hlseg (_, _, e, _, _)
   -> Some e
  | Ehpred Sil.Hdllseg (_, _, e1, _, _, _, _)
   -> Some e1 (* only one direction supported for now *)
  | Eatom Sil.Aeq (e1, _)
   -> Some e1
  | Eatom Sil.Aneq (e1, _)
   -> Some e1
  | Eatom (Sil.Apred (_, e :: _) | Anpred (_, e :: _))
   -> Some e
  | Eatom (Sil.Apred (_, []) | Anpred (_, []))
   -> None
  | Esub_entry (x, _)
   -> Some (Exp.Var x)

(** Return the successor nodes of the edge *)
let edge_get_succs = function
  | Ehpred hpred
   -> Exp.Set.elements (Prop.hpred_get_targets hpred)
  | Eatom Sil.Aeq (_, e2)
   -> [e2]
  | Eatom Sil.Aneq (_, e2)
   -> [e2]
  | Eatom (Sil.Apred _ | Anpred _)
   -> []
  | Esub_entry (_, e)
   -> [e]

let get_sigma footprint_part g = if footprint_part then g.Prop.sigma_fp else g.Prop.sigma

let get_pi footprint_part g = if footprint_part then g.Prop.pi_fp else g.Prop.pi

let get_subl footprint_part g = if footprint_part then [] else Sil.sub_to_list g.Prop.sub

(** [edge_from_source g n footprint_part is_hpred] finds and edge with the given source [n] in prop [g].
    [footprint_part] indicates whether to search the edge in the footprint part, and [is_pred] whether it is an hpred edge. *)
let edge_from_source g n footprint_part is_hpred =
  let edges =
    if is_hpred then List.map ~f:(fun hpred -> Ehpred hpred) (get_sigma footprint_part g)
    else List.map ~f:(fun a -> Eatom a) (get_pi footprint_part g)
      @ List.map ~f:(fun entry -> Esub_entry entry) (get_subl footprint_part g)
  in
  let starts_from hpred =
    match edge_get_source hpred with Some e -> Exp.equal n e | None -> false
  in
  match List.filter ~f:starts_from edges with [] -> None | edge :: _ -> Some edge

(** [get_succs g n footprint_part is_hpred] returns the successor nodes of [n] in [g].
    [footprint_part] indicates whether to search the successors in the footprint part, and [is_pred] whether to follow hpred edges. *)
let get_succs g n footprint_part is_hpred =
  match edge_from_source g n footprint_part is_hpred with None -> [] | Some e -> edge_get_succs e

(** [get_edges footprint_part g] returns the list of edges in [g], in the footprint part if [fotprint_part] is true *)
let get_edges footprint_part g =
  let hpreds = get_sigma footprint_part g in
  let atoms = get_pi footprint_part g in
  let subst_entries = get_subl footprint_part g in
  List.map ~f:(fun hpred -> Ehpred hpred) hpreds @ List.map ~f:(fun a -> Eatom a) atoms
  @ List.map ~f:(fun entry -> Esub_entry entry) subst_entries

let edge_equal e1 e2 =
  match (e1, e2) with
  | Ehpred hp1, Ehpred hp2
   -> Sil.equal_hpred hp1 hp2
  | Eatom a1, Eatom a2
   -> Sil.equal_atom a1 a2
  | Esub_entry (x1, e1), Esub_entry (x2, e2)
   -> Ident.equal x1 x2 && Exp.equal e1 e2
  | _
   -> false

(** [contains_edge footprint_part g e] returns true if the graph [g] contains edge [e],
    searching the footprint part if [footprint_part] is true. *)
let contains_edge (footprint_part: bool) (g: t) (e: edge) =
  List.exists ~f:(fun e' -> edge_equal e e') (get_edges footprint_part g)

(** [iter_edges footprint_part f g] iterates function [f] on the edges in [g] in the same order as returned by [get_edges];
    if [footprint_part] is true the edges are taken from the footprint part. *)
let iter_edges footprint_part f g = List.iter ~f (get_edges footprint_part g)

(** Graph annotated with the differences w.r.t. a previous graph *)
type diff =
  { diff_newgraph: t  (** the new graph *)
  ; diff_changed_norm: Obj.t list  (** objects changed in the normal part *)
  ; diff_cmap_norm: Pp.colormap  (** colormap for the normal part *)
  ; diff_changed_foot: Obj.t list  (** objects changed in the footprint part *)
  ; diff_cmap_foot: Pp.colormap  (** colormap for the footprint part *) }

(** Compute the subobjects in [e2] which are different from those in [e1] *)
let compute_exp_diff (e1: Exp.t) (e2: Exp.t) : Obj.t list =
  if Exp.equal e1 e2 then [] else [Obj.repr e2]

(** Compute the subobjects in [se2] which are different from those in [se1] *)
let rec compute_sexp_diff (se1: Sil.strexp) (se2: Sil.strexp) : Obj.t list =
  match (se1, se2) with
  | Sil.Eexp (e1, _), Sil.Eexp (e2, _)
   -> if Exp.equal e1 e2 then [] else [Obj.repr se2]
  | Sil.Estruct (fsel1, _), Sil.Estruct (fsel2, _)
   -> compute_fsel_diff fsel1 fsel2
  | Sil.Earray (e1, esel1, _), Sil.Earray (e2, esel2, _)
   -> compute_exp_diff e1 e2 @ compute_esel_diff esel1 esel2
  | _
   -> [Obj.repr se2]

and compute_fsel_diff fsel1 fsel2 : Obj.t list =
  match (fsel1, fsel2) with
  | (f1, se1) :: fsel1', (f2, se2 as x) :: fsel2' -> (
    match Typ.Fieldname.compare f1 f2 with
    | n when n < 0
     -> compute_fsel_diff fsel1' fsel2
    | 0
     -> compute_sexp_diff se1 se2 @ compute_fsel_diff fsel1' fsel2'
    | _
     -> Obj.repr x :: compute_fsel_diff fsel1 fsel2' )
  | _, []
   -> []
  | [], x :: fsel2'
   -> Obj.repr x :: compute_fsel_diff [] fsel2'

and compute_esel_diff esel1 esel2 : Obj.t list =
  match (esel1, esel2) with
  | (e1, se1) :: esel1', (e2, se2 as x) :: esel2' -> (
    match Exp.compare e1 e2 with
    | n when n < 0
     -> compute_esel_diff esel1' esel2
    | 0
     -> compute_sexp_diff se1 se2 @ compute_esel_diff esel1' esel2'
    | _
     -> Obj.repr x :: compute_esel_diff esel1 esel2' )
  | _, []
   -> []
  | [], x :: esel2'
   -> Obj.repr x :: compute_esel_diff [] esel2'

(** Compute the subobjects in [newedge] which are different from those in [oldedge] *)
let compute_edge_diff (oldedge: edge) (newedge: edge) : Obj.t list =
  match (oldedge, newedge) with
  | Ehpred Sil.Hpointsto (_, se1, e1), Ehpred Sil.Hpointsto (_, se2, e2)
   -> compute_sexp_diff se1 se2 @ compute_exp_diff e1 e2
  | Eatom Sil.Aeq (_, e1), Eatom Sil.Aeq (_, e2)
   -> compute_exp_diff e1 e2
  | Eatom Sil.Aneq (_, e1), Eatom Sil.Aneq (_, e2)
   -> compute_exp_diff e1 e2
  | Eatom Sil.Apred (_, es1), Eatom Sil.Apred (_, es2)
  | Eatom Sil.Anpred (_, es1), Eatom Sil.Anpred (_, es2)
   -> List.concat
        ( try List.map2_exn ~f:compute_exp_diff es1 es2
          with Invalid_argument _ -> [] )
  | Esub_entry (_, e1), Esub_entry (_, e2)
   -> compute_exp_diff e1 e2
  | _
   -> [Obj.repr newedge]

(** [compute_diff oldgraph newgraph] returns the list of edges which are only in [newgraph] *)
let compute_diff default_color oldgraph newgraph : diff =
  let compute_changed footprint_part =
    let newedges = get_edges footprint_part newgraph in
    let changed = ref [] in
    let build_changed edge =
      if not (contains_edge footprint_part oldgraph edge) then
        match edge_get_source edge with
        | Some source -> (
          match edge_from_source oldgraph source footprint_part (edge_is_hpred edge) with
          | None
           -> let changed_obj =
                match edge with
                | Ehpred hpred
                 -> Obj.repr hpred
                | Eatom a
                 -> Obj.repr a
                | Esub_entry entry
                 -> Obj.repr entry
              in
              changed := changed_obj :: !changed
          | Some oldedge
           -> changed := compute_edge_diff oldedge edge @ !changed )
        | None
         -> ()
    in
    List.iter ~f:build_changed newedges ;
    let colormap (o: Obj.t) =
      if List.exists ~f:(fun x -> phys_equal x o) !changed then Pp.Red else default_color
    in
    (!changed, colormap)
  in
  let changed_norm, colormap_norm = compute_changed false in
  let changed_foot, colormap_foot = compute_changed true in
  { diff_newgraph= newgraph
  ; diff_changed_norm= changed_norm
  ; diff_cmap_norm= colormap_norm
  ; diff_changed_foot= changed_foot
  ; diff_cmap_foot= colormap_foot }

(** [diff_get_colormap footprint_part diff] returns the colormap of a computed diff,
    selecting the footprint colormap if [footprint_part] is true. *)
let diff_get_colormap footprint_part diff =
  if footprint_part then diff.diff_cmap_foot else diff.diff_cmap_norm

(** Print a list of propositions, prepending each one with the given string.
    If !Config.pring_using_diff is true, print the diff w.r.t. the given prop,
    extracting its local stack vars if the boolean is true. *)
let pp_proplist pe0 s (base_prop, extract_stack) f plist =
  let num = List.length plist in
  let base_stack = fst (Prop.sigma_get_stack_nonstack true base_prop.Prop.sigma) in
  let add_base_stack prop =
    if extract_stack then Prop.set prop ~sigma:(base_stack @ prop.Prop.sigma) else Prop.expose prop
  in
  let update_pe_diff (prop: Prop.normal Prop.t) : Pp.env =
    if Config.print_using_diff then
      let diff = compute_diff Blue (from_prop base_prop) (from_prop prop) in
      let cmap_norm = diff_get_colormap false diff in
      let cmap_foot = diff_get_colormap true diff in
      {pe0 with cmap_norm; cmap_foot}
    else pe0
  in
  let rec pp_seq_newline n f = function
    | []
     -> ()
    | [_x]
     -> (
        let pe = update_pe_diff _x in
        let x = add_base_stack _x in
        match pe.kind with
        | TEXT
         -> F.fprintf f "%s %d of %d:@\n%a" s n num (Prop.pp_prop pe) x
        | HTML
         -> F.fprintf f "%s %d of %d:@\n%a@\n" s n num (Prop.pp_prop pe) x
        | LATEX
         -> F.fprintf f "@[%a@]@\n" (Prop.pp_prop pe) x )
    | _x :: l
     -> let pe = update_pe_diff _x in
        let x = add_base_stack _x in
        match pe.kind with
        | TEXT
         -> F.fprintf f "%s %d of %d:@\n%a@\n%a" s n num (Prop.pp_prop pe) x
              (pp_seq_newline (n + 1))
              l
        | HTML
         -> F.fprintf f "%s %d of %d:@\n%a@\n%a" s n num (Prop.pp_prop pe) x
              (pp_seq_newline (n + 1))
              l
        | LATEX
         -> F.fprintf f "@[%a@]\\\\@\n\\bigvee\\\\@\n%a" (Prop.pp_prop pe) x
              (pp_seq_newline (n + 1))
              l
  in
  pp_seq_newline 1 f plist

(** dump a propset *)
let d_proplist (p: 'a Prop.t) (pl: 'b Prop.t list) =
  L.add_print_action (L.PTproplist, Obj.repr (p, pl))
