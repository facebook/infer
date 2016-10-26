(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format
module P = Printf

(** Module for typestates: maps from expressions to annotated types, with extensions. *)

(** Parameters of a call. *)
type parameters = (Exp.t * Typ.t) list

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

(** Extension to a typestate with values of type 'a. *)
type 'a ext =
  {
    empty : 'a; (** empty extension *)
    check_instr :
      Tenv.t -> get_proc_desc -> Procname.t ->
      Cfg.Procdesc.t -> 'a -> Sil.instr -> parameters ->
      'a; (** check the extension for an instruction *)
    join : 'a -> 'a -> 'a; (** join two extensions *)
    pp : Format.formatter -> 'a -> unit (** pretty print an extension *)
  }


module M = Map.Make (struct
    type t = Exp.t
    let compare = Exp.compare end)

type range = Typ.t * TypeAnnotation.t * (Location.t list)

type 'a t =
  {
    map: range M.t;
    extension : 'a;
  }

let empty ext =
  {
    map = M.empty;
    extension = ext.empty;
  }

let locs_compare = IList.compare Location.compare
let locs_equal locs1 locs2 = locs_compare locs1 locs2 = 0

let range_equal (typ1, ta1, locs1) (typ2, ta2, locs2) =
  Typ.equal typ1 typ2 && TypeAnnotation.equal ta1 ta2 && locs_equal locs1 locs2

let equal t1 t2 =
  (* Ignore the calls field, which is a pure instrumentation *)
  M.equal range_equal t1.map t2.map

let pp ext fmt typestate =
  let pp_loc fmt loc = F.fprintf fmt "%d" loc.Location.line in
  let pp_locs fmt locs = F.fprintf fmt " [%a]" (pp_seq pp_loc) locs in
  let pp_one exp (typ, ta, locs) =
    F.fprintf fmt "  %a -> [%s] %s %a%a@\n"
      Exp.pp exp
      (TypeOrigin.to_string (TypeAnnotation.get_origin ta)) (TypeAnnotation.to_string ta)
      (Typ.pp_full pe_text) typ
      pp_locs locs in
  let pp_map map = M.iter pp_one map in

  pp_map typestate.map;
  ext.pp fmt typestate.extension

let type_join typ1 typ2 =
  if PatternMatch.type_is_object typ1 then typ2 else typ1
let locs_join locs1 locs2 =
  IList.merge_sorted_nodup Location.compare [] locs1 locs2

(** Add a list of locations to a range. *)
let range_add_locs (typ, ta, locs1) locs2 =
  let locs' = locs_join locs1 locs2 in
  (typ, ta, locs')

(** Join m2 to m1 if there are no inconsistencies, otherwise return m1. *)
let map_join m1 m2 =
  let tjoined = ref m1 in
  let range_join (typ1, ta1, locs1) (typ2, ta2, locs2) =
    match TypeAnnotation.join ta1 ta2 with
    | None -> None
    | Some ta' ->
        let typ' = type_join typ1 typ2 in
        let locs' = locs_join locs1 locs2 in
        Some (typ', ta', locs') in
  let extend_lhs exp2 range2 = (* extend lhs if possible, otherwise return false *)
    try
      let range1 = M.find exp2 m1 in
      (match range_join range1 range2 with
       | None -> ()
       | Some range' -> tjoined := M.add exp2 range' !tjoined)
    with Not_found ->
      tjoined := M.add exp2 range2 !tjoined in
  let missing_rhs exp1 range1 = (* handle elements missing in the rhs *)
    try
      ignore (M.find exp1 m2)
    with Not_found ->
      let (t1, ta1, locs1) = range1 in
      let range1' =
        let ta1' = TypeAnnotation.with_origin ta1 TypeOrigin.Undef in
        (t1, ta1', locs1) in
      tjoined := M.add exp1 range1' !tjoined in
  if m1 == m2 then m1
  else (
    M.iter extend_lhs m2;
    M.iter missing_rhs m1;
    !tjoined
  )

let join ext t1 t2 =
  if Config.eradicate_trace
  then L.stderr "@.@.**********join@.-------@.%a@.------@.%a@.********@.@."
      (pp ext) t1
      (pp ext) t2;
  {
    map = map_join t1.map t2.map;
    extension = ext.join t1.extension t2.extension;
  }

let lookup_id id typestate =
  try Some (M.find (Exp.Var id) typestate.map)
  with Not_found -> None

let lookup_pvar pvar typestate =
  try Some (M.find (Exp.Lvar pvar) typestate.map)
  with Not_found -> None

let add_id id range typestate =
  let map' = M.add (Exp.Var id) range typestate.map in
  if map' == typestate.map then typestate
  else { typestate with map = map' }

let add pvar range typestate =
  let map' = M.add (Exp.Lvar pvar) range typestate.map in
  if map' == typestate.map then typestate
  else { typestate with map = map' }

let remove_id id typestate =
  let map' = M.remove (Exp.Var id) typestate.map in
  if map' == typestate.map then typestate
  else { typestate with map = map' }

let get_extension typestate = typestate.extension

let set_extension typestate extension =
  { typestate with extension = extension }
