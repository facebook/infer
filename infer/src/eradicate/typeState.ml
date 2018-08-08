(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** Module for typestates: maps from expressions to annotated types, with extensions. *)

(** Parameters of a call. *)
type parameters = (Exp.t * Typ.t) list

(** Extension to a typestate with values of type 'a. *)
type 'a ext =
  { empty: 'a  (** empty extension *)
  ; check_instr: Tenv.t -> Typ.Procname.t -> Procdesc.t -> 'a -> Sil.instr -> parameters -> 'a
        (** check the extension for an instruction *)
  ; join: 'a -> 'a -> 'a  (** join two extensions *)
  ; pp: Format.formatter -> 'a -> unit  (** pretty print an extension *) }

let unit_ext : unit ext =
  {empty= (); check_instr= (fun _ _ _ () _ _ -> ()); join= (fun () () -> ()); pp= (fun _ () -> ())}


module M = Caml.Map.Make (struct
  type t = Exp.t

  let compare = Exp.compare
end)

type range = Typ.t * TypeAnnotation.t * Location.t list [@@deriving compare]

type 'a t = {map: range M.t; extension: 'a} [@@deriving compare]

(* Ignore the extension field, which is a pure instrumentation *)
let compare t1 t2 = compare (fun _ _ -> 0) t1 t2

let equal t1 t2 = Int.equal (compare t1 t2) 0

let empty ext = {map= M.empty; extension= ext.empty}

let pp ext fmt typestate =
  let pp_loc fmt loc = F.pp_print_int fmt loc.Location.line in
  let pp_locs fmt locs = F.fprintf fmt " [%a]" (Pp.seq pp_loc) locs in
  let pp_one exp (typ, ta, locs) =
    F.fprintf fmt "  %a -> [%s] %s %a%a@\n" Exp.pp exp
      (TypeOrigin.to_string (TypeAnnotation.get_origin ta))
      (TypeAnnotation.to_string ta) (Typ.pp_full Pp.text) typ pp_locs locs
  in
  let pp_map map = M.iter pp_one map in
  pp_map typestate.map ; ext.pp fmt typestate.extension


let type_join typ1 typ2 = if PatternMatch.type_is_object typ1 then typ2 else typ1

let locs_join locs1 locs2 = IList.merge_sorted_nodup ~cmp:Location.compare ~res:[] locs1 locs2

(** Add a list of locations to a range. *)
let range_add_locs (typ, ta, locs1) locs2 =
  let locs' = locs_join locs1 locs2 in
  (typ, ta, locs')


(** Only keep variables if they are present on both sides of the join. *)
let only_keep_intersection = true

(** Join two maps.
    If only_keep_intersection is true, keep only variables present on both sides. *)
let map_join m1 m2 =
  let tjoined = ref (if only_keep_intersection then M.empty else m1) in
  let range_join (typ1, ta1, locs1) (typ2, ta2, locs2) =
    match TypeAnnotation.join ta1 ta2 with
    | None ->
        None
    | Some ta' ->
        let typ' = type_join typ1 typ2 in
        let locs' = locs_join locs1 locs2 in
        Some (typ', ta', locs')
  in
  let extend_lhs exp2 range2 =
    (* extend lhs if possible, otherwise return false *)
    try
      let range1 = M.find exp2 m1 in
      match range_join range1 range2 with
      | None ->
          if only_keep_intersection then tjoined := M.add exp2 range1 !tjoined
      | Some range' ->
          tjoined := M.add exp2 range' !tjoined
    with Caml.Not_found -> if not only_keep_intersection then tjoined := M.add exp2 range2 !tjoined
  in
  let missing_rhs exp1 range1 =
    (* handle elements missing in the rhs *)
    try ignore (M.find exp1 m2) with Caml.Not_found ->
      let t1, ta1, locs1 = range1 in
      let range1' =
        let ta1' = TypeAnnotation.with_origin ta1 TypeOrigin.Undef in
        (t1, ta1', locs1)
      in
      if not only_keep_intersection then tjoined := M.add exp1 range1' !tjoined
  in
  if phys_equal m1 m2 then m1 else ( M.iter extend_lhs m2 ; M.iter missing_rhs m1 ; !tjoined )


let join ext t1 t2 =
  let tjoin = {map= map_join t1.map t2.map; extension= ext.join t1.extension t2.extension} in
  ( if Config.write_html then
      let s =
        F.asprintf "State 1:@.%a@.State 2:@.%a@.After Join:@.%a@." (pp ext) t1 (pp ext) t2 (pp ext)
          tjoin
      in
      L.d_strln s ) ;
  tjoin


let lookup_id id typestate = M.find_opt (Exp.Var id) typestate.map

let lookup_pvar pvar typestate = M.find_opt (Exp.Lvar pvar) typestate.map

let add_id id range typestate =
  let map' = M.add (Exp.Var id) range typestate.map in
  if phys_equal map' typestate.map then typestate else {typestate with map= map'}


let add pvar range typestate =
  let map' = M.add (Exp.Lvar pvar) range typestate.map in
  if phys_equal map' typestate.map then typestate else {typestate with map= map'}


let remove_id id typestate =
  let map' = M.remove (Exp.Var id) typestate.map in
  if phys_equal map' typestate.map then typestate else {typestate with map= map'}


let get_extension typestate = typestate.extension

let set_extension typestate extension = {typestate with extension}
