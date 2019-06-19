(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** Module for typestates: maps from expressions to annotated types, with extensions. *)

module M = Caml.Map.Make (struct
  type t = Exp.t

  let compare = Exp.compare
end)

type range = Typ.t * TypeAnnotation.t * Location.t list [@@deriving compare]

type t = range M.t [@@deriving compare]

let equal = [%compare.equal: t]

let empty = M.empty

let pp fmt typestate =
  let pp_loc fmt loc = F.pp_print_int fmt loc.Location.line in
  let pp_locs fmt locs = F.fprintf fmt " [%a]" (Pp.seq pp_loc) locs in
  let pp_one exp (typ, ta, locs) =
    F.fprintf fmt "  %a -> [%s] %s %a%a@\n" Exp.pp exp
      (TypeOrigin.to_string (TypeAnnotation.get_origin ta))
      (TypeAnnotation.to_string ta) (Typ.pp_full Pp.text) typ pp_locs locs
  in
  let pp_map map = M.iter pp_one map in
  pp_map typestate


let type_join typ1 typ2 = if PatternMatch.type_is_object typ1 then typ2 else typ1

let locs_join locs1 locs2 = IList.merge_sorted_nodup ~cmp:Location.compare ~res:[] locs1 locs2

(** Add a list of locations to a range. *)
let range_add_locs (typ, ta, locs1) locs2 =
  let locs' = locs_join locs1 locs2 in
  (typ, ta, locs')


let map_join m1 m2 =
  let range_join _exp range1_opt range2_opt =
    Option.both range1_opt range2_opt
    |> Option.map ~f:(fun (((typ1, ta1, locs1) as range1), (typ2, ta2, locs2)) ->
           TypeAnnotation.join ta1 ta2
           |> Option.value_map ~default:range1 ~f:(fun ta' ->
                  let typ' = type_join typ1 typ2 in
                  let locs' = locs_join locs1 locs2 in
                  (typ', ta', locs') ) )
  in
  if phys_equal m1 m2 then m1 else M.merge range_join m1 m2


let join t1 t2 =
  let tjoin = map_join t1 t2 in
  if Config.write_html then
    L.d_printfln "State 1:@.%a@.State 2:@.%a@.After Join:@.%a@." pp t1 pp t2 pp tjoin ;
  tjoin


let lookup_id id typestate = M.find_opt (Exp.Var id) typestate

let lookup_pvar pvar typestate = M.find_opt (Exp.Lvar pvar) typestate

let add_id id range typestate = M.add (Exp.Var id) range typestate

let add pvar range typestate = M.add (Exp.Lvar pvar) range typestate

let remove_id id typestate = M.remove (Exp.Var id) typestate
