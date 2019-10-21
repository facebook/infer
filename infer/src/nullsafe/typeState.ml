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

type range = Typ.t * InferredNullability.t [@@deriving compare]

type t = range M.t [@@deriving compare]

let equal = [%compare.equal: t]

let empty = M.empty

let pp fmt typestate =
  let pp_one exp (typ, ta) =
    F.fprintf fmt "  %a -> [%s] %s %a@\n" Exp.pp exp
      (TypeOrigin.to_string (InferredNullability.get_origin ta))
      (InferredNullability.to_string ta)
      (Typ.pp_full Pp.text) typ
  in
  let pp_map map = M.iter pp_one map in
  pp_map typestate


let map_join m1 m2 =
  let range_join _exp range1_opt range2_opt =
    Option.both range1_opt range2_opt
    |> Option.map ~f:(fun ((typ1, inferred_nullability1), (_, inferred_nullability2)) ->
           (* Unlike nullability that Nullsafe infers, Java does not support local type inference
              (for codebases and Java version nullsafe is currently aimed for).
              The real type does not depend on types being joined; it is determined by the corresponding type declaration instead.
              We don't really use type information for reasons not related to things like diagnostic, and using one of types
              serves as a good proxy.
              Let's take the left one.
           *)
           let joined_type = typ1 in
           (joined_type, InferredNullability.join inferred_nullability1 inferred_nullability2) )
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
