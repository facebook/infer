(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Environment for temporary identifiers used in instructions.
Lazy implementation: only created when actually used. *)


type t = (Sil.exp Ident.IdentHash.t) Lazy.t * Cfg.cfg

let _create cfg proc_desc =
  let map = Ident.IdentHash.create 1 in
  let do_instr node = function
    | Sil.Letderef (id, e, t, loc) ->
        Ident.IdentHash.add map id e
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc;
  map

(* lazy implementation, only create when used *)
let create cfg proc_desc =
  let map = lazy (_create cfg proc_desc) in
  map, cfg

(* create an idenv for another procedure *)
let create_from_idenv (_, cfg) proc_desc =
  let map = lazy (_create cfg proc_desc) in
  map, cfg

let lookup (_map, _) id =
  let map = Lazy.force _map in
  try
    Some (Ident.IdentHash.find map id)
  with Not_found -> None

let expand_expr idenv e = match e with
  | Sil.Var id ->
      (match lookup idenv id with
        | Some e' -> e'
        | None -> e)
  | _ -> e

let expand_expr_temps idenv node _exp =
  let exp = expand_expr idenv _exp in
  match exp with
  | Sil.Lvar pvar when Errdesc.pvar_is_frontend_tmp pvar ->
      (match Errdesc.find_program_variable_assignment node pvar with
        | None -> exp
        | Some (_, id) ->
            expand_expr idenv (Sil.Var id))
  | _ -> exp

(** Return true if the expression is a temporary variable introduced by the front-end. *)
let exp_is_temp idenv e =
  match expand_expr idenv e with
  | Sil.Lvar pvar ->
      Errdesc.pvar_is_frontend_tmp pvar
  | _ -> false
