(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IdentTbl = Stdlib.Hashtbl.Make (Ident)
module PvarTbl = Stdlib.Hashtbl.Make (Pvar)
module IdentHashSet = HashSet.Make (Ident)

(* For every [Sil.Call] in the procdesc, set [CallFlags.cf_return_null_checked = true] when the
   caller safely handles a nil result via [if let], [?.], or [guard let .. else].

   Flow-insensitive may-analysis: track the call's ret_id forward through Loads/Stores and the
   Optional bridge call; mark when a downstream [Prune] gates use of the propagated value. Tables
   iterated to fixpoint via snapshot equality. *)

let is_null_or_zero (e : Exp.t) = Exp.is_null_literal e || Exp.is_zero e

let rec strip_casts = function Exp.Cast (_, e) -> strip_casts e | e -> e

let prune_compares_to_null (instr : Sil.instr) =
  match instr with
  | Prune (BinOp (Ne, e1, e2), _, _, _) | Prune (UnOp (LNot, BinOp (Eq, e1, e2), _), _, _, _) -> (
    match (strip_casts e1, strip_casts e2) with
    | Var id, e when is_null_or_zero e ->
        Some id
    | e, Var id when is_null_or_zero e ->
        Some id
    | _ ->
        None )
  | Prune (Var id, _, _, _) ->
      Some id
  | _ ->
      None


(* Sibling of an [if let X = e { ... }] Prune: empty else (no Store, no Call). *)
let node_is_trivial_else node =
  Procdesc.Node.get_instrs node
  |> Instrs.for_all ~f:(fun (instr : Sil.instr) ->
         match instr with Store _ | Call _ -> false | _ -> true )


(* Force-unwrap / fatalError lowers to [__assert_fail] / [_assertionFailure]. *)
let is_fatal_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      let name = Procname.to_string pname in
      String.is_substring name ~substring:"__assert_fail"
      || String.is_substring name ~substring:"assertionFailure"
  | _ ->
      false


(* Sibling of a post-bridge user null check: tolerates Stores and non-fatal Calls. *)
let node_has_no_fatal_call node =
  Procdesc.Node.get_instrs node
  |> Instrs.for_all ~f:(fun (instr : Sil.instr) ->
         match instr with Call (_, callee, _, _, _) -> not (is_fatal_callee callee) | _ -> true )


let eq_branch_satisfies node ~check =
  match Procdesc.Node.get_preds node with
  | [pred] ->
      List.exists (Procdesc.Node.get_succs pred) ~f:(fun s ->
          (not (Procdesc.Node.equal s node)) && check s )
  | _ ->
      false


let is_bridge_from_objc_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      let name = Procname.to_string pname in
      String.is_substring name ~substring:"BridgeFromObjectiveC"
      || String.is_substring name ~substring:"bridgeFromObjectiveC"
  | _ ->
      false


let process pdesc =
  let id_origin : Ident.t IdentTbl.t = IdentTbl.create 32 in
  let pvar_origin : Ident.t PvarTbl.t = PvarTbl.create 16 in
  let checked = IdentHashSet.create 8 in
  let visit_instr node (instr : Sil.instr) =
    match instr with
    | Call ((ret_id, _), callee, args, _, _) -> (
        IdentTbl.replace id_origin ret_id ret_id ;
        if is_bridge_from_objc_callee callee then
          match args with
          | (arg_exp, _) :: _ -> (
            match strip_casts arg_exp with
            | Exp.Var src -> (
              match IdentTbl.find_opt id_origin src with
              | Some r ->
                  IdentTbl.replace id_origin ret_id r
              | None ->
                  () )
            | _ ->
                () )
          | _ ->
              () )
    | Load {id; e= Var src} -> (
      match IdentTbl.find_opt id_origin src with
      | Some r ->
          IdentTbl.replace id_origin id r
      | None ->
          () )
    | Load {id; e= Lvar pvar} -> (
      match PvarTbl.find_opt pvar_origin pvar with
      | Some r ->
          IdentTbl.replace id_origin id r
      | None ->
          () )
    | Load {id; e= Lfield ({exp= Var src; _}, _, _)} -> (
      (* Tuple-field load on the bridge result: propagate src's origin. *)
      match IdentTbl.find_opt id_origin src with
      | Some r ->
          IdentTbl.replace id_origin id r
      | None ->
          () )
    | Store {e1= Lvar pvar; e2} -> (
      match strip_casts e2 with
      | Var id -> (
        match IdentTbl.find_opt id_origin id with
        | Some r ->
            PvarTbl.replace pvar_origin pvar r
        | None ->
            () )
      | _ ->
          () )
    | Prune _ -> (
      match prune_compares_to_null instr with
      | Some id -> (
        match IdentTbl.find_opt id_origin id with
        | Some r when Ident.equal id r ->
            (* Raw ret_id Prune ([if let]): require trivial else. *)
            if eq_branch_satisfies node ~check:node_is_trivial_else then IdentHashSet.add r checked
        | Some r ->
            (* Propagated id Prune (chain / guard-let): tolerate non-fatal Calls. *)
            if eq_branch_satisfies node ~check:node_has_no_fatal_call then
              IdentHashSet.add r checked
        | None ->
            () )
      | None ->
          () )
    | _ ->
        ()
  in
  let one_pass () =
    Procdesc.iter_nodes
      (fun node -> Procdesc.Node.get_instrs node |> Instrs.iter ~f:(visit_instr node))
      pdesc
  in
  (* Snapshot id_origin and pvar_origin: cardinality alone misses value-only updates (e.g. bridge
     propagation overwriting [id_origin[ret]] from [ret] to its origin). *)
  let snapshot () =
    let dump_id tbl = IdentTbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    let dump_pvar tbl = PvarTbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    (dump_id id_origin, dump_pvar pvar_origin)
  in
  let max_iter = 16 in
  let rec loop prev n =
    if n >= max_iter then ()
    else (
      one_pass () ;
      let cur = snapshot () in
      if not (Poly.equal prev cur) then loop cur (n + 1) )
  in
  loop (snapshot ()) 0 ;
  if IdentHashSet.is_empty checked then ()
  else
    let _ : bool =
      Procdesc.replace_instrs pdesc ~f:(fun _node (instr : Sil.instr) ->
          match instr with
          | Call (((ret_id, _) as ret), fexp, args, loc, flags)
            when IdentHashSet.mem checked ret_id && not flags.CallFlags.cf_return_null_checked ->
              Sil.Call (ret, fexp, args, loc, {flags with cf_return_null_checked= true})
          | _ ->
              instr )
    in
    ()
