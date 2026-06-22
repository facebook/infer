(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IdentTbl = Stdlib.Hashtbl.Make (Ident)
module PvarTbl = Stdlib.Hashtbl.Make (Pvar)

module PvarField = struct
  type t = Pvar.t * Fieldname.t

  let equal (p1, f1) (p2, f2) = Pvar.equal p1 p2 && Fieldname.equal f1 f2

  let hash (p, f) = Hashtbl.hash (Pvar.hash p, Fieldname.hash f)
end

module PvarFieldTbl = Stdlib.Hashtbl.Make (PvarField)
module IdentHashSet = HashSet.Make (Ident)

(* For every [Sil.Call] in the procdesc, set [CallFlags.cf_return_null_checked = true] when the
   caller safely handles a nil result via [if let], [?.], [guard let .. else], or [??].

   Flow-insensitive may-analysis: track the call's ret_id forward through Loads/Stores, the
   Optional bridge call, tuple packing, and [memcpy]; mark when a downstream [Prune] gates use of
   the propagated value. Tables iterated to fixpoint via snapshot equality. *)

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


(* Force-unwrap / fatalError lowers to [__assert_fail] / [_assertionFailure]; the
   [Llair2TextualForceUnwrap] pass rewrites the recognised force-unwrap trap call to
   [__swift_optional_force_unwrap_trap], which is equally fatal for this analysis. *)
let is_fatal_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      let name = Procname.to_string pname in
      String.is_substring name ~substring:"__assert_fail"
      || String.is_substring name ~substring:"assertionFailure"
      || String.is_substring name ~substring:"__swift_optional_force_unwrap_trap"
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


(* Swift's [??] lowers via [memcpy] between sibling tuple pvars; copy field origins src -> dst. *)
let is_memcpy_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      String.equal (Procname.to_string pname) "memcpy"
  | _ ->
      false


(* The Swift frontend emits a defensive raw-ret_id Prune around every [objc_msgSend] result to
   support dynamic dispatch: the non-null arm calls the IUO bridge, the null arm stores zeros into
   the result temps. The shape is syntactically identical to a user-written [if e != nil { ... }],
   but the [objc_msgSend] callee is a tell-tale: typed direct Swift->ObjC calls do not get this
   defensive lowering. Mark these ret_ids so the [raw_prune_safe] arm can skip them and avoid
   suppressing real MISSING_NULLABILITY findings on bare-result-discarded dynamic dispatch
   (e.g. [let _ = device.foo()] where [device: AnyObject]). *)
let is_objc_msgSend_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      String.is_substring (Procname.to_string pname) ~substring:"objc_msgSend"
  | _ ->
      false


(* Calls that don't count as user "consuming" the value -- ARC plumbing, the bridge call itself,
   and the helpers for retain/release that move ownership without touching the payload. Used to
   gate the arg-position-passthrough check (Pattern 5) so we don't mark on transparent helpers. *)
let is_transparent_helper_callee (callee : Exp.t) =
  match callee with
  | Const (Const.Cfun pname) ->
      let name = Procname.to_string pname in
      String.is_substring name ~substring:"BridgeFromObjectiveC"
      || String.is_substring name ~substring:"bridgeFromObjectiveC"
      || String.is_substring name ~substring:"objc_retain"
      || String.is_substring name ~substring:"objc_release"
      || String.is_substring name ~substring:"swift_retain"
      || String.is_substring name ~substring:"swift_release"
      || String.is_substring name ~substring:"swift_bridgeObjectRetain"
      || String.is_substring name ~substring:"swift_bridgeObjectRelease"
      || String.equal name "memcpy"
      || String.equal name "llvm_init_tuple"
  | _ ->
      false


let process pdesc =
  let id_origin : Ident.t IdentTbl.t = IdentTbl.create 32 in
  let pvar_origin : Ident.t PvarTbl.t = PvarTbl.create 16 in
  let pvar_field_origin : Ident.t PvarFieldTbl.t = PvarFieldTbl.create 8 in
  let id_to_pvar : Pvar.t IdentTbl.t = IdentTbl.create 16 in
  let checked = IdentHashSet.create 8 in
  let force_unwrapped = IdentHashSet.create 8 in
  let arg_consumed = IdentHashSet.create 8 in
  let raw_prune_safe = IdentHashSet.create 8 in
  let from_objc_msgSend = IdentHashSet.create 4 in
  let copy_pvar_fields ~src ~dst =
    PvarFieldTbl.iter
      (fun (p, f) r -> if Pvar.equal p src then PvarFieldTbl.replace pvar_field_origin (dst, f) r)
      pvar_field_origin
  in
  let visit_instr node (instr : Sil.instr) =
    match instr with
    | Call ((ret_id, _), callee, args, _, _) ->
        IdentTbl.replace id_origin ret_id ret_id ;
        if is_objc_msgSend_callee callee then IdentHashSet.add ret_id from_objc_msgSend ;
        ( if is_bridge_from_objc_callee callee then
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
                ()
          else if is_memcpy_callee callee then
            match args with
            | (Exp.Var dst_id, _) :: (Exp.Var src_id, _) :: _ -> (
              match (IdentTbl.find_opt id_to_pvar dst_id, IdentTbl.find_opt id_to_pvar src_id) with
              | Some dst_pvar, Some src_pvar ->
                  copy_pvar_fields ~src:src_pvar ~dst:dst_pvar
              | _ ->
                  () )
            | _ ->
                () ) ;
        (* Pattern 5: a non-transparent Call consuming the propagated value as a Var arg
           means the bridged Optional<T> flows safely into the callee's parameter. Record
           the origin in [arg_consumed]; we'll mark it [checked] only if it's not also
           [force_unwrapped] elsewhere in this proc. *)
        if not (is_transparent_helper_callee callee) then
          List.iter args ~f:(fun (arg_exp, _) ->
              match strip_casts arg_exp with
              | Exp.Var v -> (
                match IdentTbl.find_opt id_origin v with
                | Some origin when not (Ident.equal v origin) ->
                    IdentHashSet.add origin arg_consumed
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
        IdentTbl.replace id_to_pvar id pvar ;
        match PvarTbl.find_opt pvar_origin pvar with
        | Some r ->
            IdentTbl.replace id_origin id r
        | None ->
            () )
    | Load {id; e= Lfield ({exp= Var src; _}, fname, _)} ->
        (* Tuple-field load: prefer pvar_field_origin, fall back to src's own origin. *)
        let fresh_origin =
          match IdentTbl.find_opt id_to_pvar src with
          | Some pvar -> (
            match PvarFieldTbl.find_opt pvar_field_origin (pvar, fname) with
            | Some r ->
                Some r
            | None ->
                IdentTbl.find_opt id_origin src )
          | None ->
              IdentTbl.find_opt id_origin src
        in
        Option.iter fresh_origin ~f:(IdentTbl.replace id_origin id)
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
    | Store {e1= Lfield ({exp= Var dst_id; _}, fname, _); e2} -> (
      (* [*n$dst_id.field = n$id]: record field origin under dst_id's source pvar. *)
      match strip_casts e2 with
      | Var id -> (
        match (IdentTbl.find_opt id_to_pvar dst_id, IdentTbl.find_opt id_origin id) with
        | Some pvar, Some r ->
            PvarFieldTbl.replace pvar_field_origin (pvar, fname) r
        | _ ->
            () )
      | _ ->
          () )
    | Prune _ -> (
      match prune_compares_to_null instr with
      | Some id -> (
        match IdentTbl.find_opt id_origin id with
        | Some r when Ident.equal id r ->
            (* Raw ret_id Prune. [if let X = e { ... }] (trivial else) marks immediately;
               [if e != nil { ... }] (Optional-packaging Stores in eq-branch but no fatal
               Call) is deferred to [raw_prune_safe] and committed only if the same origin
               isn't [force_unwrapped] elsewhere in the proc -- otherwise an immediate
               same-call force-unwrap (`let s = e!`) would slip through this arm.

               Skip both arms entirely when the call is [objc_msgSend]: the Swift frontend
               emits the same Prune shape defensively around every dynamic-dispatch result,
               so it never reflects a user-written null check there. *)
            if not (IdentHashSet.mem from_objc_msgSend r) then
              if eq_branch_satisfies node ~check:node_is_trivial_else then
                IdentHashSet.add r checked
              else if eq_branch_satisfies node ~check:node_has_no_fatal_call then
                IdentHashSet.add r raw_prune_safe
        | Some r ->
            (* Propagated id Prune (chain / [??] / guard-let): tolerate non-fatal Calls.
               If the eq-branch contains a fatal call (force-unwrap), poison [r] so any
               concurrent Pattern 5 arg-consumption check on the same origin is skipped. *)
            if eq_branch_satisfies node ~check:node_has_no_fatal_call then
              IdentHashSet.add r checked
            else IdentHashSet.add r force_unwrapped
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
  (* Snapshot all four tables: cardinality alone misses value-only updates (e.g. bridge
     propagation overwriting [id_origin[ret]] from [ret] to its origin). *)
  let snapshot () =
    let dump_id tbl = IdentTbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    let dump_pvar tbl = PvarTbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    let dump_pvarfield tbl = PvarFieldTbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
    (dump_id id_origin, dump_pvar pvar_origin, dump_pvarfield pvar_field_origin, dump_id id_to_pvar)
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
  (* Finalisation. Both [arg_consumed] (Pattern 5) and [raw_prune_safe] (Pattern 7's
     [if e != nil] check) are committed unless the same origin was also force-unwrapped
     (poisoned) elsewhere in the proc -- e.g. `let s = e!` has the Optional-packaging
     Prune (raw_prune_safe) AND a propagated-id Prune with `__assert_fail`
     (force_unwrapped); the poison wins. *)
  IdentHashSet.iter arg_consumed (fun origin ->
      if not (IdentHashSet.mem force_unwrapped origin) then IdentHashSet.add origin checked ) ;
  IdentHashSet.iter raw_prune_safe (fun origin ->
      if not (IdentHashSet.mem force_unwrapped origin) then IdentHashSet.add origin checked ) ;
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
