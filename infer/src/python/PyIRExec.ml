(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PyIR

let todo msg = L.die L.InternalError "TODO: %s" msg

module GenericUnsafeHashtbl (H : Caml.Hashtbl.S) = struct
  type 'a t = {hashtbl: 'a H.t; error_msg: H.key -> string}

  let create error_msg = {hashtbl= H.create 17; error_msg}

  let get {hashtbl; error_msg} key =
    match H.find_opt hashtbl key with
    | Some v ->
        v
    | None ->
        L.die L.InternalError "%s" (error_msg key)


  let set {hashtbl} key v = H.replace hashtbl key v
end

module SSAEnv = GenericUnsafeHashtbl (SSA.Hashtbl)
module IdentEnv = GenericUnsafeHashtbl (Ident.Hashtbl)

type pval = (* very simple for now *)
  | None | Int of Z.t | Closure of (pval list -> pval)

let get_closure = function Closure f -> f | _ -> L.die L.InternalError "get_closure failure"

let exec_cfg ~name {CFG.entry; nodes} =
  let get_node node_name =
    match NodeName.Map.find_opt node_name nodes with
    | None ->
        L.die L.InternalError "exec_cfg: in cfg %s, no node with name %a" name NodeName.pp node_name
    | Some node ->
        node
  in
  let entry_node = get_node entry in
  let ssa_env =
    SSAEnv.create (fun ssa ->
        F.asprintf "in cfg %s, SSA variable %a is not bind to any value" name SSA.pp ssa )
  in
  let ssa_get = SSAEnv.get ssa_env in
  let ssa_set = SSAEnv.set ssa_env in
  let env =
    (* we simplify for now *)
    IdentEnv.create (fun ident ->
        F.asprintf "in cfg %s, variable %a is not bind to any value" name Ident.pp ident )
  in
  let get = IdentEnv.get env in
  let set = IdentEnv.set env in
  set Ident.Special.print
    (Closure
       (fun args ->
         let args =
           List.filter_map args ~f:(function Int i -> Some (Z.to_string i) | _ -> None)
         in
         F.printf "%a@\n" (Pp.seq ~sep:" " F.pp_print_string) args ;
         None ) ) ;
  let eval_const const = match (const : Const.t) with Int i -> Int i | _ -> todo "eval_const" in
  let eval_exp exp =
    match (exp : Exp.t) with
    | Const const ->
        eval_const const
    | Var {scope= Name; ident} ->
        get ident
    | Temp ssa ->
        ssa_get ssa
    | Var _
    | Subscript _
    | BuildSlice _
    | BuildString _
    | BuildFrozenSet _
    | Collection _
    | GetAttr _
    | Yield _ ->
        todo "eval_exp"
  in
  let exec_stmt stmt =
    match (stmt : Stmt.t) with
    | Let {lhs; rhs} ->
        ssa_set lhs (eval_exp rhs)
    | Store {lhs= {scope= Name; ident}; rhs} ->
        set ident (eval_exp rhs)
    | Call {lhs; exp; args} ->
        let f = get_closure (eval_exp exp) in
        let args = List.map ~f:eval_exp args in
        ssa_set lhs (f args)
    | Store _ | SetAttr _ | StoreSubscript _ | CallMethod _ | BuiltinCall _ | SetupAnnotations ->
        todo "exec_stmt"
  in
  let exec_node {Node.ssa_parameters; stmts} args =
    List.iter2_exn ssa_parameters args ~f:(fun ssa v -> ssa_set ssa v) ;
    List.iter stmts ~f:(fun (_loc, stmt) -> exec_stmt stmt)
  in
  exec_node entry_node []


let run {Module.toplevel} = exec_cfg ~name:"toplevel" toplevel
