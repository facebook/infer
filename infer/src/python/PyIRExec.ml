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
  let get hashtbl mk_error_msg key =
    match H.find_opt hashtbl key with
    | Some v ->
        v
    | None ->
        L.die L.InternalError "%s" (mk_error_msg key)


  let get_opt hashtbl key = H.find_opt hashtbl key

  let set hashtbl key v = H.replace hashtbl key v

  type 'a t = {get: H.key -> 'a; get_opt: H.key -> 'a option; set: H.key -> 'a -> unit}

  let create ~mk_error_msg () =
    let hashtbl = H.create 17 in
    let get = get hashtbl mk_error_msg in
    let get_opt = get_opt hashtbl in
    let set = set hashtbl in
    {get; get_opt; set}
end

module SSAEnv = GenericUnsafeHashtbl (SSA.Hashtbl)
module IdentEnv = GenericUnsafeHashtbl (Ident.Hashtbl)

type pval =
  (* very simple for now *)
  | None
  | Bool of bool
  | Int of Z.t
  | String of string
  | Closure of (pval list -> pval)

let get_closure = function Closure f -> f | _ -> L.die L.InternalError "get_closure failure"

module Builtin = struct
  let print args =
    let args =
      List.filter_map args ~f:(function
        | Bool true ->
            Some "True"
        | Bool false ->
            Some "False"
        | Int i ->
            Some (Z.to_string i)
        | String s ->
            Some s
        | None ->
            Some "None"
        | Closure _ ->
            None )
    in
    F.printf "%a@\n" (Pp.seq ~sep:" " F.pp_print_string) args ;
    None


  let mk_builtins_getter () =
    let mk_error_msg ident = F.asprintf "builtin %a not found" Ident.pp ident in
    let {IdentEnv.get; set} = IdentEnv.create () ~mk_error_msg in
    set Ident.Special.print (Closure print) ;
    get
end

let exec_cfg ~name {CFG.entry; nodes} =
  let builtins_get = Builtin.mk_builtins_getter () in
  let get_node node_name =
    match NodeName.Map.find_opt node_name nodes with
    | None ->
        L.die L.InternalError "exec_cfg: in cfg %s, no node with name %a" name NodeName.pp node_name
    | Some node ->
        node
  in
  let entry_node = get_node entry in
  let {SSAEnv.get= ssa_get; set= ssa_set} =
    let mk_error_msg ssa =
      F.asprintf "in cfg %s, SSA variable %a is not bind to any value" name SSA.pp ssa
    in
    SSAEnv.create ~mk_error_msg ()
  in
  let {IdentEnv.get_opt= locals_get_opt; set= locals_set} =
    let mk_error_msg ident =
      F.asprintf "in cfg %s, local variable %a is not bind to any value in" name Ident.pp ident
    in
    IdentEnv.create ~mk_error_msg ()
  in
  let eval_const const =
    match (const : Const.t) with
    | None ->
        None
    | Bool b ->
        Bool b
    | Int i ->
        Int i
    | String s ->
        String s
    | Float _ | Complex _ | InvalidUnicode _ | Bytes _ ->
        (* I don't think it makes sense to deal with this kind of constant in the interpreter *)
        todo "eval_const"
  in
  let eval_exp exp =
    match (exp : Exp.t) with
    | Const const ->
        eval_const const
    | Var {scope= Name; ident} ->
        locals_get_opt ident |> Option.value_or_thunk ~default:(fun () -> builtins_get ident)
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
        locals_set ident (eval_exp rhs)
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
