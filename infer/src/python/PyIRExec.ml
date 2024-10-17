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

let todo msg = L.die InternalError "TODO: %s" msg

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

let pp_pval fmt = function
  | None ->
      F.pp_print_string fmt "None"
  | Bool true ->
      F.pp_print_string fmt "True"
  | Bool false ->
      F.pp_print_string fmt "False"
  | Int i ->
      F.pp_print_string fmt (Z.to_string i)
  | String s ->
      Format.pp_print_string fmt s
  | Closure _ ->
      F.pp_print_string fmt "Closure"


let expect_int ~who ?how = function
  | Int i ->
      i
  | v ->
      L.die InternalError "%s expects an integer%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_bool ~who ?how = function
  | Bool b ->
      b
  | v ->
      L.die InternalError "%s expects a bool%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_2_args ~who = function
  | [arg1; arg2] ->
      (arg1, arg2)
  | args ->
      L.die InternalError "%s expects 2 args and reveiced [%a]" who (Pp.comma_seq Exp.pp) args


let get_closure = function Closure f -> f | _ -> L.die InternalError "get_closure failure"

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

module Globals = struct
  let mk_globals_utils ~name =
    let {IdentEnv.get_opt; set} =
      let mk_error_msg ident =
        F.asprintf "in module %a, global variable %a is not bind to any value" Ident.pp name
          Ident.pp ident
      in
      IdentEnv.create ~mk_error_msg ()
    in
    (get_opt, set)
end

module Locals = struct
  let mk_locals_utils ~co_name ~co_varnames ~co_argcount args =
    let {IdentEnv.get; get_opt; set} =
      let mk_error_msg ident =
        F.asprintf "in cfg %a, local variable %a is not bind to any value" Ident.pp co_name Ident.pp
          ident
      in
      IdentEnv.create ~mk_error_msg ()
    in
    let args_length = List.length args in
    if not (Int.equal args_length co_argcount) then
      (* TODO: deal with optionals arguments *)
      L.die InternalError "In cfg %a, %d arguments are expected but %d are given" Ident.pp co_name
        args_length co_argcount ;
    List.iteri args ~f:(fun i arg ->
        let arg_name = co_varnames.(i) in
        set arg_name arg ) ;
    (get_opt, get, set)
end

let run {Module.name; toplevel; functions} =
  let builtins_get = Builtin.mk_builtins_getter () in
  let get_cfg qual_name =
    QualName.Map.find_opt qual_name functions
    |> Option.value_or_thunk ~default:(fun () ->
           L.die InternalError "exec_cfg: no cfg with name %a" QualName.pp qual_name )
  in
  let globals_get_opt, globals_set = Globals.mk_globals_utils ~name in
  let rec exec_cfg {CFG.entry; nodes; code_info} args =
    let {CodeInfo.co_name; co_varnames; co_argcount} = code_info in
    let get_node node_name =
      NodeName.Map.find_opt node_name nodes
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "exec_cfg: in cfg %a, no node with name %a" Ident.pp co_name
               NodeName.pp node_name )
    in
    let entry_node = get_node entry in
    let {SSAEnv.get= ssa_get; set= ssa_set} =
      let mk_error_msg ssa =
        F.asprintf "in cfg %a, SSA variable %a is not bind to any value" Ident.pp co_name SSA.pp ssa
      in
      SSAEnv.create ~mk_error_msg ()
    in
    let locals_get_opt, locals_get, locals_set =
      Locals.mk_locals_utils ~co_name ~co_varnames ~co_argcount args
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
      | Var {scope= Fast; ident} ->
          locals_get ident
      | Var {scope= Global; ident} ->
          globals_get_opt ident |> Option.value_or_thunk ~default:(fun () -> builtins_get ident)
      | Var {scope= Name; ident} ->
          locals_get_opt ident
          |> Option.value_or_thunk ~default:(fun () ->
                 globals_get_opt ident
                 |> Option.value_or_thunk ~default:(fun () -> builtins_get ident) )
      | Temp ssa ->
          ssa_get ssa
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
      | Store {lhs= {scope= Fast; ident}; rhs} ->
          locals_set ident (eval_exp rhs)
      | Store {lhs= {scope= Name; ident}; rhs} ->
          (* TODO: inside class body / module we will need a different behavior *)
          globals_set ident (eval_exp rhs)
      | Store {lhs= {scope= Global; ident}; rhs} ->
          globals_set ident (eval_exp rhs)
      | Call {lhs; exp; args} ->
          let f = get_closure (eval_exp exp) in
          let args = List.map ~f:eval_exp args in
          ssa_set lhs (f args)
      | BuiltinCall {lhs; call= Function {qual_name}; args} ->
          if not (Int.equal (List.length args) 4) then
            L.die InternalError "$BuiltinCall.Function expects 4 args and reveiced [%a]"
              (Pp.comma_seq Exp.pp) args ;
          let cfg = get_cfg qual_name in
          let eval = exec_cfg cfg in
          ssa_set lhs (Closure eval)
      | BuiltinCall {lhs; call= Inplace Add; args} ->
          let who = "$BuiltinCall.Inplace.Add" in
          let arg1, arg2 = expect_2_args ~who args in
          let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
          let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
          ssa_set lhs (Int (Z.add i1 i2))
      | BuiltinCall {lhs; call= Compare Le; args} ->
          let who = "$BuiltinCall.Compare.Le" in
          let arg1, arg2 = expect_2_args ~who args in
          let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
          let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
          ssa_set lhs (Bool (Z.leq i1 i2))
      | BuiltinCall {lhs; call= Binary Subtract; args} ->
          let who = "$BuiltinCall.Binary.Subtract" in
          let arg1, arg2 = expect_2_args ~who args in
          let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
          let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
          ssa_set lhs (Int (Z.sub i1 i2))
      | BuiltinCall {lhs; call= Binary Multiply; args} ->
          let who = "$BuiltinCall.Binary.Subtract" in
          let arg1, arg2 = expect_2_args ~who args in
          let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
          let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
          ssa_set lhs (Int (Z.mul i1 i2))
      | SetAttr _ | StoreSubscript _ | CallMethod _ | BuiltinCall _ | SetupAnnotations ->
          todo "exec_stmt"
    in
    let rec exec_terminator terminator =
      match (terminator : Terminator.t) with
      | Return exp ->
          eval_exp exp
      | If {exp; then_; else_} ->
          let test = eval_exp exp |> expect_bool ~who:"If terminator" in
          if test then exec_node_call then_ else exec_node_call else_
      | _ ->
          todo "exec_terminator"
    and exec_node_call {Terminator.label; ssa_args} =
      let node = get_node label in
      let args = List.map ~f:eval_exp ssa_args in
      exec_node node args
    and exec_node {Node.ssa_parameters; stmts; last} args =
      List.iter2_exn ssa_parameters args ~f:(fun ssa v -> ssa_set ssa v) ;
      List.iter stmts ~f:(fun (_loc, stmt) -> exec_stmt stmt) ;
      exec_terminator last
    in
    exec_node entry_node []
  in
  exec_cfg toplevel [] |> ignore
