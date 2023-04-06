(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module T = Textual

module DataStack = struct
  type cell =
    | Const of int (* index in co_consts *)
    | Name of int (* reference to a global name, stored in co_names *)
    | Temp of T.Ident.t (* SSA variable *)

  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)
end [@warning "-unused-constructor"]

module Env = struct
  type t =
    { stack: DataStack.t
    ; idents: T.Ident.Set.t
    ; globals: T.VarName.Set.t
    ; instructions: T.Instr.t list }

  let push env cell =
    let stack = DataStack.push env.stack cell in
    {env with stack}


  let pop ({stack} as env) =
    DataStack.pop stack |> Option.map ~f:(fun (stack, cell) -> ({env with stack}, cell))


  let temp ({idents} as env) =
    let fresh = T.Ident.fresh idents in
    let idents = T.Ident.Set.add fresh idents in
    ({env with idents}, fresh)


  let push_instr ({instructions} as env) instr = {env with instructions= instr :: instructions}

  let get_instructions {instructions} = List.rev instructions

  let register_global ({globals} as env) name = {env with globals= T.VarName.Set.add name globals}
end

module Debug = struct
  (* Custom verbose flag, while I'm still building this front end.
     I'll move to Logging once it's done. *)
  let debug = false

  let p fmt = if debug then Printf.fprintf stdout fmt else Printf.ifprintf stdout fmt
end

let var_name value = T.VarName.{value; loc= T.Location.Unknown}

let type_name value = T.TypeName.{value; loc= T.Location.Unknown}

let pyObject = T.Typ.(Ptr (Struct (type_name "PyObject")))

let global name = sprintf "$globals::%s" name

let load_cell env FFI.Code.{co_consts; co_names} cell =
  match cell with
  | DataStack.Const ndx -> (
      let const = co_consts.(ndx) in
      match FFI.Constant.to_exp const with
      | None ->
          (env, Error "[load_cell] Constant contains code objects")
      | Some exp ->
          (env, Ok exp) )
  | DataStack.Name ndx ->
      let name = global co_names.(ndx) in
      let env, id = Env.temp env in
      let exp = T.Exp.Lvar (var_name name) in
      let instr = T.Instr.Load {id; exp; typ= pyObject; loc= T.Location.Unknown} in
      let env = Env.push_instr env instr in
      (env, Ok (T.Exp.Var id))
  | DataStack.Temp id ->
      (env, Ok (T.Exp.Var id))


module LOAD_CONST = struct
  (* LOAD_CONST(consti)
     Pushes co_consts[consti] onto the stack. *)

  let run env FFI.Code.{co_consts} FFI.Instruction.{arg} =
    let const = co_consts.(arg) in
    Debug.p "[LOAD_CONST] arg = %s\n" (FFI.Constant.show const) ;
    let cell = DataStack.Const arg in
    (Env.push env cell, None)
end

module STORE_NAME = struct
  (* STORE_NAME(namei)
     Implements name = top-of-stack. namei is the index of name in the attribute co_names of the code
     object. The compiler tries to use STORE_FAST or STORE_GLOBAL if possible.

     Notes: this should only happen in global nodes, to update global variables from the global
     scope.

     In a function, local varialbes are updated using STORE_FAST, and global variables are
     updated using STORE_GLOBAL *)

  let run env FFI.Code.({co_names} as code) FFI.Instruction.{arg} =
    let name = global co_names.(arg) in
    Debug.p "[STORE_NAME] name = %s\n" name ;
    let var_name = var_name name in
    let env = Env.register_global env var_name in
    let env, cell =
      match Env.pop env with
      | None ->
          L.die InternalError "[STORE_NAME] stack is empty"
      | Some (env, cell) ->
          (env, cell)
    in
    let env, exp = load_cell env code cell in
    match exp with
    | Ok exp ->
        let instr =
          T.Instr.Store
            {exp1= T.Exp.Lvar var_name; typ= pyObject; exp2= exp; loc= T.Location.Unknown}
        in
        (Env.push_instr env instr, None)
    | Error s ->
        L.die InternalError "[STORE_NAME] %s" s
end

module RETURN_VALUE = struct
  (* RETURN_VALUE
     returns the top-of-stack *)
  let run env code _instr =
    Debug.p "[RETURN_VALUE]\n" ;
    let env, cell =
      match Env.pop env with
      | None ->
          L.die InternalError "[RETURN_VALUE] stack is empty"
      | Some (env, cell) ->
          (env, cell)
    in
    let _env, exp = load_cell env code cell in
    match exp with
    | Ok exp ->
        let term = T.Terminator.Ret exp in
        (env, Some term)
    | Error s ->
        L.die InternalError "[RETURN_VALUE] %s" s
end

let run_instruction env code FFI.Instruction.({opname} as instr) =
  let env, maybe_term =
    match opname with
    | "LOAD_CONST" ->
        LOAD_CONST.run env code instr
    | "STORE_NAME" ->
        STORE_NAME.run env code instr
    | "RETURN_VALUE" ->
        RETURN_VALUE.run env code instr
    | _ ->
        L.die InternalError "Unsupported opcode: %s" opname
  in
  (env, maybe_term)


let rec run env code = function
  | [] ->
      (env, None, [])
  | instr :: rest -> (
      let env, maybe_term = run_instruction env code instr in
      match maybe_term with Some term -> (env, Some term, rest) | None -> run env code rest )


let node globals idents label FFI.Code.({instructions} as code) =
  let env = Env.{stack= []; idents; globals; instructions= []} in
  let env, maybe_term, rest = run env code instructions in
  match maybe_term with
  | None ->
      L.die InternalError "Reached the end of code without spotting a terminator"
  | Some last ->
      let node =
        T.Node.
          { label
          ; ssa_parameters= []
          ; exn_succs= []
          ; last
          ; instrs= Env.get_instructions env
          ; last_loc= T.Location.Unknown
          ; label_loc= T.Location.Unknown }
      in
      (env.Env.idents, env.Env.globals, rest, node)


let node_name value = T.NodeName.{value; loc= T.Location.Unknown}

let proc_name value = T.ProcName.{value; loc= T.Location.Unknown}

(* TODO: We only deal with toplevel functions for now *)
let qualified_procname name : T.qualified_procname = {enclosing_class= TopLevel; name}

let to_proc_desc name globals code =
  let qualified_name = qualified_procname name in
  let pyObject = T.Typ.{typ= pyObject; attributes= []} in
  let procdecl =
    T.ProcDecl.
      { qualified_name
      ; formals_types= []
      ; are_formal_types_fully_declared= true
      ; result_type= pyObject
      ; attributes= [] }
  in
  let label = node_name "b0" in
  let idents = T.Ident.Set.empty in
  let _idents, globals, remaining_instructions, node = node globals idents label code in
  if not (List.is_empty remaining_instructions) then
    L.die InternalError "%d instructions are left" (List.length remaining_instructions) ;
  ( globals
  , T.ProcDesc.
      {procdecl; nodes= [node]; start= label; params= []; locals= []; exit_loc= T.Location.Unknown}
  )


let std_builtins =
  let annot typ = T.Typ.{typ; attributes= []} in
  let mk qualified_name typ =
    let result_type = annot pyObject in
    T.ProcDecl.
      { qualified_name
      ; formals_types= [annot typ]
      ; are_formal_types_fully_declared= true
      ; result_type
      ; attributes= [] }
  in
  let python_int = mk FFI.python_int T.Typ.Int in
  let python_string = mk FFI.python_string pyObject in
  let python_tuple = mk FFI.python_tuple pyObject in
  T.Module.[Procdecl python_int; Procdecl python_string; Procdecl python_tuple]


let to_module ~sourcefile module_name code =
  let name = proc_name module_name in
  let globals = T.VarName.Set.empty in
  let globals, decl = to_proc_desc name globals code in
  let globals =
    T.VarName.Set.fold
      (fun name acc ->
        let global = T.Global.{name; typ= pyObject; attributes= []} in
        T.Module.Global global :: acc )
      globals []
  in
  let decls = (T.Module.Proc decl :: globals) @ std_builtins in
  T.Module.{attrs= []; decls; sourcefile}
