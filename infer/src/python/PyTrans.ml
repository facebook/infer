(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module T = Textual
module PyBuiltins = PyCommon.Builtins

module DataStack = struct
  type cell =
    | Const of int (* index in co_consts *)
    | Name of int (* reference to a global name, stored in co_names *)
    | Temp of T.Ident.t (* SSA variable *)
  [@@deriving show]

  let show_cell_kind = function
    | Const _ ->
        "DataStack.Const"
    | Name _ ->
        "DataStack.Name"
    | Temp _ ->
        "DataStack.Temp"


  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)
end

module Env = struct
  (* Part of the environment shared by most structures. It gathers information like which
     builtin has been spotted, or what idents have been generated so far. *)
  type shared = {idents: T.Ident.Set.t; globals: T.VarName.Set.t; builtins: PyBuiltins.t}

  (* State of the capture while processing a single node: each node has a dedicated data stack,
     and generates its own set of instructions.
     TODO(vsiles): revisit the data stack status once generators and conditions are in the mix *)
  type node = {stack: DataStack.t; instructions: T.Instr.t list; last_line: int option}

  type t = {shared: shared; node: node}

  let mk_node shared = {shared; node= {stack= []; instructions= []; last_line= None}}

  let reset_idents env = {env with idents= T.Ident.Set.empty}

  let empty = {idents= T.Ident.Set.empty; globals= T.VarName.Set.empty; builtins= PyBuiltins.empty}

  (* Only update last_line if some line information is available *)
  let starts_line node last_line = if Option.is_some last_line then {node with last_line} else node

  let starts_line ({node} as env) last_line = {env with node= starts_line node last_line}

  let loc {last_line} =
    last_line
    |> Option.map ~f:(fun line -> T.Location.known ~line ~col:0)
    |> Option.value ~default:T.Location.Unknown


  let loc {node} = loc node

  let push ({stack} as env) cell =
    let stack = DataStack.push stack cell in
    {env with stack}


  let push ({node} as env) cell =
    let node = push node cell in
    {env with node}


  let pop ({stack} as env) =
    DataStack.pop stack |> Option.map ~f:(fun (stack, cell) -> ({env with stack}, cell))


  let pop ({node} as env) = pop node |> Option.map ~f:(fun (node, cell) -> ({env with node}, cell))

  let temp ({idents} as env) =
    let fresh = T.Ident.fresh idents in
    let idents = T.Ident.Set.add fresh idents in
    ({env with idents}, fresh)


  let temp ({shared} as env) =
    let shared, fresh = temp shared in
    ({env with shared}, fresh)


  let push_instr ({instructions} as env) instr = {env with instructions= instr :: instructions}

  let push_instr ({node} as env) instr = {env with node= push_instr node instr}

  let get_instructions {instructions} = List.rev instructions

  let get_instructions {node} = get_instructions node

  let register_global ({globals} as env) name = {env with globals= T.VarName.Set.add name globals}

  let register_global ({shared} as env) name = {env with shared= register_global shared name}

  let register_builtin ({builtins} as env) name =
    {env with builtins= PyBuiltins.register builtins name}


  let register_builtin ({shared} as env) name = {env with shared= register_builtin shared name}
end

module Debug = struct
  (* Custom verbose flag, while I'm still building this front end.
     I'll move to Logging once it's done. *)
  let debug = false

  let p fmt = if debug then Printf.fprintf stdout fmt else Printf.ifprintf stdout fmt
end

let var_name ?(loc = T.Location.Unknown) value = T.VarName.{value; loc}

let node_name ?(loc = T.Location.Unknown) value = T.NodeName.{value; loc}

let proc_name ?(loc = T.Location.Unknown) value = T.ProcName.{value; loc}

(* TODO: We only deal with toplevel functions for now *)
let qualified_procname name : T.qualified_procname = {enclosing_class= TopLevel; name}

let global name = sprintf "$globals::%s" name

(* Until we support python types, everything is a *Object *)
let pyObject = PyCommon.pyObject

let load_cell env FFI.Code.{co_consts; co_names} cell =
  let loc = Env.loc env in
  match cell with
  | DataStack.Const ndx -> (
      let const = co_consts.(ndx) in
      match FFI.Constant.to_exp const with
      | None ->
          (env, Error "[load_cell] Constant contains code objects")
      | Some exp_ty ->
          (env, Ok exp_ty) )
  | DataStack.Name ndx ->
      let name = global co_names.(ndx) in
      let env, id = Env.temp env in
      let exp = T.Exp.Lvar (var_name ~loc name) in
      let loc = Env.loc env in
      let instr = T.Instr.Load {id; exp; typ= pyObject; loc} in
      let env = Env.push_instr env instr in
      (* TODO: try to trace the type of names ? *)
      (env, Ok (T.Exp.Var id, PyCommon.pyObject))
  | DataStack.Temp id ->
      (* TODO: try to trace the type of ids ? *)
      (env, Ok (T.Exp.Var id, PyCommon.pyObject))


let pop_tos opname env =
  match Env.pop env with
  | None ->
      L.die ExternalError "[%s] stack is empty" opname
  | Some (env, cell) ->
      (env, cell)


module LOAD_CONST = struct
  (* LOAD_CONST(consti)
     Pushes co_consts[consti] onto the stack. *)

  let run env FFI.Code.{co_consts} FFI.Instruction.{opname; arg} =
    let const = co_consts.(arg) in
    Debug.p "[%s] arg = %s\n" opname (FFI.Constant.show const) ;
    let cell = DataStack.Const arg in
    (Env.push env cell, None)
end

module LOAD_NAME = struct
  (* LOAD_NAME(namei)
     Pushes the value associated with co_names[namei] onto the stack. *)

  let run env FFI.Code.{co_names} FFI.Instruction.{opname; arg} =
    let name = co_names.(arg) in
    Debug.p "[%s] arg = %s\n" opname name ;
    let cell = DataStack.Name arg in
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

  let run env FFI.Code.({co_names} as code) FFI.Instruction.{opname; arg} =
    let name = global co_names.(arg) in
    Debug.p "[%s] name = %s\n" opname name ;
    let loc = Env.loc env in
    let var_name = var_name ~loc name in
    let env = Env.register_global env var_name in
    let env, cell = pop_tos opname env in
    let env, exp_ty = load_cell env code cell in
    match exp_ty with
    | Ok (exp, typ) ->
        let instr = T.Instr.Store {exp1= T.Exp.Lvar var_name; typ; exp2= exp; loc} in
        (Env.push_instr env instr, None)
    | Error s ->
        L.die InternalError "[%s] %s" opname s
end

module RETURN_VALUE = struct
  (* RETURN_VALUE
     returns the top-of-stack *)
  let run env code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, cell = pop_tos opname env in
    let env, exp_ty = load_cell env code cell in
    match exp_ty with
    | Ok (exp, _) ->
        let term = T.Terminator.Ret exp in
        (env, Some term)
    | Error s ->
        L.die InternalError "[%s] %s" opname s
end

module POP_TOP = struct
  (* POP_TOP
     pop the top-of-stack and discard it *)
  let run env _code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, _cell = pop_tos opname env in
    (env, None)
end

module CALL_FUNCTION = struct
  (* CALL_FUNCTION(argc)
     Calls a callable object with positional arguments. argc indicates the number of positional
     arguments. The top of the stack contains positional arguments, with the right-most argument
     on top. Below the arguments is a callable object to call. This opcode pushes a fresh result
     on the top of the stack.

     Before: argN | ... | arg1 | arg0 | code-object | rest-of-the-stack
     After:  result | rest-of-the-stack
  *)

  let pop_n_tos opname code =
    let rec pop env n acc =
      if n > 0 then (
        let env, cell = pop_tos opname env in
        Debug.p "  popped %s\n" (DataStack.show_cell cell) ;
        let env, exp_ty = load_cell env code cell in
        match exp_ty with
        | Ok (exp, _) ->
            pop env (n - 1) (exp :: acc)
        | Error s ->
            L.die UserError "[%s] failed to fetch from the stack: %s" opname s )
      else (env, acc)
    in
    Debug.p "[pop_n_tos]\n" ;
    pop


  let run env FFI.Code.({co_names} as code) FFI.Instruction.{opname; arg} =
    Debug.p "[%s] argc = %d\n" opname arg ;
    let env, args = pop_n_tos opname code env arg [] in
    Debug.p "  #args = %d\n" (List.length args) ;
    let env, fname = pop_tos opname env in
    Debug.p "  fname = %s\n" (DataStack.show_cell fname) ;
    let fname =
      match fname with
      | DataStack.Name ndx ->
          co_names.(ndx)
      | DataStack.Const _ | DataStack.Temp _ ->
          L.die UserError "[%s] invalid function on the stack: %s" opname
            (DataStack.show_cell_kind fname)
    in
    let env, id = Env.temp env in
    let loc = Env.loc env in
    let env, proc =
      if PyBuiltins.is_builtin fname then
        let env = Env.register_builtin env fname in
        (env, PyCommon.builtin_name fname)
      else (env, qualified_procname @@ proc_name ~loc fname)
    in
    let call = T.Exp.Call {proc; args; kind= T.Exp.NonVirtual} in
    let let_instr = T.Instr.Let {id; exp= call; loc} in
    let env = Env.push_instr env let_instr in
    let env = Env.push env (DataStack.Temp id) in
    (env, None)
end

module BINARY_ADD = struct
  (* BINARY_ADD
     Implements top-of-stack = top-of-stack1 + top-of-stack.

     Before: TOS (rhs) | TOS1 (lhs) | rest-of-stack
     After: TOS1 + TOS (lhs + rhs) | rest-of-stack

     Since Python is using runtime types to know which `+` to do (addition, string concatenation,
     custom operator, ...), we'll need to write a model for this one. *)
  let run env code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, tos = pop_tos opname env in
    let env, tos1 = pop_tos opname env in
    let env, lhs = load_cell env code tos1 in
    let lhs =
      match lhs with Ok (lhs, _) -> lhs | Error s -> L.die InternalError "[%s] %s" opname s
    in
    let env, rhs = load_cell env code tos in
    let rhs =
      match rhs with Ok (rhs, _) -> rhs | Error s -> L.die InternalError "[%s] %s" opname s
    in
    let fname = "binary_add" in
    let env = Env.register_builtin env fname in
    let env, id = Env.temp env in
    let proc = PyCommon.builtin_name fname in
    (* Even if the call can be considered as virtual because, it's logic is not symetric. Based
       on what I gathered, like in [0], I think the best course of action is to write a model for
       it and leave it non virtual. TODO: ask David.

       [0]:
       https://stackoverflow.com/questions/58828522/is-radd-called-if-add-raises-notimplementederror
    *)
    let exp = T.Exp.Call {proc; args= [lhs; rhs]; kind= T.Exp.NonVirtual} in
    let loc = Env.loc env in
    let let_instr = T.Instr.Let {id; exp; loc} in
    let env = Env.push_instr env let_instr in
    let env = Env.push env (DataStack.Temp id) in
    (env, None)
end

let run_instruction env code FFI.Instruction.({opname; starts_line} as instr) =
  let env = Env.starts_line env starts_line in
  (* TODO: there are < 256 opcodes, we could setup an array of callbacks instead *)
  let env, maybe_term =
    match opname with
    | "LOAD_CONST" ->
        LOAD_CONST.run env code instr
    | "LOAD_NAME" ->
        LOAD_NAME.run env code instr
    | "STORE_NAME" ->
        STORE_NAME.run env code instr
    | "RETURN_VALUE" ->
        RETURN_VALUE.run env code instr
    | "POP_TOP" ->
        POP_TOP.run env code instr
    | "CALL_FUNCTION" ->
        CALL_FUNCTION.run env code instr
    | "BINARY_ADD" ->
        BINARY_ADD.run env code instr
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


let node env (T.NodeName.{loc= label_loc} as label) FFI.Code.({instructions} as code) =
  let env = Env.mk_node env in
  let env, maybe_term, rest = run env code instructions in
  let last_loc = Env.loc env in
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
          ; last_loc
          ; label_loc }
      in
      (env.Env.shared, rest, node)


let first_loc_of_code FFI.Code.{instructions} =
  match instructions with
  | {starts_line= Some line} :: _ ->
      T.Location.known ~line ~col:0
  | _ ->
      T.Location.Unknown


let to_proc_desc env name code =
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
  let loc = first_loc_of_code code in
  let label = node_name ~loc "b0" in
  let env = Env.reset_idents env in
  let env, remaining_instructions, node = node env label code in
  if not (List.is_empty remaining_instructions) then
    L.die InternalError "%d instructions are left" (List.length remaining_instructions) ;
  ( env
  , Textual.ProcDesc.
      { procdecl
      ; nodes= [node]
      ; start= label
      ; params= []
      ; locals= []
      ; exit_loc= Textual.Location.Unknown } )


let python_attribute = Textual.Attr.mk_source_language Textual.Lang.Python

let to_module ~sourcefile module_name code =
  let loc = first_loc_of_code code in
  let name = proc_name ~loc module_name in
  let env = Env.empty in
  let env, decl = to_proc_desc env name code in
  let globals =
    T.VarName.Set.fold
      (fun name acc ->
        let global = T.Global.{name; typ= pyObject; attributes= []} in
        T.Module.Global global :: acc )
      env.Env.globals []
  in
  let decls = (T.Module.Proc decl :: globals) @ PyBuiltins.to_textual env.Env.builtins in
  T.Module.{attrs= [python_attribute]; decls; sourcefile}
