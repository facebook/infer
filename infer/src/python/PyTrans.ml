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

module Builtins = Caml.Set.Make (struct
  type t = string [@@deriving compare]
end)

(* TODO: maybe split the env into the RO part and the moving part *)
module Env = struct
  type t =
    { stack: DataStack.t
    ; idents: T.Ident.Set.t
    ; globals: T.VarName.Set.t
    ; builtins: Builtins.t
    ; spotted_builtins: Builtins.t
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

  let register_builtin ({spotted_builtins} as env) name =
    {env with spotted_builtins= Builtins.add name spotted_builtins}
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

let node_name value = T.NodeName.{value; loc= T.Location.Unknown}

let proc_name value = T.ProcName.{value; loc= T.Location.Unknown}

(* TODO: We only deal with toplevel functions for now *)
let qualified_procname name : T.qualified_procname = {enclosing_class= TopLevel; name}

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
    let var_name = var_name name in
    let env = Env.register_global env var_name in
    let env, cell = pop_tos opname env in
    let env, exp = load_cell env code cell in
    match exp with
    | Ok exp ->
        let instr =
          T.Instr.Store
            {exp1= T.Exp.Lvar var_name; typ= pyObject; exp2= exp; loc= T.Location.Unknown}
        in
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
    let _env, exp = load_cell env code cell in
    match exp with
    | Ok exp ->
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
        let env, exp = load_cell env code cell in
        match exp with
        | Ok exp ->
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
    let is_builtin = Builtins.mem fname env.Env.builtins in
    let env, proc =
      if is_builtin then
        let env = Env.register_builtin env fname in
        (env, FFI.builtin_name fname)
      else (env, qualified_procname @@ proc_name fname)
    in
    let call = Textual.Exp.Call {proc; args; kind= Textual.Exp.NonVirtual} in
    let let_instr = Textual.Instr.Let {id; exp= call; loc= Textual.Location.Unknown} in
    let env = Env.push_instr env let_instr in
    let env = Env.push env (DataStack.Temp id) in
    (env, None)
end

let run_instruction env code FFI.Instruction.({opname} as instr) =
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


let node globals builtins spotted_builtins idents label FFI.Code.({instructions} as code) =
  let env = Env.{stack= []; idents; globals; builtins; spotted_builtins; instructions= []} in
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
      (env.Env.idents, env.Env.globals, env.Env.spotted_builtins, rest, node)


let to_proc_desc name globals builtins spotted_builtins code =
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
  let _idents, globals, spotted_builtins, remaining_instructions, node =
    node globals builtins spotted_builtins idents label code
  in
  if not (List.is_empty remaining_instructions) then
    L.die InternalError "%d instructions are left" (List.length remaining_instructions) ;
  ( globals
  , spotted_builtins
  , Textual.ProcDesc.
      { procdecl
      ; nodes= [node]
      ; start= label
      ; params= []
      ; locals= []
      ; exit_loc= Textual.Location.Unknown } )


let std_builtins builtins =
  let annot typ = T.Typ.{typ; attributes= []} in
  let mk ?typ qualified_name =
    let formals_types = Option.to_list @@ Option.map ~f:annot typ in
    let result_type = annot pyObject in
    T.Module.Procdecl
      T.ProcDecl.
        { qualified_name
        ; formals_types
        ; are_formal_types_fully_declared= true
        ; result_type
        ; attributes= [] }
  in
  let python_int = mk FFI.python_int ~typ:Textual.Typ.Int in
  let python_string = mk FFI.python_string ~typ:pyObject in
  let python_tuple = mk FFI.python_tuple ~typ:pyObject in
  Builtins.fold
    (fun name acc ->
      let builtin_name = FFI.builtin_name name in
      mk builtin_name :: acc )
    builtins
    [python_int; python_string; python_tuple]


let python_attribute = Textual.Attr.mk_source_language Textual.Lang.Python

let to_module ~sourcefile module_name code =
  let name = proc_name module_name in
  let globals = T.VarName.Set.empty in
  let spotted_builtins = Builtins.empty in
  let builtins = Builtins.add "print" spotted_builtins in
  let globals, spotted_builtins, decl = to_proc_desc name globals builtins spotted_builtins code in
  let globals =
    T.VarName.Set.fold
      (fun name acc ->
        let global = T.Global.{name; typ= pyObject; attributes= []} in
        T.Module.Global global :: acc )
      globals []
  in
  let decls = (T.Module.Proc decl :: globals) @ std_builtins spotted_builtins in
  T.Module.{attrs= [python_attribute]; decls; sourcefile}
