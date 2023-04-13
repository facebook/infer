(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module T = Textual
module PyBuiltins = PyCommon.Builtins

(* In Python, everything is an object, and the interpreter maintains a stack of references to
   such objects. Pushing and popping on the stack are always references to objets that leave in a
   heap. There is no need to model this heap, but the data stack is quite important. *)
module DataStack = struct
  type cell =
    | Const of int  (** index in [co_consts] *)
    | Name of int  (** reference to a global name, stored in [co_names] *)
    | VarName of int  (** reference to a local name, stored in [co_varnames] *)
    | Temp of T.Ident.t  (** SSA variable *)
    | Fun of string  (** top level user-defined function name *)
  [@@deriving show]

  let as_code FFI.Code.{co_consts} = function
    | Const n ->
        let code = co_consts.(n) in
        FFI.Constant.as_code code
    | Name _ | Temp _ | Fun _ | VarName _ ->
        None


  let show_cell_kind = function
    | Const _ ->
        "DataStack.Const"
    | Name _ ->
        "DataStack.Name"
    | VarName _ ->
        "DataStack.VarName"
    | Temp _ ->
        "DataStack.Temp"
    | Fun _ ->
        "DataStack.Fun"


  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)
end

module Env = struct
  (** Part of the environment shared by most structures. It gathers information like which builtin
      has been spotted, or what idents have been generated so far. *)
  type shared = {idents: T.Ident.Set.t; globals: T.VarName.Set.t; builtins: PyBuiltins.t}

  (* TODO(vsiles): revisit the data stack status once generators and conditions are in the mix *)

  (** State of the capture while processing a single node: each node has a dedicated data stack, and
      generates its own set of instructions. *)
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

  (* Inspired by PulseFormula.Debug. Check there for plugging it into Logging too *)
  let dummy_formatter = F.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let p fmt =
    if debug then F.kasprintf (fun s -> F.printf "%s" s) fmt else F.ifprintf dummy_formatter fmt
end

let var_name ?(loc = T.Location.Unknown) value = T.VarName.{value; loc}

let node_name ?(loc = T.Location.Unknown) value = T.NodeName.{value; loc}

let proc_name ?(loc = T.Location.Unknown) value = T.ProcName.{value; loc}

(* TODO: only deal with toplevel functions for now *)
let qualified_procname name : T.qualified_procname = {enclosing_class= TopLevel; name}

let global name = sprintf "$globals::%s" name

(* Until there is support for python types, everything is a *Object *)
let pyObject = PyCommon.pyObject

(* Python only stores references to objects on the data stack, so when data needs to be really
   accessed, [load_cell] is used to get information from the code information ([co_consts], ...).
   These data are mapped to Textual.Exp.t values as much as possible. But it's not always
   desirable (see MAKE_FUNCTION) *)
let load_cell env {FFI.Code.co_consts; co_names; co_varnames} cell =
  let loc = Env.loc env in
  match cell with
  | DataStack.Const ndx -> (
      let const = co_consts.(ndx) in
      match FFI.Constant.to_exp const with
      | None ->
          (env, `Error "[load_cell] Constant contains code objects")
      | Some exp_ty ->
          (env, `Ok exp_ty) )
  | DataStack.Name ndx ->
      let name = global co_names.(ndx) in
      let env, id = Env.temp env in
      let exp = T.Exp.Lvar (var_name ~loc name) in
      let loc = Env.loc env in
      let instr = T.Instr.Load {id; exp; typ= pyObject; loc} in
      let env = Env.push_instr env instr in
      (* TODO: try to trace the type of names ? *)
      (env, `Ok (T.Exp.Var id, PyCommon.pyObject))
  | DataStack.VarName ndx ->
      let name = co_varnames.(ndx) in
      let env, id = Env.temp env in
      let exp = T.Exp.Lvar (var_name ~loc name) in
      let loc = Env.loc env in
      let instr = T.Instr.Load {id; exp; typ= pyObject; loc} in
      let env = Env.push_instr env instr in
      (* TODO: try to trace the type of names ? *)
      (env, `Ok (T.Exp.Var id, PyCommon.pyObject))
  | DataStack.Temp id ->
      (* TODO: try to trace the type of ids ? *)
      (env, `Ok (T.Exp.Var id, PyCommon.pyObject))
  | DataStack.Fun f ->
      (env, `Fun f)


let pop_tos opname env =
  match Env.pop env with
  | None ->
      L.die ExternalError "[%s] stack is empty" opname
  | Some (env, cell) ->
      (env, cell)


(* Python opcodes support. Most of the documentation directly comes from the official python
   documentation and is only altered to improve readability.

   https://docs.python.org/3.8/library/dis.html *)

(* Encoding of the LOAD_* bytecode operations *)
module LOAD = struct
  type kind =
    | CONST  (** {v LOAD_CONST(consti) v}

                 Pushes [co_consts\[consti\]] onto the stack. *)
    | FAST
        (** {v LOAD_FAST(var_num) v}

            Pushes a reference to the local [co_varnames\[var_num\]] onto the stack. *)
    | GLOBAL
        (** {v LOAD_GLOBAL(namei) v}

            Loads the global named [co_names\[namei\]] onto the stack. *)
    | NAME
        (** {v LOAD_NAME(namei) v}

            Pushes the value associated with [co_names\[namei\]] onto the stack. *)

  let run kind env code FFI.Instruction.{opname; arg} =
    let pp {FFI.Code.co_names; co_varnames; co_consts} fmt = function
      | CONST ->
          FFI.Constant.pp fmt co_consts.(arg)
      | FAST ->
          F.fprintf fmt "%s" co_varnames.(arg)
      | NAME | GLOBAL ->
          F.fprintf fmt "%s" co_names.(arg)
    in
    let cell =
      match kind with
      | CONST ->
          DataStack.Const arg
      | FAST ->
          DataStack.VarName arg
      | NAME | GLOBAL ->
          DataStack.Name arg
    in
    Debug.p "[%s] arg = %a\n" opname (pp code) kind ;
    (Env.push env cell, None)
end

(* Encoding of the STORE_* bytecode operations *)
module STORE = struct
  type kind =
    | FAST
        (** {v STORE_FAST(var_num) v}

            Stores top-of-stack into the local [co_varnames\[var_num\]]. *)
    | NAME
        (** {v STORE_NAME(namei) v}

            Implements name = top-of-stack. namei is the index of name in the attribute co_names of
            the code object. The compiler tries to use [STORE_FAST] or [STORE_GLOBAL] if possible.

            Notes: this should only happen in global nodes, to update global variables from the
            global scope.

            In a function, local varialbes are updated using [STORE_FAST], and global variables are
            updated using [STORE_GLOBAL]. *)
    | GLOBAL
        (** {v STORE_GLOBAL(namei) v}

            Works as [STORE_NAME], but stores the name as a global.

            Since there is a special namespace for global varialbes, this is in fact the same as
            [STORE_NAME], but only called from within a function/method. *)

  let run kind env FFI.Code.({co_names; co_varnames} as code) FFI.Instruction.{opname; arg} =
    let name, is_global =
      match kind with
      | FAST ->
          (co_varnames.(arg), false)
      | NAME | GLOBAL ->
          (global co_names.(arg), true)
    in
    Debug.p "[%s] name = %s\n" opname name ;
    let loc = Env.loc env in
    let var_name = var_name ~loc name in
    let env, cell = pop_tos opname env in
    let env, exp_ty = load_cell env code cell in
    match exp_ty with
    | `Ok (exp, typ) ->
        let env = if is_global then Env.register_global env var_name else env in
        let instr = T.Instr.Store {exp1= Lvar var_name; typ; exp2= exp; loc} in
        (Env.push_instr env instr, None)
    | `Error s ->
        L.die InternalError "[%s] %s" opname s
    | `Fun f ->
        if is_global then (
          Debug.p "  top-level function defined: %s" f ;
          (env, None) )
        else L.die InternalError "[%s] no support for closure at the moment: %s" opname f
end

module RETURN_VALUE = struct
  (** {v RETURN_VALUE v}

      Returns the top-of-stack *)
  let run env code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, cell = pop_tos opname env in
    let env, exp_ty = load_cell env code cell in
    match exp_ty with
    | `Ok (exp, _) ->
        let term = T.Terminator.Ret exp in
        (env, Some term)
    | `Error s ->
        L.die InternalError "[%s] %s" opname s
    | `Fun f ->
        L.die InternalError "[%s] can't support returning closure: %s" opname f
end

module POP_TOP = struct
  (** {v POP_TOP v}

      Pop the top-of-stack and discard it *)
  let run env _code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, _cell = pop_tos opname env in
    (env, None)
end

module CALL_FUNCTION = struct
  (** {v CALL_FUNCTION(argc) v}

      Calls a callable object with positional arguments. [argc] indicates the number of positional
      arguments. The top of the stack contains positional arguments, with the right-most argument on
      top. Below the arguments is a callable object to call. This opcode pushes a fresh result on
      the top of the stack.

      Before: [ argN | ... | arg1 | arg0 | code-object | rest-of-the-stack ]

      After: [ result | rest-of-the-stack v} ] *)

  let pop_n_tos opname code =
    let rec pop env n acc =
      if n > 0 then (
        let env, cell = pop_tos opname env in
        Debug.p "  popped %s\n" (DataStack.show_cell cell) ;
        let env, exp_ty = load_cell env code cell in
        match exp_ty with
        | `Ok (exp, _) ->
            pop env (n - 1) (exp :: acc)
        | `Fun f ->
            L.die InternalError "[%s] failed to get closure as function argument: %s" opname f
        | `Error s ->
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
      | VarName _ | Fun _ | Const _ | Temp _ ->
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
  (** {v BINARY_ADD v}

      Implements top-of-stack = top-of-stack1 + top-of-stack.

      Before: [ TOS (rhs) | TOS1 (lhs) | rest-of-stack ]

      After: [ TOS1 + TOS (lhs + rhs) | rest-of-stack ]

      Since Python is using runtime types to know which [+] to do (addition, string concatenation,
      custom operator, ...), we'll need to write a model for this one. *)
  let run env code FFI.Instruction.{opname} =
    Debug.p "[%s]\n" opname ;
    let env, tos = pop_tos opname env in
    let env, tos1 = pop_tos opname env in
    let env, lhs = load_cell env code tos1 in
    let lhs =
      match lhs with
      | `Ok (lhs, _) ->
          lhs
      | `Error s ->
          L.die InternalError "[%s] %s" opname s
      | `Fun f ->
          L.die InternalError "[%s] Can't add function %s" opname f
    in
    let env, rhs = load_cell env code tos in
    let rhs =
      match rhs with
      | `Ok (rhs, _) ->
          rhs
      | `Error s ->
          L.die InternalError "[%s] %s" opname s
      | `Fun f ->
          L.die InternalError "[%s] Can't add function %s" opname f
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

module MAKE_FUNCTION = struct
  (** {v MAKE_FUNCTION(flags) v}

      Pushes a new function object on the stack. From bottom to top, the consumed stack must consist
      of values if the argument carries a specified flag value

      - [0x01] a tuple of default values for positional-only and positional-or-keyword parameters in
        positional order
      - [0x02] a dictionary of keyword-only parameters’ default values
      - [0x04] an annotation dictionary
      - [0x08] a tuple containing cells for free variables, making a closure
      - the code associated with the function (at TOS1)
      - the qualified name of the function (at TOS)

      In this first version, only support for [flags = 0x00] is implemented. Also there is no
      support for closures or nested functions *)
  let run env FFI.Code.({co_consts} as code) FFI.Instruction.{opname; arg} =
    Debug.p "[%s] flags = 0x%x\n" opname arg ;
    if arg <> 0 then L.die InternalError "%s: support for flag 0x%x is not implemented" opname arg ;
    let env, qual = pop_tos opname env in
    (* don't care about the content of the code object, but we'll check it is indeed code *)
    let env, body = pop_tos opname env in
    let body =
      match DataStack.as_code code body with
      | None ->
          L.die InternalError "%s: payload is not code: %s" opname (DataStack.show_cell_kind body)
      | Some body ->
          body
    in
    if FFI.Code.is_closure body then L.die InternalError "%s: can't create closure" opname ;
    let qual =
      match qual with
      | DataStack.(VarName _ | Name _ | Temp _ | Fun _) ->
          L.die InternalError "%s: invalid function name: %s" opname (DataStack.show_cell_kind qual)
      | DataStack.Const ndx -> (
          let const = co_consts.(ndx) in
          match FFI.Constant.as_name const with
          | Some name ->
              name
          | None ->
              L.die InternalError "%s: can't read qualified name from stack: %s" opname
                (FFI.Constant.show const) )
    in
    let env = Env.push env (DataStack.Fun qual) in
    (env, None)
end

let run_instruction env code ({FFI.Instruction.opname; starts_line} as instr) =
  let env = Env.starts_line env starts_line in
  (* TODO: there are < 256 opcodes, could setup an array of callbacks instead *)
  let env, maybe_term =
    match opname with
    | "LOAD_CONST" ->
        LOAD.(run CONST env code instr)
    | "LOAD_FAST" ->
        LOAD.(run FAST env code instr)
    | "LOAD_GLOBAL" ->
        LOAD.(run GLOBAL env code instr)
    | "LOAD_NAME" ->
        LOAD.(run NAME env code instr)
    | "STORE_FAST" ->
        STORE.(run FAST env code instr)
    | "STORE_GLOBAL" ->
        STORE.(run GLOBAL env code instr)
    | "STORE_NAME" ->
        STORE.(run NAME env code instr)
    | "RETURN_VALUE" ->
        RETURN_VALUE.run env code instr
    | "POP_TOP" ->
        POP_TOP.run env code instr
    | "CALL_FUNCTION" ->
        CALL_FUNCTION.run env code instr
    | "BINARY_ADD" ->
        BINARY_ADD.run env code instr
    | "MAKE_FUNCTION" ->
        MAKE_FUNCTION.run env code instr
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


(* TODO: No support for jumps and conditionals for now, so it's pretty straightforward *)

(** Process the instructions of a code object up to the point where a * terminator is reached. *)
let node env ({T.NodeName.loc= label_loc} as label) ({FFI.Code.instructions} as code) =
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


(** Process a single code unit (toplevel code, function body, ...) *)
let to_proc_desc env name ({FFI.Code.co_argcount; co_varnames} as code) =
  let qualified_name = qualified_procname name in
  let pyObject = T.Typ.{typ= pyObject; attributes= []} in
  let loc = first_loc_of_code code in
  let label = node_name ~loc "b0" in
  let nr_varnames = Array.length co_varnames in
  let params = Array.sub co_varnames ~pos:0 ~len:co_argcount in
  let locals = Array.sub co_varnames ~pos:co_argcount ~len:(nr_varnames - co_argcount) in
  let params = Array.map ~f:(var_name ~loc) params |> Array.to_list in
  let locals = Array.map ~f:(fun name -> (var_name ~loc name, pyObject)) locals |> Array.to_list in
  let procdecl =
    T.ProcDecl.
      { qualified_name
      ; formals_types= List.map ~f:(fun _ -> pyObject) params
      ; are_formal_types_fully_declared= true
      ; result_type= pyObject
      ; attributes= [] }
  in
  let env = Env.reset_idents env in
  let env, remaining_instructions, node = node env label code in
  if not (List.is_empty remaining_instructions) then
    L.die InternalError "%d instructions are left" (List.length remaining_instructions) ;
  ( env
  , Textual.ProcDesc.
      {procdecl; nodes= [node]; start= label; params; locals; exit_loc= Textual.Location.Unknown} )


(* TODO: No support for nested functions/methods at the moment *)

(** Process multiple code objects. Usually called by the toplevel function. *)
let to_proc_descs env codes =
  Array.fold codes ~init:(env, []) ~f:(fun (env, decls) const ->
      match FFI.Constant.as_code const with
      | None ->
          (env, decls)
      | Some FFI.Code.({co_name} as code) ->
          let loc = first_loc_of_code code in
          let name = proc_name ~loc co_name in
          let env, decl = to_proc_desc env name code in
          (env, T.Module.Proc decl :: decls) )


let python_attribute = Textual.Attr.mk_source_language Textual.Lang.Python

(** Entry point of the module: process a whole Python file / compilation unit into Textual *)
let to_module ~sourcefile module_name ({FFI.Code.co_consts} as code) =
  let env = Env.empty in
  (* First, process any code body that is in code.co_consts *)
  let env, decls = to_proc_descs env co_consts in
  (* Process top level module *)
  let loc = first_loc_of_code code in
  let name = proc_name ~loc module_name in
  let env, decl = to_proc_desc env name code in
  (* Translate globals to Textual *)
  let globals =
    T.VarName.Set.fold
      (fun name acc ->
        let global = T.Global.{name; typ= pyObject; attributes= []} in
        T.Module.Global global :: acc )
      env.Env.globals []
  in
  (* Gather everything into a Textual module *)
  let decls = ((T.Module.Proc decl :: decls) @ globals) @ PyBuiltins.to_textual env.Env.builtins in
  T.Module.{attrs= [python_attribute]; decls; sourcefile}
