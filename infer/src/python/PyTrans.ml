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
module Debug = PyDebug
module Env = PyEnv
module DataStack = PyEnv.DataStack
module Builtin = PyEnv.Builtin

let var_name ?(loc = T.Location.Unknown) value = {T.VarName.value; loc}

let node_name ?(loc = T.Location.Unknown) value = {T.NodeName.value; loc}

let proc_name ?(loc = T.Location.Unknown) value = {T.ProcName.value; loc}

let type_name ?(loc = T.Location.Unknown) value = {T.TypeName.value; loc}

(* TODO: only deal with toplevel functions for now *)
let qualified_procname ?(enclosing_class = T.TopLevel) name : T.qualified_procname =
  {enclosing_class; name}


(** Turn a Python [FFI.Code.t] into a Textual [T.Exp.t], along with the required instructions to
    effectively perform the translation. *)
let code_to_exp ~fun_or_class env name =
  let kind = if fun_or_class then Builtin.PythonCode else Builtin.PythonClass in
  let name = T.Exp.Const (Str name) in
  let env, id, typ = Env.mk_builtin_call env kind [name] in
  (env, T.Exp.Var id, typ)


(** Turn a Python [FFI.Constant.t] into a Textual [T.Exp.t], along with the required instructions to
    effectively perform the translation. *)
let rec py_to_exp env c =
  match (c : FFI.Constant.t) with
  | PYCBool b ->
      (env, PyCommon.mk_bool b, PyCommon.pyBool)
  | PYCInt i ->
      (env, PyCommon.mk_int i, PyCommon.pyInt)
  | PYCString s ->
      (env, PyCommon.mk_string s, PyCommon.pyString)
  | PYCNone ->
      let exp = T.(Exp.Const Const.Null) in
      (env, exp, T.Typ.Null)
  | PYCCode c ->
      (* We assume it's a function, as classes should only be seen during loading using
         [LOAD_BUILD_CLASS] *)
      code_to_exp env ~fun_or_class:true c.FFI.Code.co_name
  | PYCTuple arr ->
      let l = Array.to_list arr in
      let env, args =
        Env.map ~env l ~f:(fun env c ->
            let env, e, _ = py_to_exp env c in
            (env, e) )
      in
      let exp = T.Exp.Call {proc= PyCommon.python_tuple; args; kind= NonVirtual} in
      (env, exp, PyCommon.pyObject)


(** Try to load the data referenced by a [DataStack.cell], into a [Textual.Exp.t] *)
let load_cell env {FFI.Code.co_consts; co_names; co_varnames} cell =
  let info typ = {Env.typ; is_code= false; is_class= false} in
  let default = info PyCommon.pyObject in
  (* Python only stores references to objects on the data stack, so when data needs to be really
     accessed, [load_cell] is used to get information from the code information ([co_consts], ...).
     These data are mapped to Textual.Exp.t values as much as possible. But it's not always
     desirable (see MAKE_FUNCTION) *)
  let loc = Env.loc env in
  match (cell : DataStack.cell) with
  | Const ndx ->
      let const = co_consts.(ndx) in
      let env, exp, ty = py_to_exp env const in
      let info = info ty in
      (env, `Ok exp, info)
  | Name ndx ->
      let name = PyCommon.global co_names.(ndx) in
      let var_name = var_name ~loc name in
      let ({Env.typ; is_code} as info) =
        T.VarName.Map.find_opt var_name (Env.globals env) |> Option.value ~default
      in
      (* If we are trying to load some code, use the dedicated builtin *)
      if is_code then
        let env, exp, typ = code_to_exp ~fun_or_class:true env name in
        let info = {info with Env.typ} in
        (env, `Ok exp, info)
      else
        let exp = T.Exp.Lvar var_name in
        let loc = Env.loc env in
        let env, id = Env.mk_fresh_ident env info in
        let instr = T.Instr.Load {id; exp; typ; loc} in
        let env = Env.push_instr env instr in
        (* TODO: try to trace the type of names, not only global ones ? *)
        (env, `Ok (T.Exp.Var id), info)
  | VarName ndx ->
      let name = co_varnames.(ndx) in
      let exp = T.Exp.Lvar (var_name ~loc name) in
      let loc = Env.loc env in
      let typ = PyCommon.pyObject in
      let info = info typ in
      let env, id = Env.mk_fresh_ident env info in
      let instr = T.Instr.Load {id; exp; typ; loc} in
      let env = Env.push_instr env instr in
      (* TODO: try to trace the type of names and their kind ? *)
      (env, `Ok (T.Exp.Var id), info)
  | Temp id ->
      let info = Env.get_ident_info env id |> Option.value ~default in
      (env, `Ok (T.Exp.Var id), info)
  | Code {fun_or_class; code} ->
      let env, exp, typ = code_to_exp env ~fun_or_class code.FFI.Code.co_name in
      let info = {Env.typ; is_code= true; is_class= false} in
      (env, `Ok exp, info)
  | Map _map ->
      (env, `Error "TODO: load Map cells", default)
  | BuiltinBuildClass ->
      L.die InternalError "[load_cell] Can't load LOAD_BUILD_CLASS, something went wrong"


(** Pop the top of the datastack. Fails with an [InternalError] if the stack is empty. *)
let pop_tos opname env =
  match Env.pop env with
  | None ->
      L.die ExternalError "[%s] stack is empty" opname
  | Some (env, cell) ->
      (env, cell)


(* Python opcodes support. Most of the documentation directly comes from the official python
   documentation and is only altered to improve readability.

   https://docs.python.org/3.8/library/dis.html *)

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

  let run kind env code {FFI.Instruction.opname; arg} =
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


  module BUILD_CLASS = struct
    (** {v LOAD_BUILD_CLASS v}

        Pushes [builtins.__build_class__()] onto the stack. It is later called by [CALL_FUNCTION] to
        construct a class. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let env = Env.push env DataStack.BuiltinBuildClass in
      (env, None)
  end
end

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

  let run kind env ({FFI.Code.co_names; co_varnames} as code) {FFI.Instruction.opname; arg} =
    let name, is_global =
      match kind with
      | FAST ->
          (co_varnames.(arg), false)
      | NAME | GLOBAL ->
          (PyCommon.global co_names.(arg), true)
    in
    Debug.p "[%s] name = %s\n" opname name ;
    let loc = Env.loc env in
    let var_name = var_name ~loc name in
    let env, cell = pop_tos opname env in
    let env, exp, info = load_cell env code cell in
    match exp with
    | `Ok exp ->
        let {Env.typ; is_code; is_class} = info in
        if is_class then (
          Debug.p "  top-level class declaration initialized\n" ;
          (env, None) )
        else
          let env = if is_global then Env.register_global env var_name info else env in
          if is_code then
            if is_global then (
              Debug.p "  top-level function defined\n" ;
              (env, None) )
            else L.die InternalError "[%s] no support for closure at the moment: %s" opname name
          else
            let instr = T.Instr.Store {exp1= Lvar var_name; typ; exp2= exp; loc} in
            (Env.push_instr env instr, None)
    | `Error s ->
        L.die InternalError "[%s] %s" opname s
end

module POP_TOP = struct
  (** {v POP_TOP v}

      Pop the top-of-stack and discard it *)
  let run env {FFI.Instruction.opname} =
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

      After: [ result | rest-of-the-stack ] *)

  let pop_n_tos opname =
    let rec pop env n acc =
      if n > 0 then (
        let env, cell = pop_tos opname env in
        Debug.p "  popped %s\n" (DataStack.show_cell cell) ;
        pop env (n - 1) (cell :: acc) )
      else (env, acc)
    in
    Debug.p "[pop_n_tos]\n" ;
    pop


  let to_textual env opname code cells =
    Env.map ~env cells ~f:(fun env cell ->
        let env, exp, _ = load_cell env code cell in
        match exp with
        | `Ok exp ->
            (env, exp)
        | `Error s ->
            L.die UserError "[%s] failed to fetch from the stack: %s" opname s )


  let static_call env fname args =
    let loc = Env.loc env in
    let classes = Env.get_classes env in
    if List.mem ~equal:String.equal classes fname then
      let name = T.Exp.Const (T.Const.Str fname) in
      (* Calling a class constructor *)
      (* FYI, Python3 constructors first call __new__ to allocate the object, and then call
         __init__ to initialize it. Both can be overloaded in the user-declared code. *)
      let env, id, _typ = Env.mk_builtin_call env Builtin.PythonClassConstructor (name :: args) in
      let env = Env.push env (DataStack.Temp id) in
      (env, None)
    else
      let env, proc =
        let env = Env.register_call env fname in
        if Env.is_builtin env fname then (env, PyCommon.builtin_name fname)
        else
          let fname = PyCommon.global fname in
          (* TODO: update this when static method calls will be available *)
          (env, qualified_procname @@ proc_name ~loc fname)
      in
      let call = T.Exp.Call {proc; args; kind= NonVirtual} in
      (* TODO: read return type of proc if available *)
      let info = {Env.typ= PyCommon.pyObject; is_code= false; is_class= false} in
      let env, id = Env.mk_fresh_ident env info in
      let let_instr = T.Instr.Let {id; exp= call; loc} in
      let env = Env.push_instr env let_instr in
      let env = Env.push env (DataStack.Temp id) in
      (env, None)


  let dynamic_call env caller_id args =
    let args = T.Exp.Var caller_id :: args in
    let env, id, _typ = Env.mk_builtin_call env Builtin.PythonCall args in
    let env = Env.push env (DataStack.Temp id) in
    (env, None)


  let run env ({FFI.Code.co_names; co_consts} as code) {FFI.Instruction.opname; arg} =
    Debug.p "[%s] argc = %d\n" opname arg ;
    let env, cells = pop_n_tos opname env arg [] in
    let env, args = to_textual env opname code cells in
    Debug.p "  #args = %d\n" (List.length args) ;
    let env, fname = pop_tos opname env in
    Debug.p "  fname = %a\n" DataStack.pp_cell fname ;
    match fname with
    | DataStack.Name ndx ->
        let fname = co_names.(ndx) in
        static_call env fname args
    | Temp id ->
        dynamic_call env id args
    | Code {qualified_name} ->
        L.die UserError "[%s] no support for calling raw class/code : %s" opname qualified_name
    | VarName _ | Const _ | Map _ ->
        L.die UserError "[%s] invalid function on the stack: %s" opname (DataStack.show_cell fname)
    | BuiltinBuildClass -> (
      match cells with
      | [code_cell; name_cell] -> (
          let qualified_name =
            match (name_cell : DataStack.cell) with
            | Const ndx ->
                let const = co_consts.(ndx) in
                FFI.Constant.as_name const
            | _ ->
                L.die ExternalError "[%s] expected literal class name but got %a" opname
                  DataStack.pp_cell name_cell
          in
          let code =
            match (code_cell : DataStack.cell) with
            | Const ndx ->
                let const = co_consts.(ndx) in
                FFI.Constant.as_code const
            | Code {code} ->
                Some code
            | _ ->
                L.die ExternalError "[%s] expected code object but got %a" opname DataStack.pp_cell
                  code_cell
          in
          match (code, qualified_name) with
          | _, None | None, _ ->
              L.die UserError "[%s] Invalid class declaration" opname
          | Some code, Some qualified_name ->
              let env = Env.register_class env qualified_name in
              let env = Env.push env (DataStack.Code {fun_or_class= false; qualified_name; code}) in
              (env, None) )
      | _ ->
          L.die InternalError "[%s] unsupported call to LOAD_BUILD_CLASS with arg = %d" opname arg )
end

module BINARY_ADD = struct
  (** {v BINARY_ADD v}

      Implements top-of-stack = top-of-stack1 + top-of-stack.

      Before: [ TOS (rhs) | TOS1 (lhs) | rest-of-stack ]

      After: [ TOS1 + TOS (lhs + rhs) | rest-of-stack ]

      Since Python is using runtime types to know which [+] to do (addition, string concatenation,
      custom operator, ...), we'll need to write a model for this one. *)
  let run env code {FFI.Instruction.opname} =
    Debug.p "[%s]\n" opname ;
    let env, tos = pop_tos opname env in
    let env, tos1 = pop_tos opname env in
    let env, lhs, _ = load_cell env code tos1 in
    let lhs =
      match lhs with `Ok lhs -> lhs | `Error s -> L.die InternalError "[%s] %s" opname s
    in
    let env, rhs, _ = load_cell env code tos in
    let rhs =
      match rhs with `Ok rhs -> rhs | `Error s -> L.die InternalError "[%s] %s" opname s
    in
    (* Even if the call can be considered as virtual because, it's logic is not symetric. Based
       on what I gathered, like in [0], I think the best course of action is to write a model for
       it and leave it non virtual. TODO: ask David.

       [0]:
       https://stackoverflow.com/questions/58828522/is-radd-called-if-add-raises-notimplementederror
    *)
    let env, id, _typ = Env.mk_builtin_call env Builtin.BinaryAdd [lhs; rhs] in
    let env = Env.push env (DataStack.Temp id) in
    (env, None)
end

module MAKE_FUNCTION = struct
  let check_flags opname flag =
    let support_tuple_of_defaults = flag land 1 <> 0 in
    let support_dict_of_defaults = flag land 2 <> 0 in
    let support_closure = flag land 8 <> 0 in
    let unsupported = support_tuple_of_defaults || support_dict_of_defaults || support_closure in
    if unsupported then
      L.die InternalError "%s: support for flag 0x%x is not implemented" opname flag


  let unpack_annotations {FFI.Code.co_names; co_consts} annotations =
    let unpack =
      List.fold_left ~init:(Some [])
        ~f:(fun acc (key, c) ->
          match (acc, c) with
          | None, _ ->
              None
          | Some acc, DataStack.Name ndx ->
              let annot = co_names.(ndx) in
              Some ((key, annot) :: acc)
          | Some acc, DataStack.Const ndx -> (
            match co_consts.(ndx) with
            | FFI.Constant.PYCNone ->
                Some ((key, "None") :: acc)
            | _ ->
                Some acc )
          | Some acc, c ->
              Debug.p "[unpack_annotations] unsupported cell %a\n" DataStack.pp_cell c ;
              Some acc )
        annotations
    in
    Option.value unpack ~default:[]


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
  let run env ({FFI.Code.co_consts} as code) {FFI.Instruction.opname; arg} =
    Debug.p "[%s] flags = 0x%x\n" opname arg ;
    check_flags opname arg ;
    let env, qual = pop_tos opname env in
    (* don't care about the content of the code object, but check it is indeed code *)
    let env, body = pop_tos opname env in
    let env, annotations =
      if arg land 4 <> 0 then (
        let env, cell = pop_tos opname env in
        Debug.p "[%s] spotted annotations\n  %s\n" opname (DataStack.show_cell cell) ;
        let annotations = match cell with Env.DataStack.Map map -> Some map | _ -> None in
        (env, annotations) )
      else (env, None)
    in
    let annotations =
      Option.map ~f:(unpack_annotations code) annotations |> Option.value ~default:[]
    in
    let code =
      match DataStack.as_code code body with
      | None ->
          L.die InternalError "%s: payload is not code: %s" opname (DataStack.show_cell body)
      | Some body ->
          body
    in
    if FFI.Code.is_closure code then L.die InternalError "%s: can't create closure" opname ;
    let qualified_name =
      match (qual : DataStack.cell) with
      | VarName _ | Name _ | Temp _ | Code _ | Map _ | BuiltinBuildClass ->
          L.die InternalError "%s: invalid function name: %s" opname (DataStack.show_cell qual)
      | Const ndx -> (
          let const = co_consts.(ndx) in
          match FFI.Constant.as_name const with
          | Some name ->
              name
          | None ->
              L.die InternalError "%s: can't read qualified name from stack: %s" opname
                (FFI.Constant.show const) )
    in
    let env = Env.register_toplevel env qualified_name annotations in
    let env = Env.push env (DataStack.Code {fun_or_class= true; qualified_name; code}) in
    (env, None)
end

module BUILD = struct
  module CONST_KEY_MAP = struct
    let pop_n_tos opname =
      let rec pop env n acc =
        if n > 0 then (
          let env, cell = pop_tos opname env in
          Debug.p "  popped %s\n" (DataStack.show_cell cell) ;
          pop env (n - 1) (cell :: acc) )
        else (env, acc)
      in
      Debug.p "[pop_n_tos]\n" ;
      pop


    let is_tuple_ids {FFI.Code.co_consts} cell =
      let as_key = function FFI.Constant.PYCString s -> Some s | _ -> None in
      match cell with
      | DataStack.Const ndx -> (
          let c = co_consts.(ndx) in
          match c with
          | FFI.Constant.PYCTuple keys ->
              Array.fold_result keys ~init:[] ~f:(fun keys c ->
                  match as_key c with Some key -> Ok (key :: keys) | None -> Error () )
          | _ ->
              Error () )
      | _ ->
          Error ()


    (** {v BUILD_CONST_KEY_MAP(count) v}

        The version of [BUILD_MAP] specialized for constant keys. Pops the top element on the stack
        which contains a tuple of keys, then starting from [top-of-stack+1], pops [count] values to
        form values in the built dictionary, which is pushed back on the stack. *)
    let run env code {FFI.Instruction.opname; arg} =
      Debug.p "[%s] count = %d\n" opname arg ;
      let env, cell = pop_tos opname env in
      let keys =
        match is_tuple_ids code cell with
        | Ok keys ->
            List.rev keys
        | Error () ->
            L.die UserError "[%s] expecting tuple of literal keys" opname
      in
      (* TODO check cells is a tuple of literal strings *)
      let env, values = pop_n_tos opname env arg [] in
      Debug.p "  #values = %d\n" (List.length values) ;
      let map =
        match List.zip keys values with
        | List.Or_unequal_lengths.Ok map ->
            map
        | Base.List.Or_unequal_lengths.Unequal_lengths ->
            L.die UserError "[%s] keys length (%d) doesn't match values length (%d)" opname
              (List.length keys) (List.length values)
      in
      let env = Env.push env (DataStack.Map map) in
      (env, None)
  end
end

let mk_jump loc labels ssa_args =
  let nodes =
    List.map
      ~f:(fun value ->
        let label = {T.NodeName.value; loc} in
        {T.Terminator.label; ssa_args} )
      labels
  in
  T.Terminator.Jump nodes


(** When reaching a jump instruction with a non empty datastack, turn it into loads/ssa variables
    and properly set the jump/label ssa_args and ssa_parameters *)
let stack_to_ssa env code =
  let env, zipped =
    Env.map ~env (Env.stack env) ~f:(fun env cell ->
        let env, exp, {Env.typ} = load_cell env code cell in
        match exp with
        | `Ok exp ->
            (env, (exp, typ))
        | `Error s ->
            L.die InternalError "[stack_to_ssa] %s" s )
  in
  (Env.reset_stack env, List.unzip zipped)


module JUMP = struct
  type kind =
    | Label of {ssa_args: T.Exp.t list; label_info: Env.Label.info}
    | Return of T.Terminator.t
    | TwoWay of
        { ssa_args: T.Exp.t list
        ; offset: bool * int
        ; next_info: Env.Label.info
        ; other_info: Env.Label.info }
    | Relative of {ssa_args: T.Exp.t list; label_info: Env.Label.info; delta: int}
    | Absolute of {ssa_args: T.Exp.t list; label_info: Env.Label.info; target: int}

  module POP_IF = struct
    (** {v POP_JUMP_IF_TRUE(target) v}

        If top-of-stack is true, sets the bytecode counter to target. top-of-stack is popped.

        {v POP_JUMP_IF_FALSE(target) v}

        If top-of-stack is false, sets the bytecode counter to target. top-of-steack is popped. *)
    let run ~next_is_true env code {FFI.Instruction.opname; arg} =
      Debug.p "[%s] target = %d\n" opname arg ;
      let env, tos = pop_tos opname env in
      let env, cell, _ = load_cell env code tos in
      let cond =
        match cell with `Ok cond -> cond | `Error s -> L.die InternalError "[%s] %s" opname s
      in
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      let env, next_label = Env.mk_fresh_label env in
      let env, other_label = Env.mk_fresh_label env in
      (* Compute the relevant pruning expressions *)
      let env, id, _ = Env.mk_builtin_call env Env.Builtin.IsTrue [cond] in
      let condT = T.Exp.Var id in
      let condF = T.Exp.not condT in
      let next_prune = if next_is_true then condT else condF in
      let other_prune = if next_is_true then condF else condT in
      let next_prelude loc env = Env.push_instr env (T.Instr.Prune {exp= next_prune; loc}) in
      let other_prelude loc env = Env.push_instr env (T.Instr.Prune {exp= other_prune; loc}) in
      let next_info = Env.Label.mk ~ssa_parameters ~prelude:next_prelude next_label in
      let other_info = Env.Label.mk ~ssa_parameters ~prelude:other_prelude other_label in
      (env, Some (TwoWay {ssa_args; offset= (true, arg); next_info; other_info}))
  end

  module FORWARD = struct
    (** {v JUMP_FORWARD(delta) v}

        Increments bytecode counter by [delta]. *)
    let run env code {FFI.Instruction.opname; arg} =
      Debug.p "[%s] delta = %d\n" opname arg ;
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      let env, label_name = Env.mk_fresh_label env in
      let label_info = Env.Label.mk ~ssa_parameters label_name in
      (env, Some (Relative {ssa_args; label_info; delta= arg}))
  end

  module ABSOLUTE = struct
    (** {v JUMP_ABSOLUTE(target) v}

        Set bytecode counter to [target]. Can target a previous offset. *)

    let run env code {FFI.Instruction.opname; arg; offset} =
      Debug.p "[%s] target = %d\n" opname arg ;
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      (* sanity check: we should already have allocated a label for this jump, if it is a backward
         edge. *)
      let env, label_info =
        if arg < offset then
          let opt_info = Env.label_of_offset env arg in
          match opt_info with
          | Some label_info ->
              (env, label_info)
          | None ->
              L.die UserError
                "[%s] invalid input, can't find the target of a back-edge from offset %d to offset \
                 %d"
                opname offset arg
        else
          let env, label_name = Env.mk_fresh_label env in
          let label_info = Env.Label.mk ~ssa_parameters label_name in
          (env, label_info)
      in
      (env, Some (Absolute {ssa_args; label_info; target= arg}))
  end
end

module RETURN_VALUE = struct
  (** {v RETURN_VALUE v}

      Returns the top-of-stack *)
  let run env code {FFI.Instruction.opname} =
    Debug.p "[%s]\n" opname ;
    let env, cell = pop_tos opname env in
    let env, exp, _ = load_cell env code cell in
    match exp with
    | `Ok exp ->
        let term = T.Terminator.Ret exp in
        (env, Some (JUMP.Return term))
    | `Error s ->
        L.die InternalError "[%s] %s" opname s
end

module ITER = struct
  module GET = struct
    (** {v GET_ITER v}

        Implements top-of-stack = iter(top-of-stack). Most of the type calls the [__iter__()] method
        on the top of the stack. Might be C code for builtin types, so we'll model it as a builtin *)
    let run env code {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let env, cell = pop_tos opname env in
      let env, exp, _ = load_cell env code cell in
      match exp with
      | `Ok exp ->
          let env, id, _ = Env.mk_builtin_call env Builtin.PythonIter [exp] in
          let env = Env.push env (DataStack.Temp id) in
          (env, None)
      | `Error s ->
          L.die InternalError "[%s] %s" opname s
  end

  module FOR = struct
    (** {v FOR_ITER(delta) v}

        top-of-stack is an iterator. Call its [__next__()] method. If this yields a new value, push
        it on the stack (leaving the iterator below it). If the iterator indicates it is exhausted
        top-of-stack is popped, and the byte code counter is incremented by delta. *)
    let run env code {FFI.Instruction.opname; arg} =
      Debug.p "[%s] delta = %d\n" opname arg ;
      let env, iter_cell = pop_tos opname env in
      let env, iter_exp, _ = load_cell env code iter_cell in
      match iter_exp with
      | `Ok iter ->
          (* TODO: Not sure I know how to write a model for python_iter_next/python_iter_item
             as two separate functions. Maybe introduce a single one that returns a pair. *)
          let env, id, _ = Env.mk_builtin_call env Builtin.PythonIterNext [iter] in
          let cond = T.Exp.Var id in
          let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
          let env, next_label = Env.mk_fresh_label env in
          let env, other_label = Env.mk_fresh_label env in
          (* Compute the relevant pruning expressions *)
          let condT = cond in
          let condF = T.Exp.not condT in
          (* In the next branch, we know the iterator has an item available. Let's fetch it and
             push it on the stack. *)
          let next_prelude loc env =
            (* The iterator object stays on the stack while in the for loop, let's push it back *)
            let env = Env.push env iter_cell in
            let env = Env.push_instr env (T.Instr.Prune {exp= condT; loc}) in
            let env, item_id, _ = Env.mk_builtin_call env Builtin.PythonIterItem [iter] in
            Env.push env (DataStack.Temp item_id)
          in
          let other_prelude loc env = Env.push_instr env (T.Instr.Prune {exp= condF; loc}) in
          let next_info = Env.Label.mk ~ssa_parameters ~prelude:next_prelude next_label in
          let other_info = Env.Label.mk ~ssa_parameters ~prelude:other_prelude other_label in
          (env, Some (JUMP.TwoWay {ssa_args; offset= (false, arg); next_info; other_info}))
      | `Error s ->
          L.die InternalError "[%s] %s" opname s
  end
end

(** Main opcode dispatch function. *)
let run_instruction env code ({FFI.Instruction.opname; starts_line} as instr) =
  let env = Env.update_last_line env starts_line in
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
        POP_TOP.run env instr
    | "CALL_FUNCTION" ->
        CALL_FUNCTION.run env code instr
    | "BINARY_ADD" ->
        BINARY_ADD.run env code instr
    | "MAKE_FUNCTION" ->
        MAKE_FUNCTION.run env code instr
    | "POP_JUMP_IF_TRUE" ->
        JUMP.POP_IF.run ~next_is_true:false env code instr
    | "POP_JUMP_IF_FALSE" ->
        JUMP.POP_IF.run ~next_is_true:true env code instr
    | "JUMP_FORWARD" ->
        JUMP.FORWARD.run env code instr
    | "JUMP_ABSOLUTE" ->
        JUMP.ABSOLUTE.run env code instr
    | "GET_ITER" ->
        ITER.GET.run env code instr
    | "FOR_ITER" ->
        ITER.FOR.run env code instr
    | "BUILD_CONST_KEY_MAP" ->
        BUILD.CONST_KEY_MAP.run env code instr
    | "LOAD_BUILD_CLASS" ->
        LOAD.BUILD_CLASS.run env instr
    | _ ->
        L.die InternalError "Unsupported opcode: %s" opname
  in
  (env, maybe_term)


(** Helper function to check if the next instructions has a label attached to it *)
let has_jump_target env instructions =
  match List.hd instructions with
  | None ->
      (env, None)
  | Some {FFI.Instruction.offset; is_jump_target} -> (
    match Env.label_of_offset env offset with
    | Some label_info ->
        if Env.Label.is_processed label_info then (env, None)
        else (env, Some (offset, false, label_info))
    | None ->
        if is_jump_target then
          (* Probably the target of a back edge. Let's register and empty label info here. *)
          let env, label_name = Env.mk_fresh_label env in
          let label_info = Env.Label.mk label_name in
          (env, Some (offset, true, label_info))
        else (env, None) )


(** Iterator on [run_instruction]: this function will interpret instructions as long as terminator
    is not reached. *)
let rec run env code instructions =
  match instructions with
  | [] ->
      (env, None, [])
  | instr :: rest -> (
      (* If the current instruction a jump target (either because we already registered a label
         there, or because of the info from Python instructions), we stop and close the node *)
      let env, maybe_label = has_jump_target env instructions in
      match maybe_label with
      | None ->
          (* Nop, just continue processing the stream of instrunctions *)
          let env, maybe_term = run_instruction env code instr in
          if Option.is_some maybe_term then (env, maybe_term, rest) else run env code rest
      | Some (_offset, maybe_backedge, label_info) ->
          (* Yes, let's stop there, and don't forget to keep [instr] around *)
          let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
          let label_info =
            if maybe_backedge then Env.Label.update_ssa_parameters label_info ssa_parameters
            else label_info
          in
          (env, Some (JUMP.Label {ssa_args; label_info}), instructions) )


(** Return the location of the first available instruction, if any *)
let first_loc_of_code instructions =
  match instructions with
  | {FFI.Instruction.starts_line= Some line} :: _ ->
      T.Location.known ~line ~col:0
  | _ ->
      T.Location.Unknown


(** Return the offset of the next opcode, if any *)
let offset_of_code instructions =
  match instructions with FFI.Instruction.{offset} :: _ -> Some offset | _ -> None


(** Process the instructions of a code object up to the point where a terminator is reached. It will
    return the remaining instructions, new allocated node, along with any label that should be used
    to start the next node, if any (and prunning information).

    If the terminator is [Label], insert a jump to the next instruction to split the current stream
    of instructions into two valid nodes.

    If the terminator is [Return], just return the single node describing all the instruction we saw
    until now, and the remaining instructions.

    If the terminator is [TwoWay], we have to record the current node, and we register two fresh
    labels for the two possible jump locations. One is always the follow-up instruction, the "next"
    instruction, and the "other" might be located further away in case of nested "if/then/else"
    scenarios.

    If the terminator is [Relative], an unconditional jump forward is performed, based on the offset
    of the next instruction. *)
let until_terminator env label_info code instructions =
  let label_loc = first_loc_of_code instructions in
  let env, label_name, ssa_parameters = Env.Label.to_textual env label_loc label_info in
  let label = {T.NodeName.value= label_name; loc= label_loc} in
  (* process instructions until the next terminator *)
  let env, maybe_term, rest = run env code instructions in
  let last_loc = Env.loc env in
  let next_offset () =
    match offset_of_code rest with
    | None ->
        L.die InternalError "Relative jump forward at the end of code"
    | Some offset ->
        offset
  in
  let unconditional_jump env ssa_args label_info target =
    let target_label = Env.Label.name label_info in
    let jump = mk_jump last_loc [target_label] ssa_args in
    let node =
      T.Node.
        { label
        ; ssa_parameters
        ; exn_succs= []
        ; last= jump
        ; instrs= Env.instructions env
        ; last_loc
        ; label_loc }
    in
    (* Now register the target label *)
    let env = Env.register_label ~offset:target label_info env in
    (env, node)
  in
  match maybe_term with
  | None ->
      L.die InternalError "Reached the end of code without spotting a terminator"
  | Some (Label {ssa_args; label_info}) ->
      let next_label = Env.Label.name label_info in
      (* A label was spotted without having a clear Textual terminator. Insert a jump to this
         label to create a proper node, and resume the processing. *)
      let jump = mk_jump last_loc [next_label] ssa_args in
      let node =
        T.Node.
          { label
          ; ssa_parameters
          ; exn_succs= []
          ; last= jump
          ; instrs= Env.instructions env
          ; last_loc
          ; label_loc }
      in
      Debug.p "  Label: splitting instructions alongside %s\n" next_label ;
      let offset = next_offset () in
      let env = Env.register_label ~offset label_info env in
      (env, rest, node)
  | Some (Return last) ->
      let node =
        T.Node.
          { label
          ; ssa_parameters
          ; exn_succs= []
          ; last
          ; instrs= Env.instructions env
          ; last_loc
          ; label_loc }
      in
      Debug.p "  Return\n" ;
      (env, rest, node)
  | Some (TwoWay {ssa_args; offset= is_absolute, other_offset; next_info; other_info}) ->
      (* The current node ended up with a two-way jump. Either continue to the "next"
         (fall-through) part of the code, or jump to the "other" section of the code. For this
         purpose, register a fresh label for the jump. *)
      let next_label = Env.Label.name next_info in
      let other_label = Env.Label.name other_info in
      (* Register the jump target *)
      let next_offset = next_offset () in
      let other_offset = if is_absolute then other_offset else next_offset + other_offset in
      Debug.p "  TwoWay: register %s at %d\n" other_label other_offset ;
      let env = Env.register_label ~offset:next_offset next_info env in
      let env = Env.register_label ~offset:other_offset other_info env in
      let jump = mk_jump last_loc [next_label; other_label] ssa_args in
      let node =
        T.Node.
          { label
          ; ssa_parameters
          ; exn_succs= []
          ; last= jump
          ; instrs= Env.instructions env
          ; last_loc
          ; label_loc }
      in
      (env, rest, node)
  | Some (Relative {ssa_args; label_info; delta}) ->
      (* The current node ends up with a relative jump to [+delta]. The first thing to get is the
         offset of the next instruction, which is the base of the jump. Since Python
         bytecode is not of fixed size, the easiest way is to check the next instruction. *)
      let offset =
        match offset_of_code rest with
        | None ->
            L.die InternalError "Relative jump forward at the end of code"
        | Some offset ->
            offset
      in
      let env, node = unconditional_jump env ssa_args label_info (offset + delta) in
      Debug.p "  Relative: register %s at %d\n" (Env.Label.name label_info) (offset + delta) ;
      (env, rest, node)
  | Some (Absolute {ssa_args; label_info; target}) ->
      (* The current node ends up with an absolute jump to [target]. *)
      let env, node = unconditional_jump env ssa_args label_info target in
      Debug.p "  Absolute: register %s at %d\n" (Env.Label.name label_info) target ;
      (env, rest, node)


(** Process a sequence of instructions until there is no more left to process. *)
let rec nodes env label_info code instructions =
  let env = Env.enter_node env in
  let env, instructions, textual_node = until_terminator env label_info code instructions in
  if List.is_empty instructions then (env, [textual_node])
  else
    let env, label_info =
      (* If the next instruction has a label, use it, otherwise pick a fresh one. *)
      let env, jump_target = has_jump_target env instructions in
      match jump_target with
      | Some (offset, _, label_info) ->
          (* Mark the label as processed *)
          let env = Env.process_label ~offset label_info env in
          (env, label_info)
      | None ->
          let env, label_name = Env.mk_fresh_label env in
          (env, Env.Label.mk label_name)
    in
    let env, more_textual_nodes = nodes env label_info code instructions in
    (env, textual_node :: more_textual_nodes)


let type_of_annotation typ =
  let typ =
    match typ with
    | "int" ->
        PyCommon.pyInt
    | "str" ->
        PyCommon.pyString
    | "bool" ->
        PyCommon.pyBool
    | "float" ->
        PyCommon.pyFloat
    | "None" ->
        PyCommon.pyNone
    | "object" ->
        PyCommon.pyObject
    | _ ->
        Debug.p "[type_of_annotation] unsupported type: %s\n" typ ;
        PyCommon.pyObject
  in
  T.Typ.{typ; attributes= []}


(** Process a single code unit (toplevel code, function body, ...) *)
let to_proc_desc env loc enclosing_class name
    ({FFI.Code.co_argcount; co_varnames; instructions} as code) =
  Debug.p "\n\n[to_proc_desc] %s\n" name.T.ProcName.value ;
  let qualified_name = qualified_procname ~enclosing_class name in
  let pyObject = T.Typ.{typ= PyCommon.pyObject; attributes= []} in
  let nr_varnames = Array.length co_varnames in
  let params = Array.sub co_varnames ~pos:0 ~len:co_argcount in
  let locals = Array.sub co_varnames ~pos:co_argcount ~len:(nr_varnames - co_argcount) in
  let params = Array.map ~f:(var_name ~loc) params |> Array.to_list in
  let locals = Array.map ~f:(fun name -> (var_name ~loc name, pyObject)) locals |> Array.to_list in
  (* Create the original environment for this code unit *)
  let env = Env.enter_proc env in
  let env, entry_label = Env.mk_fresh_label env in
  let label = node_name ~loc entry_label in
  let label_info = Env.Label.mk entry_label in
  (* Now that a full unit has been processed, discard all the local information (local variable
     names, labels, ...) and only keep the [shared] part of the environment *)
  let env, nodes = nodes env label_info code instructions in
  let annotations = Env.lookup_signature env name |> Option.value ~default:[] in
  let types = List.map ~f:(fun (n, t) -> (n, type_of_annotation t)) annotations in
  let formals_types =
    List.map
      ~f:(fun {T.VarName.value} ->
        let typ = List.Assoc.find types ~equal:String.equal value in
        Option.value typ ~default:pyObject )
      params
  in
  let result_type =
    List.Assoc.find types ~equal:String.equal "return" |> Option.value ~default:pyObject
  in
  let procdecl =
    {T.ProcDecl.qualified_name; formals_types= Some formals_types; result_type; attributes= []}
  in
  (env, {Textual.ProcDesc.procdecl; nodes; start= label; params; locals; exit_loc= Unknown})


(* TODO: No support for nested functions/methods at the moment *)

(** Process multiple [code] objects. Usually called by the toplevel function or by a class
    definition. *)
let to_proc_descs env enclosing_class codes =
  Array.fold codes ~init:(env, []) ~f:(fun (env, decls) const ->
      match FFI.Constant.as_code const with
      | None ->
          (env, decls)
      | Some ({FFI.Code.co_name; instructions} as code) ->
          let loc = first_loc_of_code instructions in
          let classes = Env.get_classes env in
          if List.mem ~equal:String.equal classes co_name then
            let t =
              {T.Struct.name= type_name ~loc co_name; supers= []; fields= []; attributes= []}
            in
            (env, [T.Module.Struct t])
          else
            let name = PyCommon.global co_name in
            let name = proc_name ~loc name in
            let env, decl = to_proc_desc env loc enclosing_class name code in
            (env, T.Module.Proc decl :: decls) )


let python_attribute = Textual.Attr.mk_source_language Textual.Lang.Python

(** Entry point of the module: process a whole Python file / compilation unit into Textual *)
let to_module ~sourcefile ({FFI.Code.co_consts; co_name; instructions} as code) =
  Debug.p "[to_module] %a %s\n" T.SourceFile.pp sourcefile co_name ;
  if not (String.equal co_name "<module>") then
    L.die ExternalError "Toplevel modules must be named '<module>'. Got %s" co_name ;
  let env = Env.empty in
  (* Process top level module first, to gather all global definitions.
     This will also help us identify what is a class and what is a function,
     before processing the rest of the [co_consts]. There is no flag to
     distinguish class definitions from function definitions in the bytecode
     format.


     TODO: scoping might require fixing.
     Python allows multiple toplevel declaration of the same name and resolution is done
     dynamically. E.g.

     ```
     def f():
         return 10

     def g():
         return f()

     print(g())

     def f():
         return "cat"

     print (g())
     ```

     this would print `10` and then `"cat"`.  We should investigate if suche code exists, and in
     which quantity, to see if it is worth finding a solution for it.
  *)
  let loc = first_loc_of_code instructions in
  let module_name = PyCommon.global "toplevel" in
  let name = proc_name ~loc module_name in
  let env, decl = to_proc_desc env loc T.TopLevel name code in
  (* Translate globals to Textual *)
  let globals =
    T.VarName.Map.fold
      (fun name {Env.is_code} acc ->
        if is_code then
          (* don't generate a global variable name, it will be declared as a toplevel decl *)
          acc
        else
          let global = T.Global.{name; typ= PyCommon.pyObject; attributes= []} in
          T.Module.Global global :: acc )
      (Env.globals env) []
  in
  (* Then, process any code body that is in code.co_consts *)
  let env, decls = to_proc_descs env T.TopLevel co_consts in
  let decls = List.rev decls in
  (* Gather everything into a Textual module *)
  let decls =
    ((T.Module.Proc decl :: decls) @ globals) @ Env.BuiltinSet.to_textual (Env.builtins env)
  in
  {T.Module.attrs= [python_attribute]; decls; sourcefile}
