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
module DataStack = Env.DataStack
module Builtin = PyBuiltin
module Symbol = Env.Symbol
module MakeFunctionFlags = PyCommon.MakeFunctionFlags

let prefix_name ~module_name name = module_name ^ "::" ^ name

let var_name = PyCommon.var_name

let node_name = PyCommon.node_name

let proc_name = PyCommon.proc_name

let field_name = PyCommon.field_name

let type_name = PyCommon.type_name

let qualified_procname = PyCommon.qualified_procname

(** Turn a Python [FFI.Code.t] into a Textual [T.Exp.t], along with the required instructions to
    effectively perform the translation. *)
let code_to_exp ~fun_or_class env name =
  let fname =
    (* TODO: support closures *)
    let opt_sym = Env.lookup_symbol ~global:true env name in
    match (opt_sym : Symbol.t option) with
    | Some (Name _ as symbol) | Some (Class _ as symbol) | Some (Import _ as symbol) ->
        L.die InternalError "[code_to_exp] Name/Class/Import %a is not a code object" Symbol.pp
          symbol
    | Some (Code _ as symbol) ->
        let code_sep = if fun_or_class then "." else "::" in
        Symbol.to_string ~code_sep symbol
    | Some Builtin ->
        L.die InternalError "[code_to_exp] unexpected Builtin: %s\n" name
    | None ->
        (* Corner cases for things we don't support nicely at the moment *)
        name
  in
  let kind = if fun_or_class then Builtin.PythonCode else Builtin.PythonClass in
  let name = T.Exp.Const (Str fname) in
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
  | PYCFloat f ->
      (env, PyCommon.mk_float f, PyCommon.pyFloat)
  | PYCString s ->
      (env, PyCommon.mk_string s, PyCommon.pyString)
  | PYCBytes s ->
      (env, PyCommon.mk_bytes s, PyCommon.pyBytes)
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
      let exp = T.Exp.call_non_virtual PyCommon.python_tuple args in
      (env, exp, PyCommon.pyObject)


(** Special case of [load_cell] that we sometime use with a name directly, rather then an index in
    [FFI.Code.t] *)
let load_var_name env loc name =
  let info typ = {Env.typ; is_code= false; is_class= false} in
  let exp = T.Exp.Lvar (var_name ~loc name) in
  let loc = Env.loc env in
  let opt_sym = Env.lookup_symbol ~global:false env name in
  let typ =
    match (opt_sym : Symbol.t option) with
    | Some (Name {typ}) ->
        typ
    | Some (Class _ | Code _ | Import _ | Builtin) ->
        (* Note: maybe PyCommon.pyObject would be better here, but I fail to catch new behavior early *)
        L.die InternalError "[load_cell] Can't load VarName %a\n" (Pp.option Symbol.pp) opt_sym
    | None ->
        PyCommon.pyObject
  in
  let info = info typ in
  let env, id = Env.mk_fresh_ident env info in
  let instr = T.Instr.Load {id; exp; typ; loc} in
  let env = Env.push_instr env instr in
  (env, Ok (T.Exp.Var id), info)


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
      (env, Ok exp, info)
  | Name {global; ndx} ->
      let name = co_names.(ndx) in
      let ({Env.typ; is_code} as info), qualified_name =
        let opt_sym = Env.lookup_symbol ~global env name in
        match (opt_sym : Symbol.t option) with
        | Some (Name {typ} as symbol_info) ->
            let qname = Symbol.to_string symbol_info in
            ({Env.is_code= false; is_class= false; typ}, qname)
        | Some (Class _ as class_info) ->
            L.die InternalError "[load_cell] with class: should never happen ?  %s"
              (Symbol.to_string class_info)
            (* ({Env.is_code= false; is_class= true; typ= PyCommon.pyObject}, qname) *)
        | Some (Code _ as code_info) ->
            let qname = Symbol.to_string code_info in
            ({Env.is_code= true; is_class= false; typ= PyCommon.pyCode}, qname)
        | Some (Import _ as symbol) ->
            let qname = Symbol.to_string symbol in
            L.die InternalError "[load_cell] called with %s (import)" qname
        | Some Builtin | None ->
            (default, PyCommon.unknown_global name)
      in
      let var_name = var_name ~loc qualified_name in
      (* If we are trying to load some code, use the dedicated builtin *)
      if is_code then
        let env, exp, typ = code_to_exp ~fun_or_class:true env qualified_name in
        let info = {info with Env.typ} in
        (env, Ok exp, info)
      else
        let exp = T.Exp.Lvar var_name in
        let loc = Env.loc env in
        let env, id = Env.mk_fresh_ident env info in
        let instr = T.Instr.Load {id; exp; typ; loc} in
        let env = Env.push_instr env instr in
        (* TODO: try to trace the type of names, not only global ones ? *)
        (env, Ok (T.Exp.Var id), info)
  | VarName ndx ->
      let name = co_varnames.(ndx) in
      load_var_name env loc name
  | Temp id ->
      let info = Env.get_ident_info env id |> Option.value ~default in
      (env, Ok (T.Exp.Var id), info)
  | Code {fun_or_class; code} ->
      let env, exp, typ = code_to_exp env ~fun_or_class code.FFI.Code.co_name in
      let is_code, is_class = if fun_or_class then (true, false) else (false, true) in
      let info = {Env.typ; is_code; is_class} in
      (env, Ok exp, info)
  | Map _map ->
      (env, Error "TODO: load Map cells", default)
  | BuiltinBuildClass | StaticCall _ | MethodCall _ | Import _ | ImportCall _ | Super ->
      L.die InternalError "[load_cell] Can't load %a, something went wrong" DataStack.pp_cell cell


let cells_to_textual env opname code cells =
  Env.map ~env cells ~f:(fun env cell ->
      let env, exp, _ = load_cell env code cell in
      match exp with
      | Ok exp ->
          (env, exp)
      | Error s ->
          L.die UserError "[%s] failed to fetch from the stack: %s" opname s )


(** Pop the top of the datastack. Fails with an [InternalError] if the stack is empty. *)
let pop_datastack opname env =
  match Env.pop env with
  | None ->
      L.die ExternalError "[%s] can't pop, stack is empty" opname
  | Some (env, cell) ->
      (env, cell)


(** Peek the top of the datastack. Fails with an [InternalError] if the stack is empty. *)
let peek_datastack opname env =
  match Env.peek env with
  | None ->
      L.die ExternalError "[%s] can't peek, stack is empty" opname
  | Some cell ->
      cell


let pop_n_datastack opname =
  let rec pop env n acc =
    if n > 0 then (
      let env, cell = pop_datastack opname env in
      Debug.p "  popped %s\n" (DataStack.show_cell cell) ;
      pop env (n - 1) (cell :: acc) )
    else (env, acc)
  in
  Debug.p "[pop_n_datastack]\n" ;
  pop


(* Python opcodes support. Most of the documentation directly comes from the official python
   documentation and is only altered to improve readability.

   https://docs.python.org/3.8/library/dis.html *)

module LOAD = struct
  type kind =
    | ATTR
        (** {v LOAD_ATTR(namei) v}

            Replaces top-of-stack with [getattr(top-of-stack, co_names\[namei\])]. *)
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

  let run kind env ({FFI.Code.co_names} as code) {FFI.Instruction.opname; arg} =
    let pp {FFI.Code.co_names; co_varnames; co_consts} fmt = function
      | CONST ->
          FFI.Constant.pp fmt co_consts.(arg)
      | FAST ->
          F.fprintf fmt "%s" co_varnames.(arg)
      | ATTR | NAME | GLOBAL ->
          F.fprintf fmt "%s" co_names.(arg)
    in
    let env, cell =
      match kind with
      | CONST ->
          (env, DataStack.Const arg)
      | FAST ->
          (env, DataStack.VarName arg)
      | NAME ->
          (env, DataStack.Name {global= Env.is_toplevel env; ndx= arg})
      | GLOBAL ->
          let name = co_names.(arg) in
          if String.equal name "super" then
            (* [super] could have been defined in the current scope, so we have to first check
               this, to be sure the call to [super()] is to access the parent's methods, not some
               other symbol *)
            if Option.is_some (Env.lookup_symbol env ~global:true "super") then
              (env, DataStack.Name {global= true; ndx= arg})
            else (env, DataStack.Super)
          else (env, DataStack.Name {global= true; ndx= arg})
      | ATTR -> (
          let env, cell = pop_datastack opname env in
          let fname = co_names.(arg) in
          let env, res, _info = load_cell env code cell in
          match res with
          | Error s ->
              L.die ExternalError "Failed to load attribute %s from cell %a: %s" fname
                DataStack.pp_cell cell s
          | Ok exp ->
              (* TODO: refine typ if possible *)
              let typ = PyCommon.pyObject in
              let info = Env.{typ; is_class= false; is_code= false} in
              let env, id = Env.mk_fresh_ident env info in
              let loc = Env.loc env in
              let name = field_name ~loc fname in
              let field = {T.enclosing_class= T.TypeName.wildcard; name} in
              let exp = T.Exp.Field {exp; field} in
              let instr = T.Instr.Load {id; exp; typ; loc} in
              let env = Env.push_instr env instr in
              (env, DataStack.Temp id) )
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

let check_flags opname flags =
  let open MakeFunctionFlags in
  let has_tuple_of_defaults = mem flags DefaultValues in
  let has_dict_of_defaults = mem flags DictDefaultValues in
  let has_closure = mem flags Closure in
  let unsupported = has_tuple_of_defaults || has_dict_of_defaults || has_closure in
  if unsupported then
    L.die InternalError "%s: support for flag 0x%a is not implemented" opname pp flags


module FUNCTION = struct
  module CALL = struct
    let mk env fname loc proc args =
      let env = Option.value_map ~default:env ~f:(Env.register_call env) fname in
      let call = T.Exp.call_non_virtual proc args in
      (* TODO: read return type of proc if available *)
      let info = {Env.typ= PyCommon.pyObject; is_code= false; is_class= false} in
      let env, id = Env.mk_fresh_ident env info in
      let let_instr = T.Instr.Let {id; exp= call; loc} in
      let env = Env.push_instr env let_instr in
      let env = Env.push env (DataStack.Temp id) in
      (env, None)


    (** {v CALL_FUNCTION(argc) v}

        Calls a callable object with positional arguments. [argc] indicates the number of positional
        arguments. The top of the stack contains positional arguments, with the right-most argument
        on top. Below the arguments is a callable object to call. This opcode pushes a fresh result
        on the top of the stack.

        Before: [ argN | ... | arg1 | arg0 | code-object | rest-of-the-stack ]

        After: [ result | rest-of-the-stack ] *)

    let static_call env opname code fname cells =
      let env, args = cells_to_textual env opname code cells in
      let loc = Env.loc env in
      (* TODO: support nesting *)
      let opt_sym = Env.lookup_symbol ~global:true env fname in
      let mk env proc = mk env (Some fname) loc proc args in
      match (opt_sym : Symbol.t option) with
      | Some (Import _ as symbol) ->
          L.die InternalError "[static_call] Invalid Import (%s) spotted" (Symbol.to_string symbol)
      | Some Builtin ->
          let proc = PyCommon.builtin_name fname in
          mk env proc
      | Some (Name {is_imported} as symbol) ->
          let proc = Symbol.to_qualified_procname symbol in
          let env = if is_imported then Env.register_import env (Env.Import.Call proc) else env in
          mk env proc
      | Some (Code _ as symbol) ->
          let proc = Symbol.to_qualified_procname symbol in
          mk env proc
      | Some (Class _ as symbol) ->
          (* TODO: support nesting. Maybe add to_proc_name to Symbol *)
          let name = Symbol.to_string ~code_sep:"::" symbol in
          let name = proc_name ~loc name in
          let proc : T.qualified_procname = {enclosing_class= TopLevel; name} in
          mk env proc
      | None ->
          (* Unknown name, can come from a `from foo import *` so we'll try to locate it in a
             further analysis *)
          let proc =
            let enclosing_class = T.TypeName.wildcard in
            qualified_procname ~enclosing_class @@ proc_name ~loc fname
          in
          mk env proc


    let dynamic_call env opname code caller_id cells =
      let env, args = cells_to_textual env opname code cells in
      let args = T.Exp.Var caller_id :: args in
      let env, id, _typ = Env.mk_builtin_call env Builtin.PythonCall args in
      let env = Env.push env (DataStack.Temp id) in
      (env, None)


    let builtin_build_class env code opname name_cell code_cell parent_cell =
      let as_name kind cell =
        match DataStack.as_name code cell with
        | Some name ->
            name
        | None ->
            L.die ExternalError "[%s] expected literal (%s) class name but got %a" kind opname
              DataStack.pp_cell name_cell
      in
      let code_name = as_name "base" name_cell in
      let parent_name =
        Option.map
          ~f:(fun cell ->
            let short_parent = as_name "parent" cell in
            let loc = Env.loc env in
            let default =
              Symbol.Class
                { class_name=
                    Symbol.Qualified.mk ~prefix:[] (PyCommon.unknown_global short_parent) loc }
            in
            (* TODO: support nesting *)
            let sym = Env.lookup_symbol env ~global:true short_parent in
            Option.value ~default sym )
          parent_cell
      in
      let code =
        match DataStack.as_code code code_cell with
        | Some code ->
            code
        | None ->
            L.die ExternalError "[%s] expected code object but got %a" opname DataStack.pp_cell
              code_cell
      in
      let env = Env.register_class env code_name {Env.parent= parent_name} in
      let env = Env.push env (DataStack.Code {fun_or_class= false; code_name; code}) in
      env


    let run env ({FFI.Code.co_names} as code) {FFI.Instruction.opname; arg} =
      Debug.p "[%s] argc = %d\n" opname arg ;
      let env, cells = pop_n_datastack opname env arg [] in
      Debug.p "  #args = %d\n" (List.length cells) ;
      let env, fname = pop_datastack opname env in
      Debug.p "  fname = %a\n" DataStack.pp_cell fname ;
      match (fname : DataStack.cell) with
      | Name {ndx} ->
          let fname = co_names.(ndx) in
          static_call env opname code fname cells
      | Temp id ->
          dynamic_call env opname code id cells
      | Code {code_name} ->
          L.die UserError "[%s] no support for calling raw class/code : %s" opname code_name
      | VarName _ | Const _ | Map _ ->
          L.die UserError "[%s] invalid function on the stack: %s" opname
            (DataStack.show_cell fname)
      | BuiltinBuildClass -> (
        match cells with
        | [code_cell; name_cell] ->
            let env = builtin_build_class env code opname name_cell code_cell None in
            (env, None)
        | [code_cell; name_cell; parent_cell] ->
            let env = builtin_build_class env code opname name_cell code_cell (Some parent_cell) in
            (env, None)
        | _ ->
            L.die InternalError "[%s] unsupported call to LOAD_BUILD_CLASS with arg = %d" opname arg
        )
      | Import _ | ImportCall _ | StaticCall _ | MethodCall _ ->
          L.die InternalError "[%s] unsupported call to %a with arg = %d" opname DataStack.pp_cell
            fname arg
      | Super ->
          let env = Env.push env Super in
          (env, None)
  end

  module MAKE = struct
    let unpack_annotations {FFI.Code.co_names; co_consts} annotations =
      let unpack =
        List.fold_left ~init:(Some [])
          ~f:(fun acc (name, c) ->
            match (acc, c) with
            | None, _ ->
                None
            | Some acc, DataStack.Name {ndx} ->
                let annotation = co_names.(ndx) in
                Some ({PyCommon.name; annotation} :: acc)
            | Some acc, DataStack.Const ndx -> (
              match co_consts.(ndx) with
              | FFI.Constant.PYCNone ->
                  Some ({PyCommon.name; annotation= "None"} :: acc)
              | _ ->
                  Some acc )
            | Some acc, c ->
                Debug.p "[unpack_annotations] unsupported cell %a\n" DataStack.pp_cell c ;
                Some acc )
          annotations
      in
      Option.value unpack ~default:[]


    (** {v MAKE_FUNCTION(flags) v}

        Pushes a new function object on the stack. From bottom to top, the consumed stack must
        consist of values if the argument carries a specified flag value

        - [0x01] a tuple of default values for positional-only and positional-or-keyword parameters
          in positional order
        - [0x02] a dictionary of keyword-only parameters’ default values
        - [0x04] an annotation dictionary
        - [0x08] a tuple containing cells for free variables, making a closure
        - the code associated with the function (at TOS1)
        - the qualified name of the function (at TOS)

        In this first version, only support for [flags = 0x00] is implemented. Also there is no
        support for closures or nested functions *)
    let run env ({FFI.Code.co_consts} as code) {FFI.Instruction.opname; arg} =
      let flags = MakeFunctionFlags.mk arg in
      Debug.p "[%s] flags = 0x%a\n" opname MakeFunctionFlags.pp flags ;
      check_flags opname flags ;
      let env, qual = pop_datastack opname env in
      (* don't care about the content of the code object, but check it is indeed code *)
      let env, body = pop_datastack opname env in
      let env, annotations =
        if MakeFunctionFlags.mem flags Annotations then (
          let env, cell = pop_datastack opname env in
          Debug.p "[%s] spotted annotations\n  %s\n" opname (DataStack.show_cell cell) ;
          let annotations = match cell with Env.DataStack.Map map -> Some map | _ -> None in
          (env, annotations) )
        else (env, None)
      in
      let annotations = Option.value_map ~default:[] ~f:(unpack_annotations code) annotations in
      let code =
        match DataStack.as_code code body with
        | None ->
            L.die InternalError "%s: payload is not code: %s" opname (DataStack.show_cell body)
        | Some body ->
            body
      in
      let code_name =
        match (qual : DataStack.cell) with
        | VarName _
        | Name _
        | Temp _
        | Code _
        | Map _
        | BuiltinBuildClass
        | Import _
        | ImportCall _
        | MethodCall _
        | StaticCall _
        | Super ->
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
      if FFI.Code.is_closure code then
        L.user_warning "%s: support for closures is incomplete (%s)\n" opname code_name ;
      let loc = Env.loc env in
      let env = Env.register_function env code_name loc annotations in
      let env = Env.push env (DataStack.Code {fun_or_class= true; code_name; code}) in
      (env, None)
  end
end

module METHOD = struct
  module LOAD = struct
    let rec to_typename typ =
      if T.Typ.equal typ PyCommon.pyObject then T.TypeName.wildcard
      else
        match (typ : T.Typ.t) with
        | Struct tname ->
            tname
        | Ptr t ->
            to_typename t
        | Int | Float | Null | Void | Array _ ->
            T.TypeName.wildcard


    let load env code cell method_name =
      let env, res, {Env.typ} = load_cell env code cell in
      let tname = to_typename typ in
      match res with
      | Error s ->
          L.die ExternalError "Failed to load method %s from cell %a: %s" method_name
            DataStack.pp_cell cell s
      | Ok receiver ->
          let loc = Env.loc env in
          let name = proc_name ~loc method_name in
          let name : T.qualified_procname = {T.enclosing_class= Enclosing tname; name} in
          let env = Env.push env (DataStack.MethodCall {receiver; name}) in
          (env, None)


    (* Here we translate calls like [super().f(...)] into a static call [parent_type.f(self, ...)]
       so that method resolution can do its job. This only works because we
       only support single inheritance for now. We need to revisit this.

       Python does a virtual call here because of how the runtime deals with inheritance: it's
       just a pointer to the parent classes. We don't encode this like that for now.

       TODO: pass in the current function name for better error reporting, but not urgent as all
       these errors would make python runtime crash anyway *)
    let load_super env method_name =
      (* TODO: rethink the class API ? *)
      let current_module = Env.module_name env in
      (* current_module is a fully qualified name like foo#C, but we want to extract that C part
         only to lookup in our set of known classes. *)
      let class_name =
        match Str.split (Str.regexp "::") current_module |> List.last with
        | Some name ->
            name
        | None ->
            (* TODO: support nesting *)
            L.die InternalError "Can't extract class name from fully qualified name %s\n"
              current_module
      in
      let classes = Env.get_declared_classes env in
      let class_info = Env.SMap.find_opt class_name classes in
      let params = Env.get_params env in
      let is_static = Env.is_static env in
      let parent_info =
        match class_info with
        | None ->
            L.die ExternalError "Call to super() spotted in %s which not a registered class"
              current_module
        | Some {Env.parent} ->
            parent
      in
      let parent_name =
        match parent_info with
        | None ->
            L.die ExternalError
              "Call to super() spotted in %s which doesn't have a registered parent class"
              current_module
        | Some parent ->
            parent
      in
      let self =
        if is_static then
          L.die ExternalError "Call to super() spotted in a static method of %s" current_module ;
        match List.hd params with
        | None ->
            L.die ExternalError "Empty parameter list makes illegal call to super() in %s"
              current_module
        | Some self ->
            self
      in
      let super_type = Symbol.to_type_name ~is_static:false parent_name in
      let loc = Env.loc env in
      let env, res, _ = load_var_name env loc self in
      match res with
      | Ok receiver ->
          (* We're in [LOAD_METHOD] but we fake a static call because of how method resolution works. *)
          let loc = Env.loc env in
          let name = proc_name ~loc method_name in
          let call_name : T.qualified_procname = {T.enclosing_class= Enclosing super_type; name} in
          let env = Env.push env (DataStack.StaticCall {call_name; receiver= Some receiver}) in
          (env, None)
      | Error s ->
          L.die UserError "[super()] failed to load self (a.k.a %s) in %s: %s" self current_module s


    (** {v LOAD_METHOD(namei) v}

        Loads a method named [co_names\[namei\]] from the top-of-stack object. top-of-stack is
        popped. This bytecode distinguishes two cases: if top-of-stack has a method with the correct
        name, the bytecode pushes the unbound method and top-of-stack. top-of-stack will be used as
        the first argument ([self]) by [CALL_METHOD] when calling the unbound method. Otherwise,
        [NULL] and the object return by the attribute lookup are pushed. *)
    let run env ({FFI.Code.co_names} as code) {FFI.Instruction.opname; arg} =
      let method_name = co_names.(arg) in
      Debug.p "[%s] namei = %s\n" opname method_name ;
      let env, cell = pop_datastack opname env in
      match (cell : DataStack.cell) with
      | Name {global; ndx} -> (
          let name = co_names.(ndx) in
          let opt_sym = Env.lookup_symbol env ~global name in
          match (opt_sym : Symbol.t option) with
          | Some (Import {import_path}) ->
              let loc = Env.loc env in
              let enclosing_class = type_name ~loc import_path in
              let proc = qualified_procname ~enclosing_class @@ proc_name ~loc method_name in
              let env = Env.push env (DataStack.ImportCall proc) in
              (env, None)
          | Some (Class _ as qname) ->
              let loc = Env.loc env in
              let class_name = Symbol.to_string ~static:true qname in
              let enclosing_class = type_name ~loc class_name in
              let call_name = qualified_procname ~enclosing_class @@ proc_name ~loc method_name in
              let env = Env.push env (DataStack.StaticCall {call_name; receiver= None}) in
              (env, None)
          | _ ->
              load env code cell method_name )
      | Super ->
          load_super env method_name
      | _ ->
          load env code cell method_name
  end

  module CALL = struct
    (** {v CALL_METHOD(argc) v}

        Calls a method. [argc] is the number of positional arguments. Keyword arguments are not
        supported. This opcode is designed to be used with [LOAD_METHOD]. Positional arguments are
        on top of the stack. Below them, the two items described in [LOAD_METHOD] are on the stack
        (either [self] and an unbound method object or [NULL] and an arbitrary callable). All of
        them are popped and the return value is pushed. *)
    let run env code {FFI.Instruction.opname; arg} =
      Debug.p "[%s] argc = %d\n" opname arg ;
      let env, cells = pop_n_datastack opname env arg [] in
      let env, args = cells_to_textual env opname code cells in
      let env, cell_mi = pop_datastack opname env in
      match (cell_mi : DataStack.cell) with
      | ImportCall proc ->
          let loc = Env.loc env in
          let env = Env.register_import env (Env.Import.Call proc) in
          FUNCTION.CALL.mk env None loc proc args
      | StaticCall {call_name; receiver} ->
          let loc = Env.loc env in
          let args = Option.to_list receiver @ args in
          FUNCTION.CALL.mk env None loc call_name args
      | MethodCall {receiver; name} ->
          let loc = Env.loc env in
          let exp = T.Exp.call_virtual name receiver args in
          (* TODO: read return type of proc if available *)
          let info = {Env.typ= PyCommon.pyObject; is_code= false; is_class= false} in
          let env, id = Env.mk_fresh_ident env info in
          let let_instr = T.Instr.Let {id; exp; loc} in
          let env = Env.push_instr env let_instr in
          let env = Env.push env (DataStack.Temp id) in
          (env, None)
      | Const _
      | Name _
      | VarName _
      | Temp _
      | Code _
      | Map _
      | BuiltinBuildClass
      | Import _
      | Super ->
          L.die InternalError "[%s] failed to load method information from cell %a" opname
            DataStack.pp_cell cell_mi
  end
end

module STORE = struct
  type kind =
    | ATTR
        (** {v STORE_ATTR(namei) v}

            Implements [top-of-stack.name = top-of-stack-1], where [namei] is the index of name in
            [co_names]. *)
    | FAST
        (** {v STORE_FAST(var_num) v}

            Stores top-of-stack into the local [co_varnames\[var_num\]]. *)
    | NAME
        (** {v STORE_NAME(namei) v}

            Implements name = top-of-stack. namei is the index of name in the attribute co_names of
            the code object. The compiler tries to use [STORE_FAST] or [STORE_GLOBAL] if possible.

            Notes: this might happen in toplevel code, where it's equivalent to [STORE_GLOBAL].
            Otherwise, it stores information in the context of the current class declaration.

            In a function, local varialbes are updated using [STORE_FAST], and global variables are
            updated using [STORE_GLOBAL]. *)
    | GLOBAL
        (** {v STORE_GLOBAL(namei) v}

            Works as [STORE_NAME], but stores the name as a global.

            Since there is a special namespace for global variables, this is in fact the same as
            [STORE_NAME], but only called from within a function/method. *)

  let store env ({FFI.Code.co_names} as code) opname loc arg cell name global is_attr =
    let env, exp, info = load_cell env code cell in
    match exp with
    | Ok exp ->
        let {Env.typ; is_code; is_class} = info in
        let module_name = Env.module_name env in
        let qname = Symbol.Qualified.mk ~prefix:[module_name] name loc in
        let symbol_info =
          if is_code then Symbol.Code {code_name= qname}
          else if is_class then Symbol.Class {class_name= qname}
          else
            let prefix = if global then [module_name] else [] in
            let qname = Symbol.Qualified.mk ~prefix name loc in
            Symbol.Name {symbol_name= qname; is_imported= false; typ}
        in
        let gname = Symbol.to_string symbol_info in
        let var_name = var_name ~loc gname in
        if is_attr then
          let env, cell = pop_datastack opname env in
          let env, value, {Env.typ} = load_cell env code cell in
          match value with
          | Error s ->
              L.die InternalError "[%s] %s" opname s
          | Ok value ->
              let fname = co_names.(arg) in
              (* TODO: refine typ if possible *)
              let loc = Env.loc env in
              let name = field_name ~loc fname in
              let field = {T.enclosing_class= T.TypeName.wildcard; name} in
              let exp1 = T.Exp.Field {exp; field} in
              let instr = T.Instr.Store {exp1; typ; exp2= value; loc} in
              let env = Env.push_instr env instr in
              (env, None)
        else if is_class then (
          Debug.p "  top-level class declaration initialized\n" ;
          let _, env = Env.register_symbol ~global env name symbol_info in
          (env, None) )
        else if is_code then
          let _, env = Env.register_symbol ~global env name symbol_info in
          if global then (
            Debug.p "  top-level function defined\n" ;
            (env, None) )
          else L.die InternalError "[%s] no support for closure at the moment: %s" opname name
        else
          let _, env = Env.register_symbol ~global env name symbol_info in
          let instr = T.Instr.Store {exp1= Lvar var_name; typ; exp2= exp; loc} in
          (Env.push_instr env instr, None)
    | Error s ->
        L.die InternalError "[%s] %s" opname s


  let run kind env ({FFI.Code.co_names; co_varnames} as code) {FFI.Instruction.opname; arg} =
    let name, global, is_attr =
      match kind with
      | FAST ->
          (co_varnames.(arg), false, false)
      | NAME ->
          (co_names.(arg), Env.is_toplevel env, false)
      | ATTR ->
          (co_names.(arg), false, true)
      | GLOBAL ->
          (co_names.(arg), true, false)
    in
    Debug.p "[%s] namei=%d (global=%b, name=%s)\n" opname arg global name ;
    let loc = Env.loc env in
    let env, cell = pop_datastack opname env in
    match (cell : DataStack.cell) with
    | Import {import_path; symbols} ->
        let symbol_info = Symbol.Import {import_path} in
        let already_imported, env = Env.register_symbol ~global env name symbol_info in
        let env =
          (* Side effects from imports are only run once *)
          if already_imported then env
          else
            let enclosing_class = type_name ~loc import_path in
            let proc =
              qualified_procname ~enclosing_class @@ proc_name ~loc PyCommon.toplevel_function
            in
            let env, _ = FUNCTION.CALL.mk env None loc proc [] in
            env
        in
        let env =
          List.fold_left ~init:env symbols ~f:(fun env symbol ->
              let symbol_name = Symbol.Qualified.mk ~prefix:[import_path] symbol loc in
              let typ = PyCommon.pyObject in
              let symbol_info = Symbol.Name {symbol_name; is_imported= true; typ} in
              snd @@ Env.register_symbol ~global env symbol symbol_info )
        in
        (env, None)
    | _ ->
        store env code opname loc arg cell name global is_attr
end

module POP_TOP = struct
  (** {v POP_TOP v}

      Pop the top-of-stack and discard it *)
  let run env {FFI.Instruction.opname} =
    Debug.p "[%s]\n" opname ;
    let env, _cell = pop_datastack opname env in
    (env, None)
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
    let env, tos = pop_datastack opname env in
    let env, tos1 = pop_datastack opname env in
    let env, lhs, _ = load_cell env code tos1 in
    let lhs = match lhs with Ok lhs -> lhs | Error s -> L.die InternalError "[%s] %s" opname s in
    let env, rhs, _ = load_cell env code tos in
    let rhs = match rhs with Ok rhs -> rhs | Error s -> L.die InternalError "[%s] %s" opname s in
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

module BUILD = struct
  module CONST_KEY_MAP = struct
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
      let env, cell = pop_datastack opname env in
      let keys =
        match is_tuple_ids code cell with
        | Ok keys ->
            List.rev keys
        | Error () ->
            L.die UserError "[%s] expecting tuple of literal keys" opname
      in
      (* TODO check cells is a tuple of literal strings *)
      let env, values = pop_n_datastack opname env arg [] in
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

(** Return the offset of the next opcode, if any *)
let offset_of_code instructions =
  match instructions with FFI.Instruction.{offset} :: _ -> Some offset | _ -> None


let next_offset opt =
  match opt with
  | None ->
      L.die InternalError "Relative jump forward at the end of code"
  | Some offset ->
      offset


type jump_info = {label_info: Env.Label.info; offset: int; ssa_args: T.Exp.t list}

type jump = OneWay of jump_info | TwoWay of T.BoolExp.t * jump_info * jump_info

let mk_jump loc jump_info =
  let node_info {label_info; ssa_args} =
    let label_name = Env.Label.name label_info in
    let label = {T.NodeName.value= label_name; loc} in
    {T.Terminator.label; ssa_args}
  in
  let jmp node = T.Terminator.Jump [node_info node] in
  match jump_info with
  | OneWay info ->
      jmp info
  | TwoWay (bexp, then_, else_) ->
      let then_ = jmp then_ in
      let else_ = jmp else_ in
      T.Terminator.If {bexp; then_; else_}


(** When reaching a jump instruction with a non empty datastack, turn it into loads/ssa variables
    and properly set the jump/label ssa_args and ssa_parameters *)
let stack_to_ssa env code =
  let env, zipped =
    Env.map ~env (Env.stack env) ~f:(fun env cell ->
        let env, exp, {Env.typ} = load_cell env code cell in
        match exp with
        | Ok exp ->
            (env, (exp, typ))
        | Error s ->
            L.die InternalError "[stack_to_ssa] %s" s )
  in
  (Env.reset_stack env, List.unzip zipped)


(* Helper to fetch label info if there's already one registered at the specified offset, or
   create a fresh one. *)
let mk_label_info env ?prelude offset ~ssa_parameters =
  match Env.label_of_offset env offset with
  | Some label_info ->
      (env, label_info)
  | None ->
      let env, label_name = Env.mk_fresh_label env in
      let label_info = Env.Label.mk ~ssa_parameters ?prelude label_name in
      (env, label_info)


module JUMP = struct
  type kind =
    | Label of jump_info
    | Return of T.Terminator.t
    | TwoWay of {condition: T.BoolExp.t; next_info: jump_info; other_info: jump_info}
    | Absolute of jump_info

  module POP_IF = struct
    (** {v POP_JUMP_IF_TRUE(target) v}

        If top-of-stack is true, sets the bytecode counter to target. top-of-stack is popped.

        {v POP_JUMP_IF_FALSE(target) v}

        If top-of-stack is false, sets the bytecode counter to target. top-of-steack is popped. *)
    let run ~next_is_true env code {FFI.Instruction.opname; arg} next_offset_opt =
      Debug.p "[%s] target = %d\n" opname arg ;
      let env, tos = pop_datastack opname env in
      let env, cell, _ = load_cell env code tos in
      let cond =
        match cell with Ok cond -> cond | Error s -> L.die InternalError "[%s] %s" opname s
      in
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      let next_offset = next_offset next_offset_opt in
      let env, next_info = mk_label_info env next_offset ~ssa_parameters in
      let env, other_info = mk_label_info env arg ~ssa_parameters in
      (* Compute the relevant pruning expressions *)
      let env, id, _ = Env.mk_builtin_call env Builtin.IsTrue [cond] in
      let condT = T.BoolExp.Exp (Var id) in
      let condition = if next_is_true then condT else T.BoolExp.Not condT in
      ( env
      , Some
          (TwoWay
             { condition
             ; next_info= {label_info= next_info; offset= next_offset; ssa_args}
             ; other_info= {label_info= other_info; offset= arg; ssa_args} } ) )
  end

  module IF_OR_POP = struct
    (** {v JUMP_IF_TRUE_OR_POP(target) v}

        If top-of-stack is true, sets the bytecode counter to target and leaves the top-of-stack on
        the stack. Otherwise (top-of-stack is false), top-of_stack is popped.

        {v JUMP_IF_FALSE_OR_POP(target) v}

        If top-of-stack is false, sets the bytecode counter to target and leaves top-of-stack on the
        stack. Otherwise (top-of-stack is true), top-of-stack is popped. *)
    let run ~jump_if env code {FFI.Instruction.opname; arg} next_offset_opt =
      Debug.p "[%s] target = %d\n" opname arg ;
      (* We only peek the top of stack so the logic of [stack_to_ssa] correctly captures it for
         the branch where "it stays". It will be restored as part of the stack restore logic when
         we process the "other" node. *)
      let tos = peek_datastack opname env in
      let env, cell, _ = load_cell env code tos in
      let cond =
        match cell with Ok cond -> cond | Error s -> L.die InternalError "[%s] %s" opname s
      in
      (* Suggestion from @dpichardie: I could put the [IsTrue] expression directly in [condition].
         This would need a refactor of [mk_builtin_call] to get the full expression instead of
         the id that binds it. *)
      let env, id, _ = Env.mk_builtin_call env Builtin.IsTrue [cond] in
      let condT = T.BoolExp.Exp (Var id) in
      let condition = if jump_if then T.BoolExp.Not condT else condT in
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      let next_offset = next_offset next_offset_opt in
      (* In the next branch, we make sure the top-of-stack is no longer in the ssa parameters
         so it is not restored, effectively dropped as per the opcode description. *)
      let env, next_info =
        let ssa_parameters = List.tl ssa_parameters |> Option.value ~default:[] in
        mk_label_info env next_offset ~ssa_parameters
      in
      let next_ssa_args = List.tl ssa_args |> Option.value ~default:[] in
      let env, other_info = mk_label_info env arg ~ssa_parameters in
      ( env
      , Some
          (TwoWay
             { condition
             ; next_info= {label_info= next_info; offset= next_offset; ssa_args= next_ssa_args}
             ; other_info= {label_info= other_info; offset= arg; ssa_args} } ) )
  end

  module FORWARD = struct
    (** {v JUMP_FORWARD(delta) v}

        Increments bytecode counter by [delta]. *)
    let run env code {FFI.Instruction.opname; arg= delta} next_offset_opt =
      Debug.p "[%s] delta = %d\n" opname delta ;
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let next_offset = next_offset next_offset_opt in
      let offset = next_offset + delta in
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      let env, label_info = mk_label_info env offset ~ssa_parameters in
      (env, Some (Absolute {ssa_args; label_info; offset}))
  end

  module ABSOLUTE = struct
    (** {v JUMP_ABSOLUTE(target) v}

        Set bytecode counter to [target]. Can target a previous offset. *)

    let run env code {FFI.Instruction.opname; arg= target; offset} =
      Debug.p "[%s] target = %d\n" opname target ;
      let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
      (* sanity check: we should already have allocated a label for this jump, if it is a backward
         edge. *)
      let () =
        if target < offset then
          let opt_info = Env.label_of_offset env target in
          if Option.is_none opt_info then
            L.die UserError
              "[%s] invalid input, can't find the target of a back-edge from offset %d to offset %d"
              opname offset target
      in
      let env, label_info = mk_label_info env target ~ssa_parameters in
      (env, Some (Absolute {ssa_args; label_info; offset= target}))
  end
end

module RETURN_VALUE = struct
  (** {v RETURN_VALUE v}

      Returns the top-of-stack *)
  let run env code {FFI.Instruction.opname} =
    Debug.p "[%s]\n" opname ;
    let env, cell = pop_datastack opname env in
    let env, exp, _ = load_cell env code cell in
    match exp with
    | Ok exp ->
        let term = T.Terminator.Ret exp in
        (env, Some (JUMP.Return term))
    | Error s ->
        L.die InternalError "[%s] %s" opname s
end

module ITER = struct
  module GET = struct
    (** {v GET_ITER v}

        Implements top-of-stack = iter(top-of-stack). Most of the type calls the [__iter__()] method
        on the top of the stack. Might be C code for builtin types, so we'll model it as a builtin *)
    let run env code {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let env, cell = pop_datastack opname env in
      let env, exp, _ = load_cell env code cell in
      match exp with
      | Ok exp ->
          let env, id, _ = Env.mk_builtin_call env Builtin.PythonIter [exp] in
          let env = Env.push env (DataStack.Temp id) in
          (env, None)
      | Error s ->
          L.die InternalError "[%s] %s" opname s
  end

  module FOR = struct
    (** {v FOR_ITER(delta) v}

        top-of-stack is an iterator. Call its [__next__()] method. If this yields a new value, push
        it on the stack (leaving the iterator below it). If the iterator indicates it is exhausted
        top-of-stack is popped, and the byte code counter is incremented by delta. *)
    let run env code {FFI.Instruction.opname; arg= delta} next_offset_opt =
      Debug.p "[%s] delta = %d\n" opname delta ;
      let env, iter_cell = pop_datastack opname env in
      let env, iter_exp, _ = load_cell env code iter_cell in
      match iter_exp with
      | Ok iter ->
          let loc = Env.loc env in
          let env, id, _ = Env.mk_builtin_call env Builtin.PythonIterNext [iter] in
          let has_item = T.Exp.Field {exp= T.Exp.Var id; field= PyCommon.py_iter_item_has_item} in
          let has_item_info = Env.{typ= T.Typ.Int; is_code= false; is_class= false} in
          let env, has_item_id = Env.mk_fresh_ident env has_item_info in
          let has_item_load = T.Instr.Load {id= has_item_id; exp= has_item; typ= T.Typ.Int; loc} in
          let env = Env.push_instr env has_item_load in
          let cond = T.Exp.Var has_item_id in
          let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
          (* Compute the relevant pruning expressions *)
          let condition = T.BoolExp.Exp cond in
          (* In the next branch, we know the iterator has an item available. Let's fetch it and
             push it on the stack. *)
          let next_prelude loc env =
            (* The iterator object stays on the stack while in the for loop, let's push it back *)
            let env = Env.push env iter_cell in
            let next_item =
              T.Exp.Field {exp= T.Exp.Var id; field= PyCommon.py_iter_item_next_item}
            in
            (* TODO: try to track iterator types *)
            let next_item_info = Env.{typ= PyCommon.pyObject; is_code= false; is_class= false} in
            let env, next_item_id = Env.mk_fresh_ident env next_item_info in
            let next_item_load =
              T.Instr.Load {id= next_item_id; exp= next_item; typ= PyCommon.pyObject; loc}
            in
            let env = Env.push_instr env next_item_load in
            Env.push env (DataStack.Temp next_item_id)
          in
          let next_offset = next_offset next_offset_opt in
          let env, next_info =
            mk_label_info env next_offset ~prelude:next_prelude ~ssa_parameters
          in
          let other_offset = delta + next_offset in
          let env, other_info = mk_label_info env other_offset ~ssa_parameters in
          ( env
          , Some
              (JUMP.TwoWay
                 { condition
                 ; next_info= {label_info= next_info; offset= next_offset; ssa_args}
                 ; other_info= {label_info= other_info; offset= other_offset; ssa_args} } ) )
      | Error s ->
          L.die InternalError "[%s] %s" opname s
  end
end

module IMPORT = struct
  (* Specialized version of [py_to_exp] when an int is expected *)
  let py_to_int c = match (c : FFI.Constant.t) with PYCInt i -> Some i | _ -> None

  (* Specialized version of [py_to_exp] when a tuple of strings is expected (for imports) *)
  let rec py_to_strings c =
    match (c : FFI.Constant.t) with
    | PYCString s ->
        Some [s]
    | PYCBytes bs ->
        let s = Bytes.to_string bs in
        Some [s]
        (* TODO: unsure *)
    | PYCTuple arr ->
        Array.fold ~init:(Some [])
          ~f:(fun acc c ->
            match (py_to_strings c, acc) with Some hd, Some tl -> Some (hd @ tl) | _, _ -> None )
          arr
    | PYCNone ->
        Some []
    | _ ->
        None


  let load_int {FFI.Code.co_consts} cell =
    match (cell : DataStack.cell) with Const ndx -> py_to_int co_consts.(ndx) | _ -> None


  let load_strings {FFI.Code.co_consts} cell =
    match (cell : DataStack.cell) with Const ndx -> py_to_strings co_consts.(ndx) | _ -> None


  module NAME = struct
    (** {v IMPORT_NAME(namei) v}

        Imports the module [co_names\[namei\]]. The two top elements from the the stack are popped
        and provide the [fromlist] and [level] arguments of [__import__()]. The module object is
        pushed onto the stack. The current namespace is not affected: for a proper import statement,
        a subsequent STORE_FAST instruction modifies the namespace.

        For now:

        - we only support [level = 0].
        - we don't support renamaing using [import foo as bar].
        - we don't support complex paths like [import foo.bar.baz]. *)
    let run env ({FFI.Code.co_names} as code) {FFI.Instruction.opname; arg} =
      Debug.p "[%s] namei = %d\n" opname arg ;
      let env, from_list = pop_datastack opname env in
      let env, level = pop_datastack opname env in
      let from_list = load_strings code from_list in
      let level = load_int code level in
      ( match level with
      | Some level ->
          if not (Int64.equal level Int64.zero) then
            L.die InternalError "[%s] missing support from [fromlist] or [level]" opname
      | None ->
          L.die UserError "[%s] failed to load [level] from the stack" opname ) ;
      (* TODO: split the path on '.'. For now, only support [import foo] *)
      let import_path = co_names.(arg) in
      let env = Env.register_import env (Env.Import.TopLevel import_path) in
      let from_list =
        if Option.is_none from_list then
          L.debug Capture Quiet "[IMPORT_NAME] failed to process `from_list`\n" ;
        Option.value ~default:[] from_list
      in
      let env = Env.push env (DataStack.Import {import_path; symbols= from_list}) in
      (env, None)
  end

  module FROM = struct
    (** {v IMPORT_FROM(namei) v}

        Loads the attribute [co_names\[namei\]] from the module found in the top of the stack. The
        resulting object is pushed onto the stack, to be subsequently stored by a [STORE_FAST]
        instruction. *)
    let run env {FFI.Code.co_names} {FFI.Instruction.opname; arg} =
      Debug.p "[%s] namei = %d\n" opname arg ;
      let symbol_name = co_names.(arg) in
      let import_cell = peek_datastack opname env in
      match (import_cell : DataStack.cell) with
      | Import {import_path; symbols} ->
          let env =
            if List.mem ~equal:String.equal symbols symbol_name then
              (* TODO: should we do more structure ? I just reuse `Import` with a single name
                 instead of the full list. *)
              Env.push env (DataStack.Import {import_path; symbols= [symbol_name]})
            else (
              L.debug Capture Quiet "[IMPORT_FROM] symbol %s is not part of the expect list [%s]\n"
                symbol_name (String.concat ~sep:", " symbols) ;
              env )
          in
          (env, None)
      | _ ->
          L.die UserError "[%s] invalid TOS: %a spotted" opname DataStack.pp_cell import_cell
  end
end

module COMPARE_OP = struct
  (** {v COMPARE_OP(opname) v}

      Performs a Boolean operation. The operation name can be found in [cmp_op\[opname\]].

      [ dis.cmp_op = ('<', '<=', '==', '!=', '>', '>=', 'in', 'not in', 'is', 'is not', 'exception match', 'BAD') ] *)
  let run env code {FFI.Instruction.opname; arg} =
    let cmp_op =
      match List.nth Builtin.Compare.all arg with
      | Some op ->
          op
      | None ->
          L.die ExternalError
            "COMPARE_OP[%d] is not an existing cmp_op.  Please refer to `dis.cmp_op`" arg
    in
    Debug.p "[%s] cmp_op = %a\n" opname Builtin.Compare.pp cmp_op ;
    let env =
      match cmp_op with
      | Lt | Le | Gt | Ge | In | NotIn | Is | IsNot | Exception | BAD ->
          L.die InternalError "TODO: support for COMPARE_OP[%a] is not yet implemented"
            Builtin.Compare.pp cmp_op
      | Neq | Eq ->
          let env, rhs = pop_datastack opname env in
          let env, lhs = pop_datastack opname env in
          let env, lhs, _ = load_cell env code lhs in
          let lhs =
            match lhs with Ok lhs -> lhs | Error s -> L.die InternalError "[%s] %s" opname s
          in
          let env, rhs, _ = load_cell env code rhs in
          let rhs =
            match rhs with Ok rhs -> rhs | Error s -> L.die InternalError "[%s] %s" opname s
          in
          let env, id, _typ = Env.mk_builtin_call env (Builtin.CompareOp cmp_op) [lhs; rhs] in
          Env.push env (DataStack.Temp id)
    in
    (env, None)
end

(** Main opcode dispatch function. *)
let run_instruction env code ({FFI.Instruction.opname; starts_line} as instr) next_offset_opt =
  let env = Env.update_last_line env starts_line in
  (* TODO: there are < 256 opcodes, could setup an array of callbacks instead *)
  let env, maybe_term =
    match opname with
    | "LOAD_ATTR" ->
        LOAD.run ATTR env code instr
    | "LOAD_CONST" ->
        LOAD.run CONST env code instr
    | "LOAD_FAST" ->
        LOAD.run FAST env code instr
    | "LOAD_GLOBAL" ->
        LOAD.run GLOBAL env code instr
    | "LOAD_NAME" ->
        LOAD.run NAME env code instr
    | "STORE_ATTR" ->
        STORE.run ATTR env code instr
    | "STORE_FAST" ->
        STORE.run FAST env code instr
    | "STORE_GLOBAL" ->
        STORE.run GLOBAL env code instr
    | "STORE_NAME" ->
        STORE.run NAME env code instr
    | "RETURN_VALUE" ->
        RETURN_VALUE.run env code instr
    | "POP_TOP" ->
        POP_TOP.run env instr
    | "CALL_FUNCTION" ->
        FUNCTION.CALL.run env code instr
    | "BINARY_ADD" ->
        BINARY_ADD.run env code instr
    | "MAKE_FUNCTION" ->
        FUNCTION.MAKE.run env code instr
    | "POP_JUMP_IF_TRUE" ->
        JUMP.POP_IF.run ~next_is_true:false env code instr next_offset_opt
    | "POP_JUMP_IF_FALSE" ->
        JUMP.POP_IF.run ~next_is_true:true env code instr next_offset_opt
    | "JUMP_FORWARD" ->
        JUMP.FORWARD.run env code instr next_offset_opt
    | "JUMP_ABSOLUTE" ->
        JUMP.ABSOLUTE.run env code instr
    | "GET_ITER" ->
        ITER.GET.run env code instr
    | "FOR_ITER" ->
        ITER.FOR.run env code instr next_offset_opt
    | "BUILD_CONST_KEY_MAP" ->
        BUILD.CONST_KEY_MAP.run env code instr
    | "LOAD_BUILD_CLASS" ->
        LOAD.BUILD_CLASS.run env instr
    | "LOAD_METHOD" ->
        METHOD.LOAD.run env code instr
    | "CALL_METHOD" ->
        METHOD.CALL.run env code instr
    | "IMPORT_NAME" ->
        IMPORT.NAME.run env code instr
    | "IMPORT_FROM" ->
        IMPORT.FROM.run env code instr
    | "COMPARE_OP" ->
        COMPARE_OP.run env code instr
    | "JUMP_IF_TRUE_OR_POP" ->
        JUMP.IF_OR_POP.run ~jump_if:true env code instr next_offset_opt
    | "JUMP_IF_FALSE_OR_POP" ->
        JUMP.IF_OR_POP.run ~jump_if:false env code instr next_offset_opt
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
          let next_offset_opt = offset_of_code rest in
          let env, maybe_term = run_instruction env code instr next_offset_opt in
          if Option.is_some maybe_term then (env, maybe_term, rest) else run env code rest
      | Some (_offset, maybe_backedge, label_info) ->
          (* Yes, let's stop there, and don't forget to keep [instr] around *)
          let env, (ssa_args, ssa_parameters) = stack_to_ssa env code in
          let label_info =
            if maybe_backedge then Env.Label.update_ssa_parameters label_info ssa_parameters
            else label_info
          in
          let offset = offset_of_code instructions |> next_offset in
          let info = {label_info; offset; ssa_args} in
          (env, Some (JUMP.Label info), instructions) )


(** Return the location of the first available instruction, if any *)
let first_loc_of_code instructions =
  match instructions with
  | {FFI.Instruction.starts_line= Some line} :: _ ->
      T.Location.known ~line ~col:0
  | _ ->
      T.Location.Unknown


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
  let unconditional_jump env ssa_args label_info offset =
    let env = Env.register_label ~offset label_info env in
    let jump = mk_jump last_loc (OneWay {label_info; offset; ssa_args}) in
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
    (env, node)
  in
  match maybe_term with
  | None ->
      L.die InternalError "Reached the end of code without spotting a terminator"
  | Some (Label info) ->
      let {offset; label_info} = info in
      let env = Env.register_label ~offset label_info env in
      (* A label was spotted without having a clear Textual terminator. Insert a jump to this
         label to create a proper node, and resume the processing. *)
      let jump = mk_jump last_loc (OneWay info) in
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
      Debug.p "  Label: splitting instructions alongside %s\n" (Env.Label.name label_info) ;
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
  | Some (TwoWay {condition; next_info; other_info}) ->
      let {label_info= next_label_info; offset= next_offset} = next_info in
      let {label_info= other_label_info; offset= other_offset} = other_info in
      (* The current node ended up with a two-way jump. Either continue to the "next"
         (fall-through) part of the code, or jump to the "other" section of the code. For this
         purpose, register a fresh label for the jump. *)
      let env = Env.register_label ~offset:next_offset next_label_info env in
      let env = Env.register_label ~offset:other_offset other_label_info env in
      (* Register the jump target *)
      Debug.p "  TwoWay: register %s at %d\n" (Env.Label.name other_label_info) other_offset ;
      let jump = mk_jump last_loc (TwoWay (condition, next_info, other_info)) in
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
  | Some (Absolute {ssa_args; label_info; offset}) ->
      (* The current node ends up with an absolute jump to [target]. *)
      let env, node = unconditional_jump env ssa_args label_info offset in
      Debug.p "  Absolute: register %s at %d\n" (Env.Label.name label_info) offset ;
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


let type_of_annotation env raw_typ =
  match raw_typ with
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
  | _ -> (
      (* TODO: support nesting *)
      let opt = Env.lookup_symbol env ~global:true raw_typ in
      let typ = Option.bind ~f:Symbol.to_typ opt in
      match typ with
      | Some typ ->
          typ
      | None ->
          Debug.p "[type_of_annotation] unsupported type: %s (%a)\n" raw_typ (Pp.option Symbol.pp)
            opt ;
          PyCommon.pyObject )


let annotated_type_of_annotation env typ =
  let typ = type_of_annotation env typ in
  T.Typ.{typ; attributes= []}


(** Process a single code unit (toplevel code, function body, ...) *)
let to_proc_desc env loc enclosing_class_name opt_name ({FFI.Code.instructions} as code) =
  Debug.p "[to_proc_desc] %s %a\n" enclosing_class_name (Pp.option F.pp_print_string) opt_name ;
  let is_toplevel, is_static, name, enclosing_class_name, annotations =
    match opt_name with
    | None ->
        let return_typ = {PyCommon.name= PyCommon.return; annotation= "None"} in
        (true, false, PyCommon.toplevel_function, enclosing_class_name, [return_typ])
    | Some name -> (
        let annotations = Env.lookup_method env ~enclosing_class:enclosing_class_name name in
        match annotations with
        | None ->
            (false, false, name, enclosing_class_name, [])
        | Some {Env.Signature.is_static; annotations} ->
            let enclosing_class_name =
              if is_static then PyCommon.static_companion enclosing_class_name
              else enclosing_class_name
            in
            (false, is_static, name, enclosing_class_name, annotations) )
  in
  let proc_name = proc_name ~loc name in
  let enclosing_class = type_name ~loc enclosing_class_name in
  let qualified_name = qualified_procname ~enclosing_class proc_name in
  let pyObject = T.Typ.{typ= PyCommon.pyObject; attributes= []} in
  let params = FFI.Code.get_arguments code in
  let params = Array.to_list params in
  (* TODO: pass the locals in Env as it affects function lookup:
     def f(x):
       print(x)

     def g():
       f(10) # works

     def h():
       f(10)   # error, since f is listed as a local and seems to be used before being defined
       f = 42
  *)
  (* Create the original environment for this code unit *)
  let env = Env.enter_proc ~is_toplevel ~is_static ~module_name:enclosing_class_name ~params env in
  let locals = FFI.Code.get_locals code in
  let params = List.map ~f:(var_name ~loc) params in
  let locals = Array.map ~f:(fun name -> (var_name ~loc name, pyObject)) locals |> Array.to_list in
  let types =
    List.map
      ~f:(fun {PyCommon.name; annotation} ->
        let annotation = annotated_type_of_annotation env annotation in
        (name, annotation) )
      annotations
  in
  let env, formals_types =
    Env.map ~env
      ~f:(fun env {T.VarName.value; loc} ->
        let typ = List.Assoc.find types ~equal:String.equal value in
        let typ = Option.value typ ~default:pyObject in
        let symbol_name = Symbol.Qualified.mk ~prefix:[] value loc in
        let sym = Symbol.Name {symbol_name; is_imported= false; typ= typ.T.Typ.typ} in
        let env = Env.register_symbol env ~global:false value sym in
        (snd env, typ) )
      params
  in
  let result_type =
    List.Assoc.find types ~equal:String.equal PyCommon.return |> Option.value ~default:pyObject
  in
  let procdecl =
    {T.ProcDecl.qualified_name; formals_types= Some formals_types; result_type; attributes= []}
  in
  let env, entry_label = Env.mk_fresh_label env in
  let label = node_name ~loc entry_label in
  let label_info = Env.Label.mk entry_label in
  let env, nodes = nodes env label_info code instructions in
  (env, {Textual.ProcDesc.procdecl; nodes; start= label; params; locals; exit_loc= Unknown})


(* For each class declaration, we generate an explicit constructor if there
   was an explicit [__init__] method.


   define filename::classname(args): *classname {
     #entry:
       n0 = __sil_allocate(<classname>)
       ni... = load args...
       n0.classname.__init__(ni...)
       ret n0
   }

   Note that the constructor doesn't require self, but it returns it. In the
   code above, [n0] is self.

   If there is no [__init__] methods, we generate stubs and will do the logic
   in Pulse: allocate a value of the right type and use method resolution to
   locate the relevant [__init__] method in the class hierarchy.

   TODO: we currently do not support __new__ being overriden by the user.
*)
let constructor env full_name loc has_init =
  let mk typ = annotated_type_of_annotation env typ in
  let struct_ = T.Typ.(Struct (type_name ~loc full_name)) in
  let sil_allocate : T.qualified_procname =
    {enclosing_class= TopLevel; name= proc_name T.builtin_allocate}
  in
  let alloc = T.Exp.call_non_virtual sil_allocate [T.Exp.Typ struct_] in
  let n0 = T.Ident.of_int 0 in
  let instr0 = T.Instr.Let {id= n0; exp= alloc; loc} in
  let instr_load_and_call, params, formals_types =
    let params, formals_types =
      (* We skip self since the constructor is generating it, and in Python self is always the
         first method argument. We also skip [return] as [__init__] should return [None] but
         the constructor should return a valid instance of the current class *)
      let signature = List.tl has_init |> Option.value ~default:[] in
      List.fold_right signature ~init:([], [])
        ~f:(fun {PyCommon.name; annotation} (params, formals_types) ->
          if String.equal PyCommon.return name then (params, formals_types)
          else (var_name ~loc name :: params, mk annotation :: formals_types) )
    in
    (* [List.zip_exn] is fine as we just built them above and made sure they are of the same length. *)
    let typed_params = List.zip_exn params formals_types in
    let n, rev_ns, rev_loads =
      List.fold_left typed_params
        ~init:(T.Ident.next n0, [], [])
        ~f:(fun (next_id, ids, instrs) (varname, {T.Typ.typ}) ->
          let exp = T.Exp.Lvar varname in
          let instr = T.Instr.Load {id= next_id; exp; typ; loc} in
          (T.Ident.next next_id, T.Exp.Var next_id :: ids, instr :: instrs) )
    in
    let ns = List.rev rev_ns in
    let enclosing_class = type_name ~loc full_name in
    let init = proc_name ~loc PyCommon.init__ in
    let init = qualified_procname ~enclosing_class init in
    let init = T.Exp.call_virtual init (T.Exp.Var n0) ns in
    let load = T.Instr.Let {id= n; exp= init; loc} in
    let loads = List.rev (load :: rev_loads) in
    (loads, params, formals_types)
  in
  let result_type = T.Typ.mk_without_attributes @@ PyCommon.mk_type full_name in
  let last = T.Terminator.Ret (T.Exp.Var n0) in
  let label = node_name ~loc PyCommon.entry in
  let node =
    { T.Node.label
    ; ssa_parameters= []
    ; exn_succs= []
    ; last
    ; instrs= instr0 :: instr_load_and_call
    ; last_loc= loc
    ; label_loc= loc }
  in
  let procdecl =
    { T.ProcDecl.qualified_name= {enclosing_class= TopLevel; name= proc_name ~loc full_name}
    ; formals_types= Some formals_types
    ; result_type
    ; attributes= [] }
  in
  let desc =
    {T.ProcDesc.procdecl; nodes= [node]; start= label; params; locals= []; exit_loc= loc}
  in
  T.Module.Proc desc


(** When a class doesn't define its [__init__] method, we only declare the signature of [__init__]
    and its constructor. *)
let constructor_stubs ~kind loc class_name =
  let qualified_name, result_type =
    match kind with
    | `Ctor ->
        let qualified_name : T.qualified_procname =
          {T.enclosing_class= TopLevel; name= proc_name ~loc class_name}
        in
        let result_type = T.Typ.mk_without_attributes @@ PyCommon.mk_type class_name in
        (qualified_name, result_type)
    | `Init ->
        let qualified_name : T.qualified_procname =
          { T.enclosing_class= Enclosing (type_name ~loc class_name)
          ; name= proc_name ~loc PyCommon.init__ }
        in
        let result_type = T.Typ.mk_without_attributes @@ PyCommon.pyNone in
        (qualified_name, result_type)
  in
  T.Module.Procdecl {T.ProcDecl.qualified_name; formals_types= None; result_type; attributes= []}


(** Process the special function generated by the Python compiler to create a class instances. This
    is where we can see some of the type annotations for fields, and method definitions. *)
let rec class_declaration env module_name ({FFI.Code.instructions; co_name} as code) loc parent =
  Debug.p "[class declaration] %s (%a)\n" co_name (Pp.option Symbol.pp) parent ;
  (* TODO:
     - use annotations to declare class member types
     - pass in [env] to PyClassDecl when we'll support decorators
  *)
  let class_name = prefix_name ~module_name co_name in
  let static_class_name = prefix_name ~module_name (PyCommon.static_companion co_name) in
  let class_type_name = type_name ~loc class_name in
  let static_class_type_name = type_name ~loc static_class_name in
  let {PyClassDecl.members; methods; static_methods; has_init; has_new} =
    PyClassDecl.parse_class_declaration code co_name instructions
  in
  let register_methods env codes method_infos opname ~is_static =
    List.fold_left method_infos ~init:(env, codes)
      ~f:(fun (env, codes) {PyCommon.code; signature; flags} ->
        check_flags opname flags ;
        let env =
          match FFI.Constant.as_code code with
          | Some code ->
              let info = {PyEnv.Signature.is_static; annotations= signature} in
              Env.register_method env ~enclosing_class:class_name ~method_name:code.FFI.Code.co_name
                info
          | None ->
              env
        in
        (env, code :: codes) )
  in
  if Option.is_some has_new then
    L.die InternalError "TODO: No support for __new__ in user declared classes" ;
  let env, codes = register_methods env [] methods "<METHOD_DECLARATION>" ~is_static:false in
  let env, codes =
    register_methods env codes static_methods "<STATIC_METHOD_DECLARATION>" ~is_static:true
  in
  let fields =
    List.map members ~f:(fun {PyCommon.name; annotation} ->
        let name = field_name ~loc name in
        let qualified_name = {T.enclosing_class= class_type_name; name} in
        let typ = type_of_annotation env annotation in
        {T.FieldDecl.qualified_name; typ; attributes= []} )
  in
  let module_name = prefix_name ~module_name co_name in
  let env, decls = to_proc_descs env module_name (Array.of_list codes) in
  let supers, static_supers =
    Option.value_map ~default:([], [])
      ~f:(fun parent ->
        let supers = [Symbol.to_type_name ~is_static:false parent] in
        let static_supers = [Symbol.to_type_name ~is_static:true parent] in
        (supers, static_supers) )
      parent
  in
  let t = T.Module.Struct {name= class_type_name; supers; fields; attributes= []} in
  let companion =
    T.Module.Struct
      { name= static_class_type_name
      ; supers= static_supers
      ; fields= []
      ; attributes= [T.Attr.mk_static] }
  in
  (* TODO(vsiles) compare with Hack's get_static_companion when we'll introduce static members *)
  let companion_const =
    T.Module.Global {name= var_name ~loc static_class_name; typ= PyCommon.pyObject; attributes= []}
  in
  let ctor_and_init =
    match has_init with
    | None ->
        let ctor = constructor_stubs ~kind:`Ctor loc class_name in
        let init = constructor_stubs ~kind:`Init loc class_name in
        [init; ctor]
    | Some has_init ->
        (* [__init__] is already present in the source code, only generate the constructor. *)
        let ctor = constructor env class_name loc has_init in
        [ctor]
  in
  (env, (t :: companion :: companion_const :: ctor_and_init) @ decls)

(* TODO: No support for nested functions/methods at the moment *)


(** Process multiple [code] objects. Usually called by the toplevel function or by a class
    definition. *)
and to_proc_descs env enclosing_class_name codes =
  Debug.p "[to_proc_descs] %s\n" enclosing_class_name ;
  Array.fold codes ~init:(env, []) ~f:(fun (env, decls) const ->
      match FFI.Constant.as_code const with
      | None ->
          (env, decls)
      | Some ({FFI.Code.co_name; instructions} as code) -> (
          let loc = first_loc_of_code instructions in
          let classes = Env.get_declared_classes env in
          match Env.SMap.find_opt co_name classes with
          | Some {Env.parent} ->
              let env, new_decls = class_declaration env enclosing_class_name code loc parent in
              (env, new_decls @ decls)
          | None ->
              let env, decl = to_proc_desc env loc enclosing_class_name (Some co_name) code in
              (env, T.Module.Proc decl :: decls) ) )


let python_attribute = Textual.Attr.mk_source_language Textual.Lang.Python

(** Entry point of the module: process a whole Python file / compilation unit into Textual *)
let to_module ~sourcefile ({FFI.Code.co_consts; co_name; co_filename; instructions} as code) =
  Debug.p "[to_module] %a %s\n" T.SourceFile.pp sourcefile co_name ;
  if not (String.equal co_name "<module>") then
    L.die ExternalError "Toplevel modules must be named '<module>'. Got %s" co_name ;
  let module_name = Stdlib.Filename.remove_extension co_filename in
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
  let env, decl = to_proc_desc env loc module_name None code in
  (* Translate globals to Textual *)
  let globals =
    Env.SMap.fold
      (fun _name sym acc ->
        match (sym : Symbol.t) with
        | Name {is_imported} ->
            if is_imported then acc
            else
              let qname = Symbol.to_string sym in
              let varname = var_name ~loc qname in
              let global = T.Global.{name= varname; typ= PyCommon.pyObject; attributes= []} in
              T.Module.Global global :: acc
        | Builtin | Code _ | Class _ | Import _ ->
            (* don't generate a global variable name, it will be declared as a toplevel decl *)
            acc )
      (Env.globals env) []
  in
  (* Then, process any code body that is in code.co_consts *)
  let env, decls = to_proc_descs env module_name co_consts in
  let decls = List.rev decls in
  (* Declare all the import top level calls *)
  let imports = Env.get_textual_imports env in
  (* Gather everything into a Textual module *)
  let decls =
    ((T.Module.Proc decl :: decls) @ globals @ imports)
    @ Builtin.Set.to_textual (Env.get_used_builtins env)
  in
  {T.Module.attrs= [python_attribute]; decls; sourcefile}
