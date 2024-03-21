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
module Ident = PyCommon.Ident
module SMap = PyCommon.SMap

module Error = struct
  type todo =
    | UnsupportedOpcode of string
    | FunctionFlags of string * MakeFunctionFlags.t
    | StaticCallImport of Ident.t
    | SuperMultipleInheritance of Ident.t
    | Closure of string
    | CompareOp of Builtin.Compare.t
    | Exception of string * DataStack.cell
    | SetupWith of DataStack.cell
    | New of Ident.t
    | CallKwInvalidFunction of DataStack.cell
    | RaiseException of int
    | MakeFunctionDefault of DataStack.cell
    | MakeFunctionSignature of DataStack.cell

  let pp_todo fmt = function
    | UnsupportedOpcode opname ->
        F.fprintf fmt "Unsupported opcode: %s" opname
    | FunctionFlags (opname, flags) ->
        F.fprintf fmt "[%s] support for flags %a is not implemented" opname MakeFunctionFlags.pp
          flags
    | StaticCallImport id ->
        F.fprintf fmt "Unsupported Import %a in static call" Ident.pp id
    | SuperMultipleInheritance id ->
        F.fprintf fmt "Unsupported call to `super` with multiple inheritance in %a" Ident.pp id
    | Closure opname ->
        F.fprintf fmt "[%s] Unsupported statement with closure" opname
    | CompareOp op ->
        F.fprintf fmt "Unsupported compare operation: %a" Builtin.Compare.pp op
    | Exception (opname, cell) ->
        F.fprintf fmt "[%s] support for %a is not implemented at the moment" opname
          DataStack.pp_cell cell
    | SetupWith cell ->
        F.fprintf fmt "Unsupported context manager: %a" DataStack.pp_cell cell
    | New id ->
        F.fprintf fmt "Unsupported '__new__' user declaration in %a" Ident.pp id
    | CallKwInvalidFunction cell ->
        F.fprintf fmt "Unsupported CALL_FUNCTION_KW with %a" DataStack.pp_cell cell
    | RaiseException n ->
        F.fprintf fmt "Unsupported RAISE_VARARGS mode %d" n
    | MakeFunctionDefault cell ->
        F.fprintf fmt "Unsupported function defaults: %a" DataStack.pp_cell cell
    | MakeFunctionSignature cell ->
        F.fprintf fmt "Unsupported type annotation: %a" DataStack.pp_cell cell


  type kind =
    | ClassDecl of PyClassDecl.Error.kind
    | SuperLoadSelf of (string * Ident.t * kind)
    | LiteralTuple of string
    | FFI of FFI.Error.kind
    | TextualParser of Textual.SourceFile.t
    | NotCodeObject of Symbol.kind * Symbol.t
    | UnexpectedBuiltin of Symbol.key
    | LoadFailure of string * Symbol.t option
    | LoadInvalid of Symbol.kind * Ident.t
    | LoadAttribute of DataStack.cell * string
    | TODO of todo
    | EmptyStack of string * string
    | BuiltinClassName of string * string * DataStack.cell
    | BuiltinClassBody of string * DataStack.cell
    | BuiltinClassInvalidParents of string
    | BuiltinClassInvalid
    | CallInvalid of string * DataStack.cell
    | MakeFunctionInvalidCode of DataStack.cell
    | MakeFunctionInvalidName of DataStack.cell
    | MakeFunctionInvalidQname of FFI.Constant.t
    | MakeFunctionInvalidAnnotations of T.Exp.t
    | SuperNotInClass of Ident.t
    | SuperNoParent of Ident.t
    | SuperInStatic of Ident.t
    | SuperNoParameters of Ident.t
    | BuildConstKeyMap of int * int
    | RelativeJumpEOF
    | MissingBackEdge of int * int
    | ImportInvalidLevel
    | ImportLevelTooBig of Z.t
    | ImportInvalidName of string
    | ImportInvalidDepth of Ident.t * int
    | ImportInvalidRoot of DataStack.cell
    | CompareOp of int
    | WithCleanup of DataStack.cell
    | UnpackSequence of int
    | EOF
    | TopLevelName of string
    | TopLevelInvalid of string
    | CallKeywordNotString0 of FFI.Constant.t
    | CallKeywordNotString1 of DataStack.cell
    | CallKeywordBuildClass
    | RaiseExceptionInvalid of int
    | DefaultArgSpecialization of T.QualifiedProcName.t * int * int
    | BuildSliceArity of int

  type t = L.error * kind

  let rec pp_kind fmt = function
    | ClassDecl err ->
        PyClassDecl.Error.pp_kind fmt err
    | SuperLoadSelf (self, current_module, err) ->
        F.fprintf fmt "`super()` failed to load `self` (a.k.a %s) in %a: %a" self Ident.pp
          current_module pp_kind err
    | LiteralTuple opname ->
        F.fprintf fmt "[%s] expecting tuple of literal keys" opname
    | FFI kind ->
        FFI.Error.pp_kind fmt kind
    | TextualParser file ->
        F.fprintf fmt "TextualParser: error while translating module %a" Textual.SourceFile.pp file
    | TODO todo ->
        F.fprintf fmt "[TODO] %a" pp_todo todo
    | NotCodeObject (kind, sym) ->
        F.fprintf fmt "%a %a is not a code object" Symbol.pp_kind kind Symbol.pp sym
    | UnexpectedBuiltin key ->
        F.fprintf fmt "Unexpected builtin %a" Symbol.pp_key key
    | LoadFailure (kind, opt_sym) ->
        F.fprintf fmt "Failure to load %a (%s)" (Pp.option Symbol.pp) opt_sym kind
    | LoadInvalid (kind, id) ->
        F.fprintf fmt "Invalid load of %a (%a)" Ident.pp id Symbol.pp_kind kind
    | LoadAttribute (cell, name) ->
        F.fprintf fmt "Failure to load attribute %s from %a" name DataStack.pp_cell cell
    | EmptyStack (opname, action) ->
        F.fprintf fmt "[%s] can't %s, stack is empty" opname action
    | BuiltinClassName (opname, kind, cell) ->
        F.fprintf fmt "[%s] expected literal (%s) class name but got %a" opname kind
          DataStack.pp_cell cell
    | BuiltinClassBody (opname, cell) ->
        F.fprintf fmt "[%s] Expected code object but got %a" opname DataStack.pp_cell cell
    | BuiltinClassInvalidParents name ->
        F.fprintf fmt "Failure to load parent classes of '%s" name
    | BuiltinClassInvalid ->
        F.pp_print_string fmt "Failure to load class: not enough information can be found"
    | CallInvalid (opname, cell) ->
        F.fprintf fmt "[%s] invalid object to call: %a" opname DataStack.pp_cell cell
    | MakeFunctionInvalidCode cell ->
        F.fprintf fmt "function body is not a code object: %a" DataStack.pp_cell cell
    | MakeFunctionInvalidName cell ->
        F.fprintf fmt "invalid function name: %a" DataStack.pp_cell cell
    | MakeFunctionInvalidQname const ->
        F.fprintf fmt "invalid function qualified name: %a" FFI.Constant.pp const
    | MakeFunctionInvalidAnnotations exp ->
        F.fprintf fmt "invalid annotation. Expecting literal string but got %a" T.Exp.pp exp
    | SuperNotInClass id ->
        F.fprintf fmt "Call to super() in %a which is not a class" Ident.pp id
    | SuperNoParent id ->
        F.fprintf fmt "Call to super() in %a which doesn't have a parent class" Ident.pp id
    | SuperInStatic id ->
        F.fprintf fmt "Call to super() in a static method of class %a" Ident.pp id
    | SuperNoParameters id ->
        F.fprintf fmt "Call to super() in %a with an empty list of parameters" Ident.pp id
    | BuildConstKeyMap (key_length, value_length) ->
        F.fprintf fmt "BUILD_CONST_KEY_MAP: keys length %d does not match values length %d"
          key_length value_length
    | RelativeJumpEOF ->
        F.pp_print_string fmt "Relative jump forward at the end of code"
    | MissingBackEdge (from, to_) ->
        F.fprintf fmt "Invalid absolute jump: missing target of back-edge from %d to %d" from to_
    | ImportInvalidLevel ->
        F.pp_print_string fmt "Failure to load the `level` of import statement"
    | ImportLevelTooBig z ->
        F.fprintf fmt "The import `level` %s is too big" (Z.to_string z)
    | ImportInvalidName name ->
        F.fprintf fmt "Absolute path '%s' for import statement cannot be parsed correctly" name
    | ImportInvalidDepth (id, level) ->
        F.fprintf fmt "Current module path '%a' is not deep enough for import with level %d"
          Ident.pp id level
    | ImportInvalidRoot cell ->
        F.fprintf fmt "Invalid root %a for 'from ... import ...' statement" DataStack.pp_cell cell
    | CompareOp n ->
        F.fprintf fmt "Invalid compare operation %d. Please refer to the definition of `dis.cmp_op`"
          n
    | WithCleanup cell ->
        F.fprintf fmt "SETUP_WITH: expected a 'with' context manager, but got %a" DataStack.pp_cell
          cell
    | UnpackSequence n ->
        F.fprintf fmt "UNPACK_SEQUENCE with count = %d" n
    | EOF ->
        F.pp_print_string fmt "Capture reached the end of code without a terminator statement"
    | TopLevelName name ->
        F.fprintf fmt "Toplevel modules must be named '<module>', but got %s" name
    | TopLevelInvalid filename ->
        F.fprintf fmt "Invalid module path '%s'" filename
    | CallKeywordNotString0 cst ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a string: %a" FFI.Constant.pp cst
    | CallKeywordNotString1 cell ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a string: %a" DataStack.pp_cell cell
    | CallKeywordBuildClass ->
        F.pp_print_string fmt "CALL_FUNCTION_KW cannot be used with LOAD_BUILD_CLASS"
    | RaiseExceptionInvalid n ->
        F.fprintf fmt "RAISE_VARARGS invalid mode %d" n
    | DefaultArgSpecialization (name, param_size, default_size) ->
        F.fprintf fmt "%a has more default arguments (%d) then actual arguments (%d)"
          T.QualifiedProcName.pp name default_size param_size
    | BuildSliceArity n ->
        F.fprintf fmt "Invalid BUILD_SLICE arity. Expected 2 or 3 but got %d" n


  let class_decl kind = (L.InternalError, ClassDecl kind)

  let ffi (err, kind) = (err, FFI kind)

  let textual_parser file = (L.InternalError, TextualParser file)
end

let var_name = PyCommon.var_name

let node_name = PyCommon.node_name

let proc_name = PyCommon.proc_name

let field_name = PyCommon.field_name

let qualified_procname = PyCommon.qualified_procname

let python_implicit_names = ["__name__"; "__file__"]

let python_implicit_names_prefix = "$python_implicit_names"

let mk_key global loc name =
  if global then Symbol.Global (Ident.mk ~global ~loc name) else Symbol.Local name


let lift_type_ident env type_id =
  if Ident.is_primitive_type type_id then Some type_id
  else
    let key = Symbol.Global type_id in
    match Env.lookup_symbol env key with Some {Symbol.kind= Class; id} -> Some id | _ -> None


let lift_annotation env {PyCommon.name; annotation} =
  Option.map ~f:(fun annotation -> {PyCommon.name; annotation}) (lift_type_ident env annotation)


(** Turn a Python [FFI.Code.t] into a Textual [T.Exp.t], along with the required instructions to
    effectively perform the translation. *)
let code_to_exp ~fun_or_class env key =
  let open IResult.Let_syntax in
  let* fname =
    (* TODO: support closures *)
    match Env.lookup_symbol env key with
    | None ->
        (* Corner cases for things we don't support nicely at the moment *)
        let id = Ident.mk_unknown_ident "code" in
        let name = Ident.to_string ~sep:"." id in
        Ok name
    | Some ({Symbol.kind; id} as symbol) -> (
      match (kind : Symbol.kind) with
      | Name _ | Import | ImportCall ->
          Error (L.InternalError, Error.NotCodeObject (kind, symbol))
      | Class ->
          Ok (Ident.to_string ~sep:"::" id)
      | Code ->
          let sep = if fun_or_class then "." else "::" in
          Ok (Ident.to_string ~sep id)
      | Builtin ->
          Error (L.InternalError, Error.UnexpectedBuiltin key) )
  in
  let kind = if fun_or_class then Builtin.PythonCode else Builtin.PythonClass in
  let name = T.Exp.Const (Str fname) in
  let env, id, typ = Env.mk_builtin_call env kind [name] in
  Ok (env, T.Exp.Var id, typ)


(** Turn a Python [FFI.Constant.t] into a Textual [T.Exp.t], along with the required instructions to
    effectively perform the translation. *)
let rec py_to_exp env c =
  match (c : FFI.Constant.t) with
  | PYCBool b ->
      Ok (env, PyCommon.mk_bool b, PyCommon.pyBool)
  | PYCInt z ->
      Ok (env, PyCommon.mk_int z, PyCommon.pyInt)
  | PYCFloat f ->
      Ok (env, PyCommon.mk_float f, PyCommon.pyFloat)
  | PYCComplex _ ->
      L.die InternalError "TODO: complex"
  | PYCString s ->
      Ok (env, PyCommon.mk_string s, PyCommon.pyString)
  | PYCInvalidUnicode _ ->
      L.die InternalError "TODO: invalid unicode"
  | PYCBytes s ->
      Ok (env, PyCommon.mk_bytes s, PyCommon.pyBytes)
  | PYCNone ->
      let exp = T.(Exp.Const Const.Null) in
      Ok (env, exp, T.Typ.Null)
  | PYCCode c ->
      (* We assume it's a function, as classes should only be seen during loading using
         [LOAD_BUILD_CLASS] *)
      let loc = Env.loc env in
      let key = Symbol.Global (Ident.mk ~loc c.FFI.Code.co_name) in
      code_to_exp env ~fun_or_class:true key
  | PYCTuple arr ->
      let open IResult.Let_syntax in
      let l = Array.to_list arr in
      let* env, args =
        Env.map_result ~env l ~f:(fun env c ->
            let* env, e, _ = py_to_exp env c in
            Ok (env, e) )
      in
      let exp = T.Exp.call_non_virtual PyCommon.python_tuple args in
      Ok (env, exp, PyCommon.pyTuple)
  | PYCFrozenSet _ ->
      L.die InternalError "TODO: frozenset"


let annotated_type_of_annotation typ =
  let typ = Ident.to_typ typ in
  T.Typ.{typ; attributes= []}


(** Special case of [load_cell] that we sometime use with a name directly, rather then an index in
    [FFI.Code.t] *)
let load_var_name env loc name =
  let open IResult.Let_syntax in
  let info typ = Env.Info.default typ in
  let exp = T.Exp.Lvar (var_name ~loc name) in
  let loc = Env.loc env in
  let key = Symbol.Local name in
  let opt_sym = Env.lookup_symbol env key in
  Debug.p "loading varname: %s -> %a\n" name (Pp.option Symbol.pp) opt_sym ;
  let* typ =
    match (opt_sym : Symbol.t option) with
    | Some {Symbol.kind= Name {typ}} ->
        Ok typ
    | Some _ ->
        (* Note: maybe PyCommon.pyObject would be better here, but I fail to catch new behavior early *)
        Error (L.InternalError, Error.LoadFailure ("VarName", opt_sym))
    | None ->
        Ok PyCommon.pyObject
  in
  let info = info typ in
  let env, id = Env.mk_fresh_ident env info in
  Debug.p "%a <- %a\n" T.Ident.pp id T.Exp.pp exp ;
  let typ = Some typ in
  let instr = T.Instr.Load {id; exp; typ; loc} in
  let env = Env.push_instr env instr in
  Ok (env, T.Exp.Var id, info)


(** Special case of [load_name] *)
let load_name env loc ~global name =
  let open IResult.Let_syntax in
  let default_info typ = Env.Info.default typ in
  let default = default_info PyCommon.pyObject in
  let* ({Env.Info.typ; kind} as info), qualified_name =
    let key = mk_key global loc name in
    let opt_sym = Env.lookup_symbol env key in
    match (opt_sym : Symbol.t option) with
    | Some {Symbol.kind; id} -> (
      match (kind : Symbol.kind) with
      | Name {typ} ->
          Ok (default_info typ, id)
      | Class ->
          Ok ({Env.Info.kind= Class; typ= PyCommon.pyClass}, id)
      | Builtin ->
          if Ident.is_primitive_type id then Ok ({Env.Info.kind= Class; typ= PyCommon.pyClass}, id)
          else Error (L.InternalError, Error.LoadInvalid (kind, id))
      | Import | ImportCall ->
          Error (L.InternalError, Error.LoadInvalid (kind, id))
      | Code ->
          Ok ({Env.Info.kind= Code; typ= PyCommon.pyCode}, id) )
    | None ->
        (* Some default names like [__file__] and [__name__] are available implicitly in Python *)
        if List.mem ~equal:String.equal python_implicit_names name then
          let typ = PyCommon.pyString in
          (* Note to self: not in builtins because I currently only have functions in there. Might
             be worth merging in the future *)
          let prefix = Ident.mk ~global ~loc python_implicit_names_prefix in
          let id = Ident.extend ~prefix name in
          Ok (default_info typ, id)
        else Ok (default, Ident.mk_unknown_ident name)
  in
  if Env.Info.is_code kind then
    (* If we are trying to load some code, use the dedicated builtin *)
    let fname = Ident.to_string ~sep:"." qualified_name in
    let name = T.Exp.Const (Str fname) in
    let env, id, typ = Env.mk_builtin_call env Builtin.PythonCode [name] in
    let info = {info with Env.Info.typ} in
    Ok (env, T.Exp.Var id, info)
  else if Env.Info.is_class kind then
    (* Class/Type as an expression, use the dedicated builtin *)
    let tyname = T.Exp.Typ (Ident.to_typ qualified_name) in
    let env, id, typ = Env.mk_builtin_call env Builtin.PythonClassName [tyname] in
    let info = {info with Env.Info.typ} in
    Ok (env, T.Exp.Var id, info)
  else
    let var_name = Ident.to_var_name qualified_name in
    let exp = T.Exp.Lvar var_name in
    let loc = Env.loc env in
    let env, id = Env.mk_fresh_ident env info in
    let instr = T.Instr.Load {id; exp; typ= Some typ; loc} in
    let env = Env.push_instr env instr in
    (* TODO: try to trace the type of names, not only global ones ? *)
    Ok (env, T.Exp.Var id, info)


let is_imported env path =
  let f_root env ~global ~loc name =
    let key = mk_key global loc name in
    let opt = Env.lookup_symbol env key in
    match opt with
    | None ->
        false
    | Some {Symbol.kind= Import | ImportCall} ->
        true
    | Some _ ->
        false
  in
  let f_path _ acc = acc in
  Ident.fold ~f_root ~f_path ~init:env path


(** Try to load the data referenced by a [DataStack.cell], into a [Textual.Exp.t] *)
let rec load_cell env cell =
  let open IResult.Let_syntax in
  let default_info typ = Env.Info.default typ in
  let default = default_info PyCommon.pyObject in
  (* Python only stores references to objects on the data stack, so when data needs to be really
     accessed, [load_cell] is used to get information from the code information ([co_consts], ...).
     These data are mapped to Textual.Exp.t values as much as possible. But it's not always
     desirable (see MAKE_FUNCTION) *)
  let loc = Env.loc env in
  Debug.p "[load_cell] %a\n" DataStack.pp_cell cell ;
  match (cell : DataStack.cell) with
  | Const const ->
      let* env, exp, ty = py_to_exp env const in
      let info = default_info ty in
      Ok (env, exp, info)
  | Name {global; name} ->
      load_name env loc ~global name
  | VarName name ->
      load_var_name env loc name
  | Temp id ->
      let info = Env.get_ident_info env id |> Option.value ~default in
      Ok (env, T.Exp.Var id, info)
  | Code {fun_or_class; code} ->
      let key = Symbol.Global (Ident.mk ~loc code.FFI.Code.co_name) in
      let* env, exp, typ = code_to_exp env ~fun_or_class key in
      let kind = if fun_or_class then Env.Info.Code else Env.Info.Class in
      let info = {Env.Info.typ; kind} in
      Ok (env, exp, info)
  | Map map ->
      let* env, items =
        List.fold_result ~init:(env, []) (List.rev map) ~f:(fun (env, acc) (key, value) ->
            let* env, value, _typ = load_cell env value in
            Ok (env, key :: value :: acc) )
      in
      let env, id, typ = Env.mk_builtin_call env (Builtin.PythonBuild Map) items in
      Ok (env, T.Exp.Var id, default_info typ)
  | List (collection, items) ->
      let* env, items = cells_to_textual env items in
      let env, id, typ = Env.mk_builtin_call env (Builtin.PythonBuild collection) items in
      Ok (env, T.Exp.Var id, default_info typ)
  | Path id ->
      if
        (* TODO: this is incomplete. If something is imported, we can' really know if it is a
           value, a function, a class name... In this version, we try to load them as a value, to
           catch the most common patterns like loading [sys.argv] as [let n = load &sys.argv].
           If this is not enough, we'll move to something more abstract and defer to pulse like
           with [let n0 = python_import("sys.argv")]
        *)
        is_imported env id
      then
        let env = Env.register_imported_value env id in
        let typ = PyCommon.pyObject in
        let info = Env.Info.default typ in
        let exp = T.Exp.Lvar (Ident.to_var_name id) in
        let env, id = Env.mk_fresh_ident env info in
        let instr = T.Instr.Load {id; exp; typ= Some typ; loc} in
        let env = Env.push_instr env instr in
        Ok (env, T.Exp.Var id, info)
      else
        let f_root env ~global ~loc name =
          Debug.p "f_root %b %s\n" global name ;
          let* env, exp, info = load_name env loc ~global name in
          let {Env.Info.typ} = info in
          let type_name = match (typ : T.Typ.t) with Ptr (Struct name) -> Some name | _ -> None in
          let fields = Option.bind type_name ~f:(Env.lookup_fields env) in
          Ok (env, exp, info, fields)
        in
        let f_path attribute acc =
          match acc with
          | Error err ->
              Error err
          | Ok (env, exp, _, fields) ->
              let field =
                Option.bind fields
                  ~f:
                    (List.find_map ~f:(fun {PyCommon.name; annotation} ->
                         Option.some_if (String.equal name attribute) annotation ) )
              in
              let lifted_field_typ =
                let open IOption.Let_syntax in
                let* field in
                let* lifted_field_id = lift_type_ident env field in
                let lifted_field_typ = Ident.to_typ lifted_field_id in
                Some lifted_field_typ
              in
              let field_typ = Option.value ~default:PyCommon.pyObject lifted_field_typ in
              let info = Env.Info.default field_typ in
              let env, id = Env.mk_fresh_ident env info in
              let loc = Env.loc env in
              let field_name = field_name ~loc attribute in
              let field = {T.enclosing_class= T.TypeName.wildcard; name= field_name} in
              let exp = T.Exp.Field {exp; field} in
              let instr = T.Instr.Load {id; exp; typ= Some field_typ; loc} in
              let env = Env.push_instr env instr in
              (* Now try to fetch information about the current step of the path *)
              let type_name =
                match (field_typ : T.Typ.t) with Ptr (Struct name) -> Some name | _ -> None
              in
              let new_fields = Option.bind type_name ~f:(Env.lookup_fields env) in
              Ok (env, T.Exp.Var id, info, new_fields)
        in
        let res = Ident.fold ~init:env ~f_root ~f_path id in
        Result.map ~f:(fun (env, exp, info, _fields) -> (env, exp, info)) res
  | NoException ->
      Ok (env, T.Exp.Const T.Const.Null, default_info PyCommon.pyNone)
  | BuiltinBuildClass
  | StaticCall _
  | MethodCall _
  | Import _
  | ImportFrom _
  | ImportCall _
  | Super
  | WithContext _ ->
      L.die InternalError "[load_cell] Can't load %a, something went wrong@\n" DataStack.pp_cell
        cell


and cells_to_textual env cells =
  let open IResult.Let_syntax in
  Env.map_result ~env cells ~f:(fun env cell ->
      let* env, exp, _ = load_cell env cell in
      Ok (env, exp) )


(** Pop the top of the datastack. Fails with an [InternalError] if the stack is empty. *)
let pop_datastack opname env =
  match Env.pop env with
  | None ->
      Error (L.ExternalError, Error.EmptyStack (opname, "pop"))
  | Some (env, cell) ->
      Ok (env, cell)


(** Peek the top of the datastack. Fails with an [InternalError] if the stack is empty. *)
let peek_datastack opname env =
  match Env.peek env with
  | None ->
      Error (L.ExternalError, Error.EmptyStack (opname, "peek"))
  | Some cell ->
      Ok cell


let pop_n_datastack opname env n =
  let open IResult.Let_syntax in
  let rec pop env n acc =
    if n > 0 then (
      let* env, cell = pop_datastack opname env in
      Debug.p "  popped %a\n" DataStack.pp_cell cell ;
      pop env (n - 1) (cell :: acc) )
    else Ok (env, acc)
  in
  Debug.p "[pop_n_datastack]\n" ;
  pop env n []


(* Python opcodes support. Most of the documentation directly comes from the official python
   documentation and is only altered to improve readability.

   https://docs.python.org/3.8/library/dis.html *)

module LOAD = struct
  type kind =
    | ATTR
        (** {v LOAD_ATTR(namei) v}

            Replaces top-of-stack with [getattr(top-of-stack, co_names[namei])]. *)
    | CONST  (** {v LOAD_CONST(consti) v}

                 Pushes [co_consts[consti]] onto the stack. *)
    | FAST
        (** {v LOAD_FAST(var_num) v}

            Pushes a reference to the local [co_varnames[var_num]] onto the stack. *)
    | GLOBAL
        (** {v LOAD_GLOBAL(namei) v}

            Loads the global named [co_names[namei]] onto the stack. *)
    | NAME
        (** {v LOAD_NAME(namei) v}

            Pushes the value associated with [co_names[namei]] onto the stack. *)

  let run kind env ({FFI.Code.co_names; co_varnames; co_consts} as code)
      {FFI.Instruction.opname; arg} =
    let open IResult.Let_syntax in
    let pp {FFI.Code.co_names; co_varnames; co_consts} fmt = function
      | CONST ->
          FFI.Constant.pp fmt co_consts.(arg)
      | FAST ->
          F.fprintf fmt "%s" co_varnames.(arg)
      | ATTR | NAME | GLOBAL ->
          F.fprintf fmt "%s" co_names.(arg)
    in
    let* env, cell =
      match kind with
      | CONST ->
          let const = co_consts.(arg) in
          Ok (env, DataStack.Const const)
      | FAST ->
          let varname = co_varnames.(arg) in
          Ok (env, DataStack.VarName varname)
      | NAME ->
          let name = co_names.(arg) in
          Ok (env, DataStack.Name {global= Env.is_toplevel env; name})
      | GLOBAL ->
          let name = co_names.(arg) in
          if String.equal name "super" then
            let key = Symbol.Global (Ident.mk "super") in
            (* [super] could have been defined in the current scope, so we have to first check
               this, to be sure the call to [super()] is to access the parent's methods, not some
               other symbol *)
            if Option.is_some (Env.lookup_symbol env key) then
              Ok (env, DataStack.Name {global= true; name})
            else Ok (env, DataStack.Super)
          else Ok (env, DataStack.Name {global= true; name})
      | ATTR ->
          let* env, cell = pop_datastack opname env in
          let fname = co_names.(arg) in
          let loc = Env.loc env in
          let* env, cell =
            (* if the cell is Import related, we wont' be able to load things correctly.
               Let's Pulse do its job later *)
            match (cell : DataStack.cell) with
            | ImportCall {id; loc} ->
                let id = Ident.extend ~prefix:id fname in
                Ok (env, DataStack.ImportCall {id; loc})
            | Import {import_path} ->
                let id = Ident.extend ~prefix:import_path fname in
                Ok (env, DataStack.ImportCall {id; loc})
            | Temp v ->
                let exp = T.Exp.Var v in
                let field : T.qualified_fieldname =
                  let enclosing_class = T.TypeName.wildcard in
                  let name = {T.FieldName.value= fname; loc} in
                  {T.enclosing_class; name}
                in
                let access = T.Exp.Field {exp; field} in
                let info = Env.Info.default PyCommon.pyObject in
                let env, id = Env.mk_fresh_ident env info in
                let instr = T.Instr.Load {id; exp= access; typ= Some PyCommon.pyObject; loc} in
                let env = Env.push_instr env instr in
                Ok (env, DataStack.Temp id)
            | _ -> (
                let prefix = DataStack.as_id cell in
                let id = Option.map ~f:(fun prefix -> Ident.extend ~prefix fname) prefix in
                match id with
                | None ->
                    Error (L.InternalError, Error.LoadAttribute (cell, fname))
                | Some id ->
                    Ok (env, DataStack.Path id) )
          in
          Ok (env, cell)
    in
    Debug.p "[%s] arg = %a\n" opname (pp code) kind ;
    Ok (Env.push env cell, None)


  module BUILD_CLASS = struct
    (** {v LOAD_BUILD_CLASS v}

        Pushes [builtins.__build_class__()] onto the stack. It is later called by [CALL_FUNCTION] to
        construct a class. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let env = Env.push env DataStack.BuiltinBuildClass in
      Ok (env, None)
  end
end

let check_flags opname flags =
  let open MakeFunctionFlags in
  let has_dict_of_defaults = mem flags DictDefaultValues in
  let has_closure = mem flags Closure in
  let unsupported = has_dict_of_defaults || has_closure in
  if unsupported then Error (L.InternalError, Error.TODO (FunctionFlags (opname, flags))) else Ok ()


module FUNCTION = struct
  module CALL = struct
    let mk env fid ?(typ = PyCommon.pyObject) loc proc args =
      let env = Option.value_map ~default:env ~f:(Env.register_call env) fid in
      let call = T.Exp.call_non_virtual proc args in
      let info = Env.Info.default typ in
      let env, id = Env.mk_fresh_ident env info in
      let let_instr = T.Instr.Let {id; exp= call; loc} in
      let env = Env.push_instr env let_instr in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)


    (** {v CALL_FUNCTION(argc) v}

        Calls a callable object with positional arguments. [argc] indicates the number of positional
        arguments. The top of the stack contains positional arguments, with the right-most argument
        on top. Below the arguments is a callable object to call. This opcode pushes a fresh result
        on the top of the stack.

        Before: [ argN | ... | arg1 | arg0 | code-object | rest-of-the-stack ]

        After: [ result | rest-of-the-stack ] *)

    let static_call_with_args env fid args =
      let fid_as_id = match fid with `Path id -> id | `Name (loc, name) -> Ident.mk ~loc name in
      let mk env loc ?typ proc = mk env (Some fid_as_id) ?typ loc proc args in
      let symbol_opt =
        match fid with
        | `Path id ->
            let key = Symbol.Global id in
            Env.lookup_symbol env key
        | `Name (loc, name) ->
            (* Let's check first if the name is "local/nested" before looking in the global context *)
            let key = mk_key false loc name in
            let local_sym = Env.lookup_symbol env key in
            if Option.is_some local_sym then local_sym
            else
              let key = mk_key true loc name in
              Env.lookup_symbol env key
      in
      let loc = Env.loc env in
      match symbol_opt with
      | None ->
          (* Unknown name, can come from a `from foo import *` so we'll try to locate it in a
             further analysis *)
          let proc =
            let fid = Ident.extend_unknown_ident fid_as_id in
            Ident.to_qualified_procname fid
          in
          mk env loc proc
      | Some symbol -> (
        match (symbol : Symbol.t) with
        | {kind= Import | ImportCall; id} ->
            Error (L.InternalError, Error.TODO (StaticCallImport id))
        | {kind= Builtin; id} ->
            (* TODO: propagate builtin type information *)
            let proc = Ident.to_qualified_procname id in
            mk env loc proc
        | {kind= Name {is_imported}; id} ->
            (* TODO: propagate type information if available *)
            let proc = Ident.to_qualified_procname id in
            let key = Symbol.Global id in
            let symbol_info = {Symbol.kind= ImportCall; id; loc} in
            let env = if is_imported then Env.register_symbol env key symbol_info else env in
            mk env loc proc
        | {kind= Code; id} ->
            (* TODO: propagate type information if available *)
            let proc = Ident.to_qualified_procname id in
            mk env loc proc
        | {kind= Class; id} ->
            (* TODO: support nesting. Maybe add to_proc_name to Symbol *)
            let typ = Ident.to_typ id in
            let name = Ident.to_constructor id in
            let proc : T.QualifiedProcName.t = {enclosing_class= TopLevel; name} in
            mk env loc ~typ proc )


    let static_call env fid cells =
      let open IResult.Let_syntax in
      (* TODO: we currently can't handle hasattr correctly, so let's ignore it
               At some point, introduce a builtin to keep track of its content
      *)
      let hasattr =
        match fid with `Name (_, name) -> String.equal "hasattr" name | `Path _ -> false
      in
      if hasattr then (
        L.user_warning "no support for `hasattr` at the moment.  Skipping...@\n" ;
        let info = Env.Info.default PyCommon.pyBool in
        let loc = Env.loc env in
        let env, id = Env.mk_fresh_ident env info in
        let let_instr = T.Instr.Let {id; exp= T.Exp.Const (T.Const.Int Z.zero); loc} in
        let env = Env.push_instr env let_instr in
        let env = Env.push env (DataStack.Temp id) in
        Ok (env, None) )
      else
        let* env, args = cells_to_textual env cells in
        static_call_with_args env fid args


    let dynamic_call env caller_id args =
      let args = T.Exp.Var caller_id :: args in
      let env, id, _typ = Env.mk_builtin_call env Builtin.PythonCall args in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)


    let builtin_build_class env opname name_cell code_cell parent_cells =
      let open IResult.Let_syntax in
      let as_name kind cell =
        match DataStack.as_name cell with
        | Some name ->
            Ok name
        | None ->
            Error (L.ExternalError, Error.BuiltinClassName (opname, kind, cell))
      in
      let* code_name = as_name "base" name_cell in
      let* parents =
        let ids = List.map ~f:DataStack.as_id parent_cells in
        let ids = Option.all ids in
        Result.of_option ids ~error:(L.InternalError, Error.BuiltinClassInvalidParents code_name)
      in
      let* code =
        match DataStack.as_code code_cell with
        | Some code ->
            Ok code
        | None ->
            Error (L.ExternalError, Error.BuiltinClassBody (opname, code_cell))
      in
      let qualified_name =
        let module_name = Env.module_name env in
        Ident.extend ~prefix:module_name code_name
      in
      let env = Env.register_class env code_name qualified_name parents in
      let env = Env.push env (DataStack.Code {fun_or_class= false; code_name; code}) in
      Ok env


    let run env {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      Debug.p "[%s] argc = %d\n" opname arg ;
      let* env, cells = pop_n_datastack opname env arg in
      Debug.p "  #args = %d\n" (List.length cells) ;
      let* env, fname = pop_datastack opname env in
      Debug.p "fname= %a\n" DataStack.pp_cell fname ;
      let loc = Env.loc env in
      match (fname : DataStack.cell) with
      | VarName name | Name {name} ->
          static_call env (`Name (loc, name)) cells
      | Temp id ->
          let* env, args = cells_to_textual env cells in
          dynamic_call env id args
      | Code _
      | Import _
      | ImportFrom _
      | StaticCall _
      | MethodCall _
      | NoException
      | WithContext _
      | Const _
      | List _
      | Map _ ->
          (* TODO: should be user error, but for now we might trigger it because we don't cover
             enough constructions of the language, so it's an internal error for now *)
          Error (L.InternalError, Error.CallInvalid (opname, fname))
      | BuiltinBuildClass -> (
        match cells with
        | [code_cell; name_cell] ->
            let* env = builtin_build_class env opname name_cell code_cell [] in
            Ok (env, None)
        | code_cell :: name_cell :: parent_cells ->
            let* env = builtin_build_class env opname name_cell code_cell parent_cells in
            Ok (env, None)
        | _ ->
            Error (L.InternalError, Error.BuiltinClassInvalid) )
      | Path id ->
          static_call env (`Path id) cells
      | ImportCall {id; loc} ->
          (* TODO: test it *)
          Debug.todo "TEST IT !\n" ;
          let* env, args = cells_to_textual env cells in
          let proc = Ident.to_qualified_procname id in
          mk env (Some id) loc proc args
      | Super ->
          let env = Env.push env Super in
          Ok (env, None)
  end

  module MAKE = struct
    let unpack_annotations env annotations =
      let open IResult.Let_syntax in
      let loc = Env.loc env in
      List.fold_result ~init:[]
        ~f:(fun acc (name, c) ->
          let* name =
            match PyCommon.get_string name with
            | Some name ->
                Ok name
            | _ ->
                Error (L.ExternalError, Error.MakeFunctionInvalidAnnotations name)
          in
          match c with
          | DataStack.Name {name= annot} ->
              let annotation = Ident.mk ~loc annot in
              let typed_name = {PyCommon.name; annotation} in
              let typed_name = Option.value ~default:typed_name (lift_annotation env typed_name) in
              Ok (typed_name :: acc)
          | DataStack.Const const -> (
            match const with
            | FFI.Constant.PYCNone ->
                Ok ({PyCommon.name; annotation= Ident.mk "None"} :: acc)
            | _ ->
                Ok acc )
          | _ ->
              Error (L.InternalError, Error.TODO (MakeFunctionSignature c)) )
        annotations


    (* TODO: we don't support correctly keeping track of non constant default arugments.
       For example if the value is a global constant, we'll store
       it's loaded reference `n` which won't make sense in the specialized
       function. *)
    let unpack_defaults env defaults =
      let open IResult.Let_syntax in
      match (defaults : DataStack.cell) with
      | Const _ ->
          let* env, tuple, _typ = load_cell env defaults in
          let args = PyCommon.get_tuple_as_list tuple in
          let* args =
            Result.of_option
              ~error:(L.InternalError, Error.TODO (MakeFunctionDefault defaults))
              args
          in
          Ok (env, args)
      | List (_, args) ->
          let* env, args = cells_to_textual env args in
          Ok (env, args)
      | _ ->
          Error (L.InternalError, Error.TODO (MakeFunctionDefault defaults))


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
    let run env {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      let flags = MakeFunctionFlags.mk arg in
      Debug.p "[%s] flags = 0x%a\n" opname MakeFunctionFlags.pp flags ;
      let* () = check_flags opname flags in
      let* env, qual = pop_datastack opname env in
      (* don't care about the content of the code object, but check it is indeed code *)
      let* env, body = pop_datastack opname env in
      let* env, annotations =
        if MakeFunctionFlags.mem flags Annotations then (
          let* env, cell = pop_datastack opname env in
          Debug.p "[%s] spotted annotations\n  %a\n" opname DataStack.pp_cell cell ;
          let annotations = match cell with Env.DataStack.Map map -> Some map | _ -> None in
          Ok (env, annotations) )
        else Ok (env, None)
      in
      let* annotations =
        Option.value_map ~default:(Ok []) ~f:(unpack_annotations env) annotations
      in
      let* env, default_arguments =
        if MakeFunctionFlags.mem flags DefaultValues then
          let* env, cell = pop_datastack opname env in
          let* env, defaults = unpack_defaults env cell in
          Ok (env, defaults)
        else Ok (env, [])
      in
      let* code =
        match DataStack.as_code body with
        | None ->
            Error (L.InternalError, Error.MakeFunctionInvalidCode body)
        | Some body ->
            Ok body
      in
      let* code_name =
        match (qual : DataStack.cell) with
        | VarName _
        | Name _
        | Temp _
        | Code _
        | List _
        | Map _
        | BuiltinBuildClass
        | Import _
        | ImportFrom _
        | ImportCall _
        | MethodCall _
        | StaticCall _
        | Path _
        | NoException
        | WithContext _
        | Super ->
            Error (L.InternalError, Error.MakeFunctionInvalidName qual)
        | Const const -> (
          match FFI.Constant.as_name const with
          | Some name ->
              Ok name
          | None ->
              Error (L.InternalError, Error.MakeFunctionInvalidQname const) )
      in
      if FFI.Code.is_closure code then
        L.user_warning "%s: support for closures is incomplete (%s)@\n" opname code_name ;
      let loc = Env.loc env in
      let env = Env.register_function env code_name loc annotations default_arguments in
      let env = Env.push env (DataStack.Code {fun_or_class= true; code_name; code}) in
      Ok (env, None)
  end

  module CALL_KW = struct
    let extract_kw_names const =
      let open IResult.Let_syntax in
      let error const = (L.UserError, Error.CallKeywordNotString0 const) in
      match (const : FFI.Constant.t) with
      | PYCTuple tuple ->
          Array.fold_right tuple ~init:(Ok []) ~f:(fun const acc ->
              let* acc in
              let* name = Result.of_option (FFI.Constant.as_name const) ~error:(error const) in
              Ok (name :: acc) )
      | _ ->
          Error (error const)


    (* there is more args than names, and nameless values must come first in the end *)
    let partial_zip env argc args names =
      let nr_positional = argc - List.length names in
      let rec zip env ndx l1 l2 =
        match (l1, l2) with
        | [], _ ->
            (env, [])
        | _ :: _, [] ->
            (env, l1)
        | hd1 :: tl1, hd2 :: tl2 ->
            if ndx < nr_positional then
              let env, tl = zip env (ndx + 1) tl1 l2 in
              (env, hd1 :: tl)
            else
              let name = T.Exp.Const (Str hd2) in
              let env, id, _typ = Env.mk_builtin_call env Builtin.PythonKWArg [name; hd1] in
              let hd = T.Exp.Var id in
              let env, tl = zip env (ndx + 1) tl1 tl2 in
              (env, hd :: tl)
      in
      zip env 0 args names


    (** {v CALL_FUNCTION_KW(argc) v}

        Calls a callable object with positional (if any) and keyword arguments. [argc] indicates the
        total number of positional and keyword arguments. The top element on the stack contains a
        tuple of keyword argument names. Below that are keyword arguments in the order corresponding
        to the tuple. Below that are positional arguments, with the right-most parameter on top.
        Below the arguments is a callable object to call. [CALL_FUNCTION_KW] pops all arguments and
        the callable object off the stack, calls the callable object with those arguments, and
        pushes the return value returned by the callable object. *)

    let run env {FFI.Instruction.opname; arg= argc} =
      (* TODO:
         - make support more complete
      *)
      let open IResult.Let_syntax in
      Debug.p "[%s] argc = %d\n" opname argc ;
      let* env, arg_names = pop_datastack opname env in
      let* arg_names =
        match (arg_names : DataStack.cell) with
        | Const tuple ->
            (* kw names should be constant tuple of strings, so we directly access them *)
            extract_kw_names tuple
        | _ ->
            Error (L.UserError, Error.CallKeywordNotString1 arg_names)
      in
      let* env, cells = pop_n_datastack opname env argc in
      let* env, all_args = cells_to_textual env cells in
      let env, args = partial_zip env argc all_args arg_names in
      let* env, fname = pop_datastack opname env in
      let call env fname =
        let env, id, _typ = Env.mk_builtin_call env Builtin.PythonCallKW (fname :: args) in
        let env = Env.push env (DataStack.Temp id) in
        Ok (env, None)
      in
      let loc = Env.loc env in
      match (fname : DataStack.cell) with
      | Name {global; name} ->
          let key = mk_key global loc name in
          let fid =
            match Env.lookup_symbol env key with
            | Some {Symbol.id} ->
                id
            | None ->
                Ident.mk_unknown_ident ~loc name
          in
          let fname = Ident.to_string ~sep:"." fid in
          let fname = T.Exp.Const (Str fname) in
          call env fname
      | Temp id ->
          call env (T.Exp.Var id)
      | BuiltinBuildClass ->
          Error (L.ExternalError, Error.CallKeywordBuildClass)
      | Path id ->
          let key = Symbol.Global id in
          let fname =
            match Env.lookup_symbol env key with
            | Some {Symbol.id} ->
                Ident.to_string ~sep:"." id
            | None -> (
                (* A special encoding of the compiler would use
                     LOAD_NAME                0 (foo)
                     LOAD_METHOD              1 (bar)
                     LOAD_CONST               0 (1)
                     CALL_METHOD              0

                   to encode `foo.bar()`

                   but with named arguments, we get
                     LOAD_NAME                0 (foo)
                     LOAD_ATTR                1 (bar)
                     LOAD_CONST               0 (1)
                     LOAD_CONST               1 (('x',))
                     CALL_FUNCTION_KW         1

                   to encode `foo.bar(x=1)`

                   So we have to reverse engineer a few things here *)
                let last, prefix = Ident.pop id in
                let key =
                  match prefix with
                  | None ->
                      (* TODO: check this global *)
                      mk_key true loc last
                  | Some id ->
                      Symbol.Global id
                in
                match Env.lookup_symbol env key with
                | Some {Symbol.id} ->
                    let fname = Ident.to_string ~sep:"::" id in
                    sprintf "%s.%s" fname last
                | None ->
                    let id = Ident.extend_unknown_ident id in
                    let fname = Ident.to_string ~sep:"::" id in
                    sprintf "%s.%s" fname last )
          in
          let fname = T.Exp.Const (Str fname) in
          call env fname
      | ImportCall {id} ->
          (* TODO: test it *)
          Debug.todo "TEST IT 2!\n" ;
          let fname = Ident.to_string ~sep:"." id in
          let fname = T.Exp.Const (Str fname) in
          call env fname
      | Code _
      | VarName _
      | Const _
      | List _
      | Map _
      | Import _
      | ImportFrom _
      | StaticCall _
      | MethodCall _
      | NoException
      | WithContext _ ->
          Error (L.InternalError, Error.TODO (CallKwInvalidFunction fname))
      | Super ->
          let env = Env.push env Super in
          Ok (env, None)
  end
end

module METHOD = struct
  module LOAD = struct
    let load env cell method_name =
      let open IResult.Let_syntax in
      let* env, receiver, _ = load_cell env cell in
      let loc = Env.loc env in
      let name = proc_name ~loc method_name in
      let name : T.QualifiedProcName.t = {enclosing_class= Enclosing T.TypeName.wildcard; name} in
      let env = Env.push env (DataStack.MethodCall {receiver; name}) in
      Ok (env, None)


    (* Here we translate calls like [super().f(...)] into a static call [parent_type.f(self, ...)]
       so that method resolution can do its job. This only works because we
       only support single inheritance for now. We need to revisit this.

       Python does a virtual call here because of how the runtime deals with inheritance: it's
       just a pointer to the parent classes. We don't encode this like that for now.

       TODO: pass in the current function name for better error reporting, but not urgent as all
       these errors would make python runtime crash anyway *)
    let load_super env method_name =
      let open IResult.Let_syntax in
      (* TODO: rethink the class API ? *)
      let current_module = Env.module_name env in
      (* current_module is a fully qualified name like foo#C, but we want to extract that C part
         only to lookup in our set of known classes. *)
      let _func_name, class_path = Ident.pop current_module in
      (* super() must be called into a class method so if the compiler accepts the code, we have
         at least function_name.class_name in the current_module *)
      let class_path = Option.value_exn class_path in
      let class_name, module_path = Ident.pop class_path in
      let module_path = Option.value_exn module_path in
      let classes = Env.get_declared_classes env in
      let class_info = SMap.find_opt class_name classes in
      let params = Env.get_params env in
      let is_static = Env.is_static env in
      let* parent =
        match class_info with
        | None ->
            Error (L.ExternalError, Error.SuperNotInClass module_path)
        | Some {parents} -> (
          match parents with
          | [] ->
              Error (L.ExternalError, Error.SuperNoParent module_path)
          | [parent] ->
              Ok parent
          | _ ->
              Error (L.InternalError, Error.TODO (SuperMultipleInheritance module_path)) )
      in
      let parent_name =
        let key = Symbol.Global parent in
        let id = Env.lookup_symbol env key in
        (* Parent is in a different file, we can only trust it's there *)
        Option.value_map ~f:(fun {Symbol.id} -> id) ~default:parent id
      in
      let* self =
        if is_static then Error (L.ExternalError, Error.SuperInStatic module_path)
        else
          match List.hd params with
          | None ->
              Error (L.ExternalError, Error.SuperNoParameters module_path)
          | Some self ->
              Ok self
      in
      let loc = Env.loc env in
      let super_type = Ident.to_type_name parent_name in
      match load_var_name env loc self with
      | Ok (env, receiver, _) ->
          (* We're in [LOAD_METHOD] but we fake a static call because of how method resolution works. *)
          let loc = Env.loc env in
          let name = proc_name ~loc method_name in
          let call_name : T.QualifiedProcName.t = {enclosing_class= Enclosing super_type; name} in
          let env = Env.push env (DataStack.StaticCall {call_name; receiver= Some receiver}) in
          Ok (env, None)
      | Error (_, err) ->
          Error (L.UserError, Error.SuperLoadSelf (self, module_path, err))


    (** {v LOAD_METHOD(namei) v}

        Loads a method named [co_names[namei]] from the top-of-stack object. top-of-stack is popped.
        This bytecode distinguishes two cases: if top-of-stack has a method with the correct name,
        the bytecode pushes the unbound method and top-of-stack. top-of-stack will be used as the
        first argument ([self]) by [CALL_METHOD] when calling the unbound method. Otherwise, [NULL]
        and the object return by the attribute lookup are pushed. *)
    let run env {FFI.Code.co_names} {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      let method_name = co_names.(arg) in
      Debug.p "[%s] namei = %s\n" opname method_name ;
      let* env, cell = pop_datastack opname env in
      match (cell : DataStack.cell) with
      | Name {global; name} -> (
          let loc = Env.loc env in
          let key = mk_key global loc name in
          let opt_sym = Env.lookup_symbol env key in
          match (opt_sym : Symbol.t option) with
          | Some {kind= Import; id} ->
              let id = Ident.extend ~prefix:id method_name in
              let env = Env.push env (DataStack.ImportCall {loc; id}) in
              Ok (env, None)
          | Some {kind= Class; id} ->
              let enclosing_class = Ident.to_type_name ~static:true id in
              let call_name = qualified_procname ~enclosing_class @@ proc_name ~loc method_name in
              let env = Env.push env (DataStack.StaticCall {call_name; receiver= None}) in
              Ok (env, None)
          | _ ->
              load env cell method_name )
      | Super ->
          load_super env method_name
      | _ ->
          load env cell method_name
  end

  module CALL = struct
    (** {v CALL_METHOD(argc) v}

        Calls a method. [argc] is the number of positional arguments. Keyword arguments are not
        supported. This opcode is designed to be used with [LOAD_METHOD]. Positional arguments are
        on top of the stack. Below them, the two items described in [LOAD_METHOD] are on the stack
        (either [self] and an unbound method object or [NULL] and an arbitrary callable). All of
        them are popped and the return value is pushed. *)
    let run env {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      Debug.p "[%s] argc = %d\n" opname arg ;
      let* env, cells = pop_n_datastack opname env arg in
      let* env, args = cells_to_textual env cells in
      let* env, cell_mi = pop_datastack opname env in
      match (cell_mi : DataStack.cell) with
      | ImportCall {id; loc} ->
          let key = Symbol.Global id in
          let symbol_info = {Symbol.kind= ImportCall; id; loc} in
          let env = Env.register_symbol env key symbol_info in
          let proc = Ident.to_qualified_procname id in
          FUNCTION.CALL.mk env None loc proc args
      | StaticCall {call_name; receiver} ->
          let loc = Env.loc env in
          let args = Option.to_list receiver @ args in
          FUNCTION.CALL.mk env None loc call_name args
      | MethodCall {receiver; name} ->
          let loc = Env.loc env in
          let exp = T.Exp.call_virtual name receiver args in
          (* TODO: read return type of proc if available *)
          let info = Env.Info.default PyCommon.pyObject in
          let env, id = Env.mk_fresh_ident env info in
          let let_instr = T.Instr.Let {id; exp; loc} in
          let env = Env.push_instr env let_instr in
          let env = Env.push env (DataStack.Temp id) in
          Ok (env, None)
      | Path _
      | Const _
      | Name _
      | VarName _
      | Temp _
      | Code _
      | Map _
      | List _
      | BuiltinBuildClass
      | Import _
      | ImportFrom _
      | NoException
      | WithContext _
      | Super ->
          Error (L.InternalError, Error.CallInvalid (opname, cell_mi))
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

            Stores top-of-stack into the local [co_varnames[var_num]]. *)
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

  let store env {FFI.Code.co_names} opname loc arg cell name global is_attr =
    let open IResult.Let_syntax in
    Debug.p "[store] cell= %a name= %s global= %b is_attr= %b\n" DataStack.pp_cell cell name global
      is_attr ;
    let* env, exp, info = load_cell env cell in
    let {Env.Info.typ; kind} = info in
    let module_name = Env.module_name env in
    let prefix =
      if global then
        if List.mem ~equal:String.equal python_implicit_names name then
          Ident.mk python_implicit_names_prefix
        else Ident.root module_name
      else module_name
    in
    let id = Ident.extend ~prefix name in
    let symbol_info =
      match (kind : Env.Info.kind) with
      | Code ->
          {Symbol.kind= Code; loc; id}
      | Class ->
          {Symbol.kind= Class; loc; id}
      | Other ->
          let id = if global then id else Ident.mk ~global:false ~loc name in
          {Symbol.kind= Name {is_imported= false; typ}; loc; id}
    in
    let var_name = Ident.to_var_name symbol_info.Symbol.id in
    if is_attr then
      let* env, cell = pop_datastack opname env in
      let* env, value, {Env.Info.typ} = load_cell env cell in
      let fname = co_names.(arg) in
      (* TODO: refine typ if possible *)
      let loc = Env.loc env in
      let name = field_name ~loc fname in
      let field = {T.enclosing_class= T.TypeName.wildcard; name} in
      let exp1 = T.Exp.Field {exp; field} in
      let instr = T.Instr.Store {exp1; typ= Some typ; exp2= value; loc} in
      let env = Env.push_instr env instr in
      Ok (env, None)
    else
      let key = mk_key global loc name in
      let env = Env.register_symbol env key symbol_info in
      match (kind : Env.Info.kind) with
      | Class ->
          Debug.p "  top-level class declaration initialized\n" ;
          Ok (env, None)
      | Code ->
          if global then (
            Debug.p "  top-level function defined\n" ;
            Ok (env, None) )
          else Error (L.InternalError, Error.TODO (Closure opname))
      | Other ->
          let instr = T.Instr.Store {exp1= Lvar var_name; typ= Some typ; exp2= exp; loc} in
          Ok (Env.push_instr env instr, None)


  let run kind env ({FFI.Code.co_names; co_varnames} as code) {FFI.Instruction.opname; arg} =
    let open IResult.Let_syntax in
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
    let* env, cell = pop_datastack opname env in
    match (cell : DataStack.cell) with
    | Import {import_path} ->
        let env =
          let symbol_info = {Symbol.kind= Import; loc; id= import_path} in
          let key = mk_key global loc name in
          Env.register_symbol env key symbol_info
        in
        Ok (env, None)
    | ImportFrom {import_path; imported_name} ->
        let env =
          let id = Ident.extend ~prefix:import_path imported_name in
          let typ = PyCommon.pyObject in
          let symbol_info = {Symbol.kind= Name {is_imported= true; typ}; loc; id} in
          let key = mk_key global loc name in
          Env.register_symbol env key symbol_info
        in
        Ok (env, None)
    | _ ->
        store env code opname loc arg cell name global is_attr


  module SUBSCR = struct
    (** {v STORE_SUBSCR v}

        Implements [top-of-stack1[top-of-stack] = top-of-stack2] *)
    let run env {FFI.Instruction.opname} =
      let open IResult.Let_syntax in
      Debug.p "[%s]\n" opname ;
      let* env, cell_ndx = pop_datastack opname env in
      let* env, cell_lhs = pop_datastack opname env in
      let* env, cell_rhs = pop_datastack opname env in
      let is_annotations =
        match DataStack.as_name cell_lhs with
        | Some name ->
            String.equal name "__annotations__"
        | None ->
            false
      in
      if Env.has_annotations env && is_annotations then
        (* We do not store user annotations at the moment *)
        Ok (env, None)
      else
        let* env, ndx, _ = load_cell env cell_ndx in
        let* env, lhs, _ = load_cell env cell_lhs in
        let* env, rhs, _ = load_cell env cell_rhs in
        let env, _id, _typ = Env.mk_builtin_call env Builtin.PythonSubscriptSet [lhs; ndx; rhs] in
        Ok (env, None)
  end
end

module POP_TOP = struct
  (** {v POP_TOP v}

      Pop the top-of-stack and discard it *)
  let run env {FFI.Instruction.opname} =
    let open IResult.Let_syntax in
    Debug.p "[%s]\n" opname ;
    let* env, cell = pop_datastack opname env in
    (* Maybe we're popping a delayed access using a [Path] so let's load it in case it had side-effects *)
    let* env =
      if DataStack.is_path cell then
        (* TODO: maybe better error here ? *)
        let* env, _, _ = load_cell env cell in
        Ok env
      else Ok env
    in
    Ok (env, None)
end

module POP_BLOCK = struct
  (** {v POP_BLOCK v}

      Removes one block from the block stack. Per frame, there is a stack of blocks, denoting try
      statements, and such.

      Note: we don't mode blocks (yet?) so this is a NOP *)
  let run env {FFI.Instruction.opname} =
    Debug.p "[%s]\n" opname ;
    Ok (env, None)
end

module BINARY = struct
  (** Binary operations remove the top of the stack and the second top-most stack item from the
      stack. They perform the operation, and put the result back on the stack.

      In-place operations are like binary operations, in that they remove top-of-stack and
      top-of-stack1, and push the result back on the stack, but the operation is done in-place when
      top-of-stack1 supports it, and the resulting top-of-stack may be (but does not have to be) the
      original top-of-stack1. *)
  let run env {FFI.Instruction.opname} builtin =
    let open IResult.Let_syntax in
    Debug.p "[%s]\n" opname ;
    let* env, tos = pop_datastack opname env in
    let* env, tos1 = pop_datastack opname env in
    let* env, lhs, _ = load_cell env tos1 in
    let* env, rhs, _ = load_cell env tos in
    (* Even if the call can be considered as virtual because, it's logic is not symetric. Based
       on what I gathered, like in [0], I think the best course of action is to write a model for
       it and leave it non virtual. TODO: ask David.

       [0]:
       https://stackoverflow.com/questions/58828522/is-radd-called-if-add-raises-notimplementederror
    *)
    let env, id, _typ = Env.mk_builtin_call env builtin [lhs; rhs] in
    let env = Env.push env (DataStack.Temp id) in
    Ok (env, None)


  module SUBSCR = struct
    (** {v BINARY_SUBSCR v}

        Implements [top-of-stack = top-of-stack1[top-of-stack]]. *)
    let run env {FFI.Instruction.opname} =
      let open IResult.Let_syntax in
      Debug.p "[%s]\n" opname ;
      let* env, cell_ndx = pop_datastack opname env in
      let* env, cell_lhs = pop_datastack opname env in
      let* env, ndx, _ = load_cell env cell_ndx in
      let* env, lhs, _ = load_cell env cell_lhs in
      let env, id, _typ = Env.mk_builtin_call env Builtin.PythonSubscriptGet [lhs; ndx] in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)
  end
end

module UNARY = struct
  (** Unary operations take the top of the stack, apply the operation, and push the result back on
      the stack. *)

  let run env {FFI.Instruction.opname} builtin =
    let open IResult.Let_syntax in
    Debug.p "[%s]\n" opname ;
    let* env, tos = pop_datastack opname env in
    let* env, tos, _ = load_cell env tos in
    let env, id, _typ = Env.mk_builtin_call env builtin [tos] in
    let env = Env.push env (DataStack.Temp id) in
    Ok (env, None)
end

module BUILD = struct
  module CONST_KEY_MAP = struct
    let is_tuple_ids cell =
      let as_key c =
        match (c : FFI.Constant.t) with
        | PYCString s ->
            Some (PyCommon.mk_string s)
        | PYCInt z ->
            Some (PyCommon.mk_int z)
        | PYCBytes s ->
            let s = Bytes.to_string s in
            Some (PyCommon.mk_string s)
        | _ ->
            None
      in
      match cell with
      | DataStack.Const c -> (
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
    let run env {FFI.Instruction.opname; arg= count} =
      let open IResult.Let_syntax in
      Debug.p "[%s] count = %d\n" opname count ;
      let* env, cell = pop_datastack opname env in
      let* keys =
        match is_tuple_ids cell with
        | Ok keys ->
            Ok (List.rev keys)
        | Error () ->
            Error (L.UserError, Error.LiteralTuple opname)
      in
      (* TODO check cells is a tuple of literal strings *)
      let* env, values = pop_n_datastack opname env count in
      Debug.p "  #values = %d\n" (List.length values) ;
      let* map =
        match List.zip keys values with
        | List.Or_unequal_lengths.Ok map ->
            Ok map
        | Base.List.Or_unequal_lengths.Unequal_lengths ->
            Error (L.UserError, Error.BuildConstKeyMap (List.length keys, List.length values))
      in
      let env = Env.push env (DataStack.Map map) in
      Ok (env, None)
  end

  module MAP = struct
    let build_map env cells =
      let open IResult.Let_syntax in
      let rec aux env = function
        | [] ->
            Ok []
        | key :: value :: rest ->
            let* env, key, _typ = load_cell env key in
            let* tl = aux env rest in
            Ok ((key, value) :: tl)
        | _ ->
            (* We popped 2 * count number of cells, this should be unreachable *)
            L.die InternalError "BUILD_MAP wrong number of cells"
      in
      aux env cells


    (** {v BUILD_MAP(count) v}

        Pushes a new dictionary object onto the stack. Pops [2 * count] items so that the dictionary
        holds count entries: [{..., TOS3: TOS2, TOS1: TOS}] *)
    let run env {FFI.Instruction.opname; arg= count} =
      let open IResult.Let_syntax in
      Debug.p "[%s] count = %d\n" opname count ;
      let* env, items = pop_n_datastack opname env (2 * count) in
      let* map = build_map env items in
      let env = Env.push env (DataStack.Map map) in
      Ok (env, None)
  end

  module SLICE = struct
    (** {v BUILD_SLICE(argc) v}

        Pushes a slice object on the stack. [argc] must be 2 or 3. If it is 2,
        [slice(top-of-stack1, top-of-stack)] is pushed; if it is 3,
        [slice(top-of-stack2, top-of-stack1, top-of-stack)] is pushed.

        See the [slice()] built-in function for more information.

        https://docs.python.org/3.8/library/functions.html#slice *)
    let run env {FFI.Instruction.opname; arg= argc} =
      let open IResult.Let_syntax in
      Debug.p "[%s] argc = %d\n" opname argc ;
      if argc <> 2 && argc <> 3 then Error (L.ExternalError, Error.BuildSliceArity argc)
      else
        let* env, cells = pop_n_datastack opname env argc in
        let* env, exps = cells_to_textual env cells in
        let env, id, _ = Env.mk_builtin_call env (Builtin.PythonBuild Slice) exps in
        let env = Env.push env (DataStack.Temp id) in
        Ok (env, None)
  end

  (** {v BUILD_LIST(count) v}
      {v BUILD_SET(count) v}
      {v BUILD_TUPLE(count) v}
      {v BUILD_STRING(count) v}

      Creates a collection/string consuming [count] items from the stack, and pushes the resulting
      collection/string onto the stack.

      For strings, it concatenate the inputs into the output. *)
  let run env {FFI.Instruction.opname; arg= count} collection =
    let open IResult.Let_syntax in
    Debug.p "[%s] count = %d\n" opname count ;
    let* env, items = pop_n_datastack opname env count in
    let env = Env.push env (DataStack.List (collection, items)) in
    Ok (env, None)
end

(** Return the offset of the next opcode, if any *)
let offset_of_code instructions =
  match instructions with FFI.Instruction.{offset} :: _ -> Some offset | _ -> None


let next_offset opt =
  match opt with None -> Error (L.InternalError, Error.RelativeJumpEOF) | Some offset -> Ok offset


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
let stack_to_ssa env =
  let open IResult.Let_syntax in
  let* env, zipped =
    Env.map_result ~env (Env.stack env) ~f:(fun env cell ->
        let* env, exp, {Env.Info.typ} = load_cell env cell in
        Ok (env, (exp, typ)) )
  in
  Ok (Env.reset_stack env, List.unzip zipped)


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
    | Throw of T.Exp.t

  module POP_IF = struct
    (** {v POP_JUMP_IF_TRUE(target) v}

        If top-of-stack is true, sets the bytecode counter to target. top-of-stack is popped.

        {v POP_JUMP_IF_FALSE(target) v}

        If top-of-stack is false, sets the bytecode counter to target. top-of-steack is popped. *)
    let run ~next_is_true env {FFI.Instruction.opname; arg} next_offset_opt =
      let open IResult.Let_syntax in
      Debug.p "[%s] target = %d\n" opname arg ;
      let* env, tos = pop_datastack opname env in
      let* env, cond, _ = load_cell env tos in
      let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
      let* next_offset = next_offset next_offset_opt in
      let env, next_info = mk_label_info env next_offset ~ssa_parameters in
      let env, other_info = mk_label_info env arg ~ssa_parameters in
      (* Compute the relevant pruning expressions *)
      let env, id, _ = Env.mk_builtin_call env Builtin.IsTrue [cond] in
      let condT = T.BoolExp.Exp (Var id) in
      let condition = if next_is_true then condT else T.BoolExp.Not condT in
      Ok
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
    let run ~jump_if env {FFI.Instruction.opname; arg} next_offset_opt =
      let open IResult.Let_syntax in
      Debug.p "[%s] target = %d\n" opname arg ;
      (* We only peek the top of stack so the logic of [stack_to_ssa] correctly captures it for
         the branch where "it stays". It will be restored as part of the stack restore logic when
         we process the "other" node. *)
      let* tos = peek_datastack opname env in
      let* env, cond, _ = load_cell env tos in
      (* Suggestion from @dpichardie: I could put the [IsTrue] expression directly in [condition].
         This would need a refactor of [mk_builtin_call] to get the full expression instead of
         the id that binds it. *)
      let env, id, _ = Env.mk_builtin_call env Builtin.IsTrue [cond] in
      let condT = T.BoolExp.Exp (Var id) in
      let condition = if jump_if then T.BoolExp.Not condT else condT in
      let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
      let* next_offset = next_offset next_offset_opt in
      (* In the next branch, we make sure the top-of-stack is no longer in the ssa parameters
         so it is not restored, effectively dropped as per the opcode description. *)
      let env, next_info =
        let ssa_parameters = List.tl ssa_parameters |> Option.value ~default:[] in
        mk_label_info env next_offset ~ssa_parameters
      in
      let next_ssa_args = List.tl ssa_args |> Option.value ~default:[] in
      let env, other_info = mk_label_info env arg ~ssa_parameters in
      Ok
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
    let run env {FFI.Instruction.opname; arg= delta} next_offset_opt =
      let open IResult.Let_syntax in
      Debug.p "[%s] delta = %d\n" opname delta ;
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = next_offset next_offset_opt in
      let offset = next_offset + delta in
      let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
      let env, label_info = mk_label_info env offset ~ssa_parameters in
      Ok (env, Some (Absolute {ssa_args; label_info; offset}))
  end

  module ABSOLUTE = struct
    (** {v JUMP_ABSOLUTE(target) v}

        Set bytecode counter to [target]. Can target a previous offset. *)

    let run env {FFI.Instruction.opname; arg= target; offset} =
      let open IResult.Let_syntax in
      Debug.p "[%s] target = %d\n" opname target ;
      let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
      (* sanity check: we should already have allocated a label for this jump, if it is a backward
         edge. *)
      let* () =
        if target < offset then
          let opt_info = Env.label_of_offset env target in
          if Option.is_none opt_info then Error (L.UserError, Error.MissingBackEdge (offset, target))
          else Ok ()
        else Ok ()
      in
      let env, label_info = mk_label_info env target ~ssa_parameters in
      Ok (env, Some (Absolute {ssa_args; label_info; offset= target}))
  end
end

module RETURN_VALUE = struct
  (** {v RETURN_VALUE v}

      Returns the top-of-stack *)
  let run env {FFI.Instruction.opname} =
    let open IResult.Let_syntax in
    Debug.p "[%s]\n" opname ;
    let* env, cell = pop_datastack opname env in
    let* env, exp, _ = load_cell env cell in
    let term = T.Terminator.Ret exp in
    Ok (env, Some (JUMP.Return term))
end

module ITER = struct
  module GET = struct
    (** {v GET_ITER v}

        Implements top-of-stack = iter(top-of-stack). Most of the type calls the [__iter__()] method
        on the top of the stack. Might be C code for builtin types, so we'll model it as a builtin *)
    let run env {FFI.Instruction.opname} =
      let open IResult.Let_syntax in
      Debug.p "[%s]\n" opname ;
      let* env, cell = pop_datastack opname env in
      let* env, exp, _ = load_cell env cell in
      let env, id, _ = Env.mk_builtin_call env Builtin.PythonIter [exp] in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)
  end

  module FOR = struct
    (** {v FOR_ITER(delta) v}

        top-of-stack is an iterator. Call its [__next__()] method. If this yields a new value, push
        it on the stack (leaving the iterator below it). If the iterator indicates it is exhausted
        top-of-stack is popped, and the byte code counter is incremented by delta. *)
    let run env {FFI.Instruction.opname; arg= delta} next_offset_opt =
      let open IResult.Let_syntax in
      Debug.p "[%s] delta = %d\n" opname delta ;
      let* env, iter_cell = pop_datastack opname env in
      let* env, iter, _ = load_cell env iter_cell in
      let loc = Env.loc env in
      let env, id, _ = Env.mk_builtin_call env Builtin.PythonIterNext [iter] in
      let has_item = T.Exp.Field {exp= T.Exp.Var id; field= PyCommon.py_iter_item_has_item} in
      let has_item_info = Env.Info.default T.Typ.Int in
      let env, has_item_id = Env.mk_fresh_ident env has_item_info in
      let has_item_load = T.Instr.Load {id= has_item_id; exp= has_item; typ= Some T.Typ.Int; loc} in
      let env = Env.push_instr env has_item_load in
      let cond = T.Exp.Var has_item_id in
      let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
      (* Compute the relevant pruning expressions *)
      let condition = T.BoolExp.Exp cond in
      (* In the next branch, we know the iterator has an item available. Let's fetch it and
         push it on the stack. *)
      let next_prelude loc env =
        (* The iterator object stays on the stack while in the for loop, let's push it back *)
        let env = Env.push env iter_cell in
        let next_item = T.Exp.Field {exp= T.Exp.Var id; field= PyCommon.py_iter_item_next_item} in
        (* TODO: try to track iterator types *)
        let next_item_info = Env.Info.default PyCommon.pyObject in
        let env, next_item_id = Env.mk_fresh_ident env next_item_info in
        let next_item_load =
          T.Instr.Load {id= next_item_id; exp= next_item; typ= Some PyCommon.pyObject; loc}
        in
        let env = Env.push_instr env next_item_load in
        Env.push env (DataStack.Temp next_item_id)
      in
      let* next_offset = next_offset next_offset_opt in
      let env, next_info = mk_label_info env next_offset ~prelude:next_prelude ~ssa_parameters in
      let other_offset = delta + next_offset in
      let env, other_info = mk_label_info env other_offset ~ssa_parameters in
      Ok
        ( env
        , Some
            (JUMP.TwoWay
               { condition
               ; next_info= {label_info= next_info; offset= next_offset; ssa_args}
               ; other_info= {label_info= other_info; offset= other_offset; ssa_args} } ) )
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


  let load_int cell =
    match (cell : DataStack.cell) with Const const -> py_to_int const | _ -> None


  let load_strings cell =
    match (cell : DataStack.cell) with Const const -> py_to_strings const | _ -> None


  module NAME = struct
    (** {v IMPORT_NAME(namei) v}

        Imports the module [co_names[namei]]. The two top elements from the the stack are popped and
        provide the [fromlist] and [level] arguments of [__import__()]. The module object is pushed
        onto the stack. The current namespace is not affected: for a proper import statement, a
        subsequent STORE_FAST instruction modifies the namespace.

        Also the doc says: [level] specifies whether to use absolute or relative imports. 0 (the
        default) means only perform absolute imports. Positive values for [level] indicate the
        number of parent directories to search relative to the directory of the module calling
        [__import__()] *)
    let run env {FFI.Code.co_names} {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      Debug.p "[%s] namei = %d\n" opname arg ;
      let* env, from_list = pop_datastack opname env in
      let* env, level = pop_datastack opname env in
      let from_list = load_strings from_list in
      let level = load_int level in
      let* level =
        match level with
        | None ->
            Error (L.UserError, Error.ImportInvalidLevel)
        | Some level -> (
          try Ok (Z.to_int level)
          with Z.Overflow -> Error (L.ExternalError, Error.ImportLevelTooBig level) )
      in
      let loc = Env.loc env in
      let module_name = Env.module_name env in
      let name = co_names.(arg) in
      let* import_path =
        match level with
        | 0 ->
            (* Absolute path *)
            Result.of_option
              ~error:(L.InternalError, Error.ImportInvalidName name)
              (Ident.from_string ~loc name)
        | _ -> (
            (* Relative path *)
            let rec pop_levels n id =
              match (n, id) with
              | 0, None ->
                  Some `Root
              | 0, Some id ->
                  Some (`Path id)
              | _, None ->
                  None
              | _, Some id ->
                  let _, next_id = Ident.pop id in
                  pop_levels (n - 1) next_id
            in
            let prefix = pop_levels level (Some module_name) in
            match prefix with
            | Some `Root ->
                Ok (Ident.mk ~loc name)
            | Some (`Path prefix) ->
                if String.is_empty name then Ok prefix else Ok (Ident.extend ~prefix name)
            | None ->
                Error (L.ExternalError, Error.ImportInvalidDepth (module_name, level)) )
      in
      let key = Symbol.Global import_path in
      let* env =
        if Option.is_some (Env.lookup_symbol env key) then Ok env
        else
          let symbol_info = {Symbol.kind= Import; id= import_path; loc} in
          let env = Env.register_symbol env key symbol_info in
          let enclosing_class = Ident.to_type_name import_path in
          let proc =
            qualified_procname ~enclosing_class @@ proc_name ~loc PyCommon.toplevel_function
          in
          let* env, _is_always_none = FUNCTION.CALL.mk env None loc proc [] in
          (* Python toplevel code can't use [return] but still we just put a useless Textual id
             on the stack. Popping it *)
          let* env, _ = pop_datastack "STORE_IMPORTED_NAME" env in
          Ok env
      in
      let from_list =
        (* TODO: turn into `Error` ? *)
        if Option.is_none from_list then
          L.debug Capture Quiet "[IMPORT_NAME] failed to process `from_list`@\n" ;
        Option.value ~default:[] from_list
      in
      let env = Env.push env (DataStack.Import {import_path; from_list}) in
      Ok (env, None)
  end

  module FROM = struct
    (** {v IMPORT_FROM(namei) v}

        Loads the attribute [co_names[namei]] from the module found in the top of the stack. The
        resulting object is pushed onto the stack, to be subsequently stored by a [STORE_FAST]
        instruction. *)
    let run env {FFI.Code.co_names} {FFI.Instruction.opname; arg} =
      let open IResult.Let_syntax in
      Debug.p "[%s] namei = %d\n" opname arg ;
      let symbol_name = co_names.(arg) in
      let* import_cell = peek_datastack opname env in
      match (import_cell : DataStack.cell) with
      | Import {import_path; from_list} ->
          let env =
            if List.mem ~equal:String.equal from_list symbol_name then
              Env.push env (DataStack.ImportFrom {import_path; imported_name= symbol_name})
            else (
              (* TODO: update the message and turn it into a user_warning ? *)
              L.debug Capture Quiet "[IMPORT_FROM] symbol %s is not part of the expect list [%s]@\n"
                symbol_name
                (String.concat ~sep:", " from_list) ;
              env )
          in
          Ok (env, None)
      | _ ->
          Error (L.UserError, Error.ImportInvalidRoot import_cell)
  end
end

module COMPARE_OP = struct
  (** {v COMPARE_OP(opname) v}

      Performs a Boolean operation. The operation name can be found in [cmp_op[opname]].

      [ dis.cmp_op = ('<', '<=', '==', '!=', '>', '>=', 'in', 'not in', 'is', 'is not', 'exception match', 'BAD') ] *)
  let run env {FFI.Instruction.opname; arg} =
    let open IResult.Let_syntax in
    let* cmp_op =
      match List.nth Builtin.Compare.all arg with
      | Some op ->
          Ok op
      | None ->
          Error (L.ExternalError, Error.CompareOp arg)
    in
    Debug.p "[%s] cmp_op = %a\n" opname Builtin.Compare.pp cmp_op ;
    let* env =
      match (cmp_op : Builtin.Compare.t) with
      | Exception | BAD ->
          Error (L.InternalError, Error.TODO (CompareOp cmp_op))
      | Lt | Le | Gt | Ge | Neq | Eq | In | Is | IsNot | NotIn ->
          let* env, rhs = pop_datastack opname env in
          let* env, lhs = pop_datastack opname env in
          let* env, lhs, _ = load_cell env lhs in
          let* env, rhs, _ = load_cell env rhs in
          let env, id, _typ = Env.mk_builtin_call env (Builtin.CompareOp cmp_op) [lhs; rhs] in
          let env = Env.push env (DataStack.Temp id) in
          Ok env
    in
    Ok (env, None)
end

module FINALLY = struct
  module BEGIN = struct
    (** {v BEGIN_FINALLY v}

        Pushes [NoException] onto the stack for using it in [END_FINALLY], [POP_FINALLY],
        [WITH_CLEANUP_START] and [WITH_CLEANUP_FINISH]. Starts the finally block.

        Note: we only provide initial support to be used with [SETUP_WITH] *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let env = Env.push env DataStack.NoException in
      Ok (env, None)
  end

  module END = struct
    (** {v END_FINALLY v}

        Terminates a finally clause. The interpreter recalls whether the exception has to be
        re-raised or execution has to be continued depending on the value on top of the stack.

        If top-of-stack is [NoException] (pushed by [BEGIN_FINALLY]) continue from the next
        instruction. top-of-stack is popped.

        If top-of-stack is an integer (pushed by [CALL_FINALLY]), sets the bytecode counter to
        top-of-stack. top-of-stack is popped.

        If top-of-stack is an exception type (pushed when an exception has been raised) 6 values are
        popped from the stack, the first three popped values are used to re-raise the exception and
        the last three popped values are used to restore the exception state. An exception handler
        block is removed from the block stack.

        Note: we only support [NoException] for the time being. *)
    let run env {FFI.Instruction.opname} =
      let open IResult.Let_syntax in
      Debug.p "[%s]\n" opname ;
      let* env, tos = pop_datastack opname env in
      let* env =
        if DataStack.is_no_exception tos then Ok env
        else Error (L.InternalError, Error.TODO (Exception (opname, tos)))
      in
      Ok (env, None)
  end

  module SETUP = struct
    (** {v SETUP_FINALLY(delta) v}

        Pushes a try block from a try-finally or try-except clause onto the block stack. [delta]
        points to the finally block or the first except block.

        Note: we don't support exception yet, so this doesn't do much at the moment *)
    let run env {FFI.Instruction.opname; arg= delta} next_offset_opt =
      Debug.p "[%s] delta= %d\n" opname delta ;
      let open IResult.Let_syntax in
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = next_offset next_offset_opt in
      let offset = next_offset + delta in
      let env = Env.register_with_target ~offset env in
      Ok (env, None)
  end
end

module WITH = struct
  module CLEANUP_START = struct
    (** {v WITH_CLEANUP_START v}

        Starts cleaning up the stack when a with statement block exits. At the top of the stack are
        either [NoException] (pushed by [BEGIN_FINALLY]) or 6 values pushed if an exception has been
        raised in the with block. Below is the context manager’s [__exit__()] or [__aexit__()] bound
        method.

        If top-of-stack is [NoException], calls [SECOND(None, None, None)], removes the function
        from the stack, leaving top-of-stack, and pushes [NoException] to the stack. Otherwise calls
        [SEVENTH(TOP, SECOND, THIRD)], shifts the bottom 3 values of the stack down, replaces the
        empty spot with [NoException] and pushes top-of-stack. Finally pushes the result of the
        call.

        Note: we only support the first case with [NoException] for the moment we only support
        [__exit__]. TODO: learn how [__aexit__] works *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos = pop_datastack opname env in
      let* () =
        if DataStack.is_no_exception tos then Ok ()
        else Error (L.InternalError, Error.TODO (Exception (opname, tos)))
      in
      let* env, context_manager = pop_datastack opname env in
      let* exp =
        match context_manager with
        | DataStack.WithContext exp ->
            Ok exp
        | _ ->
            Error (L.ExternalError, Error.WithCleanup context_manager)
      in
      let proc =
        let enclosing_class = T.TypeName.wildcard in
        qualified_procname ~enclosing_class @@ proc_name PyCommon.exit
      in
      let none = T.Exp.Const T.Const.Null in
      let exit_exp = T.Exp.call_virtual proc (T.Exp.Var exp) [none; none; none] in
      let loc = Env.loc env in
      let info = Env.Info.default PyCommon.pyObject in
      let env, id = Env.mk_fresh_ident env info in
      let instr = T.Instr.Let {id; exp= exit_exp; loc} in
      let env = Env.push_instr env instr in
      let env = Env.push env DataStack.NoException in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)
  end

  module CLEANUP_FINISH = struct
    (** {v WITH_CLEANUP_FINISH v}

        Finishes cleaning up the stack when a with statement block exits. top-of-stack is result of
        [__exit__()] or [__aexit__()] function call pushed by [WITH_CLEANUP_START]. [SECOND] is
        [NoException] or an exception type (pushed when an exception has been raised).

        Pops two values from the stack. If [SECOND] is not [NoException] and top-of-stack is true
        unwinds the [EXCEPT_HANDLER] block which was created when the exception was caught and
        pushes [NoException] to the stack.

        Note: we only support the [NoException] case for the moment *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, _exit_res = pop_datastack opname env in
      let* env, tos = pop_datastack opname env in
      let* () =
        if DataStack.is_no_exception tos then Ok ()
        else Error (L.InternalError, Error.TODO (Exception (opname, tos)))
      in
      (* TODO: check exit_res to deal with exception unwinding *)
      let env = Env.push env DataStack.NoException in
      Ok (env, None)
  end

  module SETUP = struct
    (** {v SETUP_WITH(delta) v}

        This opcode performs several operations before a with block starts. First, it loads
        [__exit__()] from the context manager and pushes it onto the stack for later use by
        [WITH_CLEANUP_START]. Then, [__enter__()] is called, and a finally block pointing to [delta]
        is pushed. Finally, the result of calling the [__enter__()] method is pushed onto the stack.
        The next opcode will either ignore it [POP_TOP], or store it in (a) variable(s)
        [STORE_FAST], [STORE_NAME], or [UNPACK_SEQUENCE]. *)
    let run env {FFI.Instruction.opname; arg= delta} next_offset_opt =
      Debug.p "[%s] delta= %d\n" opname delta ;
      let open IResult.Let_syntax in
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = next_offset next_offset_opt in
      let offset = next_offset + delta in
      let env = Env.register_with_target ~offset env in
      let* env, cell = pop_datastack opname env in
      let* id =
        match (cell : DataStack.cell) with
        | Temp id ->
            Ok id
        | _ ->
            Error (L.InternalError, Error.TODO (SetupWith cell))
      in
      let env = Env.push env (DataStack.WithContext id) in
      let proc =
        let enclosing_class = T.TypeName.wildcard in
        qualified_procname ~enclosing_class @@ proc_name PyCommon.enter
      in
      (* TODO: maybe track that the label pointed to by [delta] is a clean-up block *)
      let enter_exp = T.Exp.call_virtual proc (T.Exp.Var id) [] in
      let loc = Env.loc env in
      let info = Env.Info.default PyCommon.pyObject in
      let env, id = Env.mk_fresh_ident env info in
      let instr = T.Instr.Let {id; exp= enter_exp; loc} in
      let env = Env.push_instr env instr in
      let env = Env.push env (DataStack.Temp id) in
      Ok (env, None)
  end
end

module DUP = struct
  module TOP = struct
    (** {v DUP_TOP v}

        Duplicates the reference on top of the stack. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos = pop_datastack opname env in
      let env = Env.push env tos in
      let env = Env.push env tos in
      Ok (env, None)
  end

  module TOP_TWO = struct
    (** {v DUP_TOP_TWO v}

        Duplicates the two references on top of the stack, leaving them in the same order. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos0 = pop_datastack opname env in
      let* env, tos1 = pop_datastack opname env in
      let env = Env.push env tos1 in
      let env = Env.push env tos0 in
      let env = Env.push env tos1 in
      let env = Env.push env tos0 in
      Ok (env, None)
  end
end

module ROT = struct
  module TWO = struct
    (** {v ROT_TWO v}

        Swaps the two top-most stack items. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos0 = pop_datastack opname env in
      let* env, tos1 = pop_datastack opname env in
      let env = Env.push env tos0 in
      let env = Env.push env tos1 in
      Ok (env, None)
  end

  module THREE = struct
    (** {v ROT_THREE v}

        Lifts second and third stack item one position up, moves top down to position three. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos0 = pop_datastack opname env in
      let* env, tos1 = pop_datastack opname env in
      let* env, tos2 = pop_datastack opname env in
      let env = Env.push env tos0 in
      let env = Env.push env tos2 in
      let env = Env.push env tos1 in
      Ok (env, None)
  end

  module FOUR = struct
    (** {v ROT_FOUR v}

        Lifts second, third and fourth stack items one position up, moves top down to position four. *)
    let run env {FFI.Instruction.opname} =
      Debug.p "[%s]\n" opname ;
      let open IResult.Let_syntax in
      let* env, tos0 = pop_datastack opname env in
      let* env, tos1 = pop_datastack opname env in
      let* env, tos2 = pop_datastack opname env in
      let* env, tos3 = pop_datastack opname env in
      let env = Env.push env tos0 in
      let env = Env.push env tos3 in
      let env = Env.push env tos2 in
      let env = Env.push env tos1 in
      Ok (env, None)
  end
end

module UNPACK = struct
  module SEQUENCE = struct
    (** {v UNPACK_SEQUENCE(count) v}

        Unpacks top-of-stack into [count] individual values, which are put onto the stack
        right-to-left. *)
    let run env {FFI.Instruction.opname; arg= count} =
      let open IResult.Let_syntax in
      let build_let env exp n =
        let env, id, _typ =
          Env.mk_builtin_call env Builtin.PythonIndex [exp; T.Exp.Const (Int (Z.of_int n))]
        in
        Env.push env (DataStack.Temp id)
      in
      let rec unpack env exp n =
        if Int.equal n 0 then build_let env exp 0
        else
          let env = build_let env exp n in
          unpack env exp (n - 1)
      in
      Debug.p "[%s] count= %d\n" opname count ;
      let* env, cell = pop_datastack opname env in
      (* TODO: try to keep tuple type information around *)
      let* env, tuple, _info = load_cell env cell in
      let* () = if count <= 0 then Error (L.ExternalError, Error.UnpackSequence count) else Ok () in
      let env = unpack env tuple (count - 1) in
      Ok (env, None)
  end
end

module RAISE_VARARGS = struct
  (** {v RAISE_VARARGS(argc) v}

      Raises an exception using one of the 3 forms of the raise statement, depending on the value of
      [argc]:

      0: raise (re-raise previous exception)

      1: raise top-of-stack (raise exception instance or type at TOS)

      2: raise top-of-stack1 from top-of-stack (raise exception instance or type at top-of-stack1
      with [__cause__] set to top-of-stack) *)
  let run env {FFI.Instruction.opname; arg= argc} =
    let open IResult.Let_syntax in
    Debug.p "[%s] argc= %d\n" opname argc ;
    let* () =
      match argc with
      | 1 ->
          Ok ()
      | 0 | 2 ->
          Error (L.InternalError, Error.TODO (RaiseException argc))
      | _ ->
          Error (L.ExternalError, Error.RaiseExceptionInvalid argc)
    in
    let loc = Env.loc env in
    let* env, tos = pop_datastack opname env in
    let default () =
      let* env, exp, _info = load_cell env tos in
      let throw = JUMP.Throw exp in
      Ok (env, Some throw)
    in
    match (tos : DataStack.cell) with
    | Name {global; name} -> (
        let key = mk_key global loc name in
        let opt_sym = Env.lookup_symbol env key in
        match opt_sym with
        | Some {Symbol.kind= Class; id} ->
            (* TODO: Check if we can raise a class rather than an instance of a class *)
            let s = Ident.to_string ~sep:"::" id in
            let exp = T.Exp.Const (Str s) in
            let throw = JUMP.Throw exp in
            Ok (env, Some throw)
        | _ ->
            default () )
    | _ ->
        default ()
end

module FORMAT_VALUE = struct
  (** {v FORMAT_VALUE(flags) v}

      Used for implementing formatted literal strings (f-strings). Pops an optional [fmt_spec] from
      the stack, then a required value. [flags] is interpreted as follows:

      (flags & 0x03) == 0x00: value is formatted as-is.

      (flags & 0x03) == 0x01: call str() on value before formatting it.

      (flags & 0x03) == 0x02: call repr() on value before formatting it.

      (flags & 0x03) == 0x03: call ascii() on value before formatting it.

      (flags & 0x04) == 0x04: pop fmt_spec from the stack and use it, else use an empty fmt_spec.

      Formatting is performed using [PyObject_Format()]. The result is pushed on the stack. *)

  let mk_conv flags =
    match flags land 0x03 with
    | 0x00 ->
        None
    | 0x01 ->
        Some Builtin.PythonFormatStr
    | 0x02 ->
        Some Builtin.PythonFormatRepr
    | 0x03 ->
        Some Builtin.PythonFormatAscii
    | _ ->
        (* unreachable *)
        L.die InternalError "[FORMAT_VALUE] impossible flags 0x%x@\n" flags


  let has_fmt_spec flags = Int.equal (flags land 0x04) 0x04

  let run env {FFI.Instruction.opname; arg= flags} =
    let open IResult.Let_syntax in
    Debug.p "[%s] flags= %d@\n" opname flags ;
    let* env, fmt_spec =
      (* fmt_spec must be a string literal *)
      if has_fmt_spec flags then
        let* env, fmt_spec = pop_datastack opname env in
        let* env, fmt_spec, _ = load_cell env fmt_spec in
        Ok (env, fmt_spec)
      else Ok (env, T.Exp.Const Null)
    in
    let* env, value = pop_datastack opname env in
    let* env, value, _typ = load_cell env value in
    let conv_fn = mk_conv flags in
    let env, value =
      match conv_fn with
      | None ->
          (env, value)
      | Some conv_fn ->
          let env, id, _typ = Env.mk_builtin_call env conv_fn [value] in
          (env, T.Exp.Var id)
    in
    let env, id, _typ = Env.mk_builtin_call env Builtin.PythonFormat [value; fmt_spec] in
    let env = Env.push env (DataStack.Temp id) in
    Ok (env, None)
end

(** Main opcode dispatch function. *)
let run_instruction env code ({FFI.Instruction.opname; starts_line} as instr) next_offset_opt =
  Debug.p "Dump Stack:@\n%a@\n" (Pp.seq ~sep:"\n" DataStack.pp_cell) (Env.stack env) ;
  if Option.is_some starts_line then Debug.p ">@\n" ;
  Debug.p "> %s@\n" opname ;
  let env = Env.update_last_line env starts_line in
  (* TODO: there are < 256 opcodes, could setup an array of callbacks instead *)
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
      RETURN_VALUE.run env instr
  | "POP_BLOCK" ->
      POP_BLOCK.run env instr
  | "POP_TOP" ->
      POP_TOP.run env instr
  | "CALL_FUNCTION" ->
      FUNCTION.CALL.run env instr
  | "CALL_FUNCTION_KW" ->
      FUNCTION.CALL_KW.run env instr
  | "BINARY_ADD" ->
      BINARY.run env instr (Builtin.Binary Add)
  | "BINARY_SUBTRACT" ->
      BINARY.run env instr (Builtin.Binary Subtract)
  | "BINARY_AND" ->
      BINARY.run env instr (Builtin.Binary And)
  | "BINARY_FLOOR_DIVIDE" ->
      BINARY.run env instr (Builtin.Binary FloorDivide)
  | "BINARY_LSHIFT" ->
      BINARY.run env instr (Builtin.Binary LShift)
  | "BINARY_MATRIX_MULTIPLY" ->
      BINARY.run env instr (Builtin.Binary MatrixMultiply)
  | "BINARY_MODULO" ->
      BINARY.run env instr (Builtin.Binary Modulo)
  | "BINARY_MULTIPLY" ->
      BINARY.run env instr (Builtin.Binary Multiply)
  | "BINARY_OR" ->
      BINARY.run env instr (Builtin.Binary Or)
  | "BINARY_POWER" ->
      BINARY.run env instr (Builtin.Binary Power)
  | "BINARY_RSHIFT" ->
      BINARY.run env instr (Builtin.Binary RShift)
  | "BINARY_TRUE_DIVIDE" ->
      BINARY.run env instr (Builtin.Binary TrueDivide)
  | "BINARY_XOR" ->
      BINARY.run env instr (Builtin.Binary Xor)
  | "INPLACE_ADD" ->
      BINARY.run env instr (Builtin.Inplace Add)
  | "INPLACE_SUBTRACT" ->
      BINARY.run env instr (Builtin.Inplace Subtract)
  | "INPLACE_AND" ->
      BINARY.run env instr (Builtin.Inplace And)
  | "INPLACE_FLOOR_DIVIDE" ->
      BINARY.run env instr (Builtin.Inplace FloorDivide)
  | "INPLACE_LSHIFT" ->
      BINARY.run env instr (Builtin.Inplace LShift)
  | "INPLACE_MATRIX_MULTIPLY" ->
      BINARY.run env instr (Builtin.Inplace MatrixMultiply)
  | "INPLACE_MODULO" ->
      BINARY.run env instr (Builtin.Inplace Modulo)
  | "INPLACE_MULTIPLY" ->
      BINARY.run env instr (Builtin.Inplace Multiply)
  | "INPLACE_OR" ->
      BINARY.run env instr (Builtin.Inplace Or)
  | "INPLACE_POWER" ->
      BINARY.run env instr (Builtin.Inplace Power)
  | "INPLACE_RSHIFT" ->
      BINARY.run env instr (Builtin.Inplace RShift)
  | "INPLACE_TRUE_DIVIDE" ->
      BINARY.run env instr (Builtin.Inplace TrueDivide)
  | "INPLACE_XOR" ->
      BINARY.run env instr (Builtin.Inplace Xor)
  | "UNARY_POSITIVE" ->
      UNARY.run env instr (Builtin.Unary Positive)
  | "UNARY_NEGATIVE" ->
      UNARY.run env instr (Builtin.Unary Negative)
  | "UNARY_NOT" ->
      UNARY.run env instr (Builtin.Unary Not)
  | "UNARY_INVERT" ->
      UNARY.run env instr (Builtin.Unary Invert)
  | "MAKE_FUNCTION" ->
      FUNCTION.MAKE.run env instr
  | "POP_JUMP_IF_TRUE" ->
      JUMP.POP_IF.run ~next_is_true:false env instr next_offset_opt
  | "POP_JUMP_IF_FALSE" ->
      JUMP.POP_IF.run ~next_is_true:true env instr next_offset_opt
  | "JUMP_FORWARD" ->
      JUMP.FORWARD.run env instr next_offset_opt
  | "JUMP_ABSOLUTE" ->
      JUMP.ABSOLUTE.run env instr
  | "GET_ITER" ->
      ITER.GET.run env instr
  | "FOR_ITER" ->
      ITER.FOR.run env instr next_offset_opt
  | "BUILD_CONST_KEY_MAP" ->
      BUILD.CONST_KEY_MAP.run env instr
  | "LOAD_BUILD_CLASS" ->
      LOAD.BUILD_CLASS.run env instr
  | "LOAD_METHOD" ->
      METHOD.LOAD.run env code instr
  | "CALL_METHOD" ->
      METHOD.CALL.run env instr
  | "IMPORT_NAME" ->
      IMPORT.NAME.run env code instr
  | "IMPORT_FROM" ->
      IMPORT.FROM.run env code instr
  | "COMPARE_OP" ->
      COMPARE_OP.run env instr
  | "JUMP_IF_TRUE_OR_POP" ->
      JUMP.IF_OR_POP.run ~jump_if:true env instr next_offset_opt
  | "JUMP_IF_FALSE_OR_POP" ->
      JUMP.IF_OR_POP.run ~jump_if:false env instr next_offset_opt
  | "BUILD_LIST" ->
      BUILD.run env instr Builtin.List
  | "BUILD_MAP" ->
      BUILD.MAP.run env instr
  | "BUILD_SET" ->
      BUILD.run env instr Builtin.Set
  | "BUILD_TUPLE" ->
      BUILD.run env instr Builtin.Tuple
  | "BUILD_STRING" ->
      BUILD.run env instr Builtin.String
  | "BUILD_SLICE" ->
      BUILD.SLICE.run env instr
  | "STORE_SUBSCR" ->
      STORE.SUBSCR.run env instr
  | "BINARY_SUBSCR" ->
      BINARY.SUBSCR.run env instr
  | "EXTENDED_ARG" ->
      (* The FFI.Instruction framework already did the magic and this opcode can be ignored. *)
      Ok (env, None)
  | "BEGIN_FINALLY" ->
      FINALLY.BEGIN.run env instr
  | "END_FINALLY" ->
      FINALLY.END.run env instr
  | "SETUP_FINALLY" ->
      FINALLY.SETUP.run env instr next_offset_opt
  | "SETUP_WITH" ->
      WITH.SETUP.run env instr next_offset_opt
  | "WITH_CLEANUP_START" ->
      WITH.CLEANUP_START.run env instr
  | "WITH_CLEANUP_FINISH" ->
      WITH.CLEANUP_FINISH.run env instr
  | "DUP_TOP" ->
      DUP.TOP.run env instr
  | "DUP_TOP_TWO" ->
      DUP.TOP_TWO.run env instr
  | "ROT_TWO" ->
      ROT.TWO.run env instr
  | "ROT_THREE" ->
      ROT.THREE.run env instr
  | "ROT_FOUR" ->
      ROT.FOUR.run env instr
  | "UNPACK_SEQUENCE" ->
      UNPACK.SEQUENCE.run env instr
  | "RAISE_VARARGS" ->
      RAISE_VARARGS.run env instr
  | "FORMAT_VALUE" ->
      FORMAT_VALUE.run env instr
  | "SETUP_ANNOTATIONS" ->
      let env = Env.set_annotations env in
      Ok (env, None)
  | _ ->
      Error (L.InternalError, Error.TODO (UnsupportedOpcode opname))


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
          (* If it's the target of a [with] statement, ignore it *)
          if Env.is_with_target env ~offset then (env, None)
          else
            (* Probably the target of a back edge. Let's register and empty label info here. *)
            let env, label_name = Env.mk_fresh_label env in
            let label_info = Env.Label.mk label_name in
            (env, Some (offset, true, label_info))
        else (env, None) )


(** Iterator on [run_instruction]: this function will interpret instructions as long as terminator
    is not reached. *)
let rec run env code instructions =
  let open IResult.Let_syntax in
  match instructions with
  | [] ->
      Ok (env, None, [])
  | instr :: rest -> (
      (* If the current instruction a jump target (either because we already registered a label
         there, or because of the info from Python instructions), we stop and close the node *)
      let env, maybe_label = has_jump_target env instructions in
      match maybe_label with
      | None ->
          (* Nop, just continue processing the stream of instrunctions *)
          let next_offset_opt = offset_of_code rest in
          let* env, maybe_term = run_instruction env code instr next_offset_opt in
          if Option.is_some maybe_term then Ok (env, maybe_term, rest) else run env code rest
      | Some (_offset, maybe_backedge, label_info) ->
          (* Yes, let's stop there, and don't forget to keep [instr] around *)
          let* env, (ssa_args, ssa_parameters) = stack_to_ssa env in
          let label_info =
            if maybe_backedge then Env.Label.update_ssa_parameters label_info ssa_parameters
            else label_info
          in
          let* offset = offset_of_code instructions |> next_offset in
          let info = {label_info; offset; ssa_args} in
          Ok (env, Some (JUMP.Label info), instructions) )


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
  let open IResult.Let_syntax in
  let label_loc = first_loc_of_code instructions in
  let env, label_name, ssa_parameters = Env.Label.to_textual env label_loc label_info in
  let label = {T.NodeName.value= label_name; loc= label_loc} in
  (* process instructions until the next terminator *)
  let* env, maybe_term, rest = run env code instructions in
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
  match (maybe_term : JUMP.kind option) with
  | None ->
      Error (L.InternalError, Error.EOF)
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
      Ok (env, rest, node)
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
      Ok (env, rest, node)
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
      Ok (env, rest, node)
  | Some (Absolute {ssa_args; label_info; offset}) ->
      (* The current node ends up with an absolute jump to [target]. *)
      let env, node = unconditional_jump env ssa_args label_info offset in
      Debug.p "  Absolute: register %s at %d\n" (Env.Label.name label_info) offset ;
      Ok (env, rest, node)
  | Some (Throw exp) ->
      Debug.p "  Throwing exception %a\n" T.Exp.pp exp ;
      let node =
        T.Node.
          { label
          ; ssa_parameters
          ; exn_succs= [] (* TODO ? *)
          ; last= Throw exp
          ; instrs= Env.instructions env
          ; last_loc
          ; label_loc }
      in
      Ok (env, rest, node)


(** Process a sequence of instructions until there is no more left to process. *)
let rec nodes env label_info code instructions =
  let open IResult.Let_syntax in
  let env = Env.enter_node env in
  let* env, instructions, textual_node = until_terminator env label_info code instructions in
  if List.is_empty instructions then Ok (env, [textual_node])
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
    let* env, more_textual_nodes = nodes env label_info code instructions in
    Ok (env, textual_node :: more_textual_nodes)


(** Given a function signature and some default arguments, this function will generate a specialized
    version with some formal arguments removed, replaced by their default value. *)
let specialize_proc_decl qualified_name formals_types result_type attributes params
    default_arguments =
  let open IResult.Let_syntax in
  let param_size = List.length params in
  let default_size = List.length default_arguments in
  let* () =
    if param_size < default_size then
      Error
        (L.ExternalError, Error.DefaultArgSpecialization (qualified_name, default_size, param_size))
    else Ok ()
  in
  let n = param_size - default_size in
  let params, locals = List.split_n params n in
  let formals_types, default_types = List.split_n formals_types n in
  let typed_params = List.zip_exn params formals_types in
  let locals = List.zip_exn locals default_types in
  let defaults = List.zip_exn locals default_arguments in
  let procdecl =
    {T.ProcDecl.qualified_name; formals_types= Some formals_types; result_type; attributes}
  in
  let stores =
    List.map defaults ~f:(fun ((var, {T.Typ.typ}), exp) ->
        T.Instr.Store {exp1= T.Exp.Lvar var; typ= Some typ; exp2= exp; loc= Unknown} )
  in
  let id = T.Ident.of_int 0 in
  let args =
    List.map
      ~f:(fun (var, {T.Typ.typ}) -> T.Exp.Load {exp= T.Exp.Lvar var; typ= Some typ})
      typed_params
  in
  let local_args =
    List.map ~f:(fun (var, {T.Typ.typ}) -> T.Exp.Load {exp= T.Exp.Lvar var; typ= Some typ}) locals
  in
  let exp = T.Exp.call_non_virtual qualified_name (args @ local_args) in
  let instrs = stores @ [T.Instr.Let {id; exp; loc= Unknown}] in
  let label = {T.NodeName.value= "b0"; loc= Unknown} in
  let node =
    { T.Node.label
    ; ssa_parameters= []
    ; exn_succs= []
    ; last= T.Terminator.Ret (T.Exp.Var id)
    ; instrs
    ; last_loc= Unknown
    ; label_loc= Unknown }
  in
  Ok {T.ProcDesc.procdecl; nodes= [node]; start= label; params; locals; exit_loc= Unknown}


let generate_specialized_proc_decl qualified_name formals_types result_type attributes params
    default_arguments =
  let open IResult.Let_syntax in
  if List.is_empty default_arguments then Ok []
  else
    let f = specialize_proc_decl qualified_name formals_types result_type attributes params in
    let rec fold default_arguments =
      match default_arguments with
      | [] ->
          Ok []
      | _ :: tl ->
          let* decl = f default_arguments in
          let* decls = fold tl in
          Ok (T.Module.Proc decl :: decls)
    in
    fold default_arguments


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
let constructor full_name loc has_init =
  let mk typ = annotated_type_of_annotation typ in
  let full_type_name = Ident.to_type_name full_name in
  let struct_ = T.Typ.(Struct full_type_name) in
  let sil_allocate : T.QualifiedProcName.t =
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
          let typ = Some typ in
          let instr = T.Instr.Load {id= next_id; exp; typ; loc} in
          (T.Ident.next next_id, T.Exp.Var next_id :: ids, instr :: instrs) )
    in
    let ns = List.rev rev_ns in
    let init = proc_name ~loc PyCommon.init__ in
    let init = qualified_procname ~enclosing_class:full_type_name init in
    let init = T.Exp.call_virtual init (T.Exp.Var n0) ns in
    let load = T.Instr.Let {id= n; exp= init; loc} in
    let loads = List.rev (load :: rev_loads) in
    (loads, params, formals_types)
  in
  let result_type = T.Typ.mk_without_attributes @@ Ident.to_typ full_name in
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
    { T.ProcDecl.qualified_name= {enclosing_class= TopLevel; name= Ident.to_constructor full_name}
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
        let qualified_name : T.QualifiedProcName.t =
          {enclosing_class= TopLevel; name= Ident.to_constructor class_name}
        in
        let result_type = T.Typ.mk_without_attributes @@ Ident.to_typ class_name in
        (qualified_name, result_type)
    | `Init ->
        let qualified_name : T.QualifiedProcName.t =
          { enclosing_class= Enclosing (Ident.to_type_name class_name)
          ; name= proc_name ~loc PyCommon.init__ }
        in
        let result_type = T.Typ.mk_without_attributes @@ PyCommon.pyNone in
        (qualified_name, result_type)
  in
  T.Module.Procdecl {T.ProcDecl.qualified_name; formals_types= None; result_type; attributes= []}


(** Process the special function generated by the Python compiler to create a class instances. This
    is where we can see some of the type annotations for fields, and method definitions. *)
let rec class_declaration env module_name ({FFI.Code.instructions; co_name} as code) loc parents =
  let open IResult.Let_syntax in
  Debug.p "[class declaration] %s (%a)\n" co_name (Pp.seq ~sep:", " Ident.pp) parents ;
  (* TODO:
     - use annotations to declare class member types
     - pass in [env] to PyClassDecl when we'll support decorators
  *)
  let class_name = Ident.extend ~prefix:module_name co_name in
  let static_class_name = Ident.extend ~prefix:module_name (PyCommon.static_companion co_name) in
  let class_type_name = Ident.to_type_name class_name in
  let static_class_type_name = Ident.to_type_name static_class_name in
  let* {PyClassDecl.State.members; methods; static_methods; has_init; has_new} =
    Result.map_error ~f:Error.class_decl
    @@ PyClassDecl.parse_class_declaration code co_name instructions
  in
  let load_defaults env defaults =
    let* env, defaults =
      Env.map_result ~env defaults ~f:(fun env cell ->
          let* env, exp, _info = load_cell env cell in
          Ok (env, exp) )
    in
    Ok (env, defaults)
  in
  let register_methods env codes method_infos opname =
    List.fold_result method_infos ~init:(env, codes)
      ~f:(fun
          (env, codes)
          {PyClassDecl.State.code; signature; defaults; flags; is_static; is_abstract}
        ->
        let* env, default_arguments = load_defaults env defaults in
        let* () = check_flags opname flags in
        let env =
          match FFI.Constant.as_code code with
          | Some code ->
              let annotations = List.filter_map ~f:(lift_annotation env) signature in
              let info = {PyEnv.Signature.is_static; is_abstract; annotations} in
              Env.register_method env ~enclosing_class:class_name ~method_name:code.FFI.Code.co_name
                info default_arguments
          | None ->
              env
        in
        Ok (env, code :: codes) )
  in
  if Option.is_some has_new then Error (L.InternalError, Error.TODO (New class_name))
  else
    let* env, codes = register_methods env [] methods "<METHOD_DECLARATION>" in
    let* env, codes = register_methods env codes static_methods "<STATIC_METHOD_DECLARATION>" in
    let env = Env.register_fields env class_type_name members in
    let fields =
      List.map members ~f:(fun {PyCommon.name; annotation} ->
          let annotation =
            let lifted_annotation = lift_type_ident env annotation in
            Option.value ~default:annotation lifted_annotation
          in
          let name = field_name ~loc name in
          let qualified_name = {T.enclosing_class= class_type_name; name} in
          let typ = Ident.to_typ annotation in
          {T.FieldDecl.qualified_name; typ; attributes= []} )
    in
    let* env, decls = to_proc_descs env class_name (Array.of_list codes) in
    let supers, static_supers, parent_inits =
      (* TODO: check if/how ordering of parent class matters. Atm I keep the same order. *)
      List.fold_right ~init:([], [], []) parents
        ~f:(fun parent (supers, static_supers, parent_inits) ->
          let parent_id =
            let key = Symbol.Global parent in
            match Env.lookup_symbol env key with Some {Symbol.id} -> Some id | _ -> None
          in
          let is_imported_from_ABC =
            Option.value_map ~default:false ~f:Ident.is_imported_ABC parent_id
          in
          if is_imported_from_ABC then (supers, static_supers, parent_inits)
          else
            let parent, parent_inits =
              match parent_id with
              | None ->
                  (* If the parent class is imported, we can't do much check, so we trust it upfront,
                     since compilation was happy. We also have to declare its
                     constructor in case [super().__init__] is in the source
                     code *)
                  (parent, parent :: parent_inits)
              | Some parent ->
                  (parent, parent_inits)
            in
            let supers = Ident.to_type_name ~static:false parent :: supers in
            let static_supers = Ident.to_type_name ~static:true parent :: static_supers in
            (supers, static_supers, parent_inits) )
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
      T.Module.Global
        {name= Ident.to_var_name static_class_name; typ= PyCommon.pyObject; attributes= []}
    in
    let env, ctor_and_init =
      match has_init with
      | None ->
          let ctor = constructor_stubs ~kind:`Ctor loc class_name in
          let init = constructor_stubs ~kind:`Init loc class_name in
          (env, [init; ctor])
      | Some has_init ->
          (* [__init__] is already present in the source code, only generate the constructor.
             We also declare parent constructors that might be in another file *)
          let env, parent_inits =
            List.fold_left ~init:(env, [])
              ~f:(fun (env, parent_inits) parent ->
                let info =
                  {PyEnv.Signature.is_static= false; is_abstract= false; annotations= []}
                in
                let env =
                  Env.register_method env ~enclosing_class:parent ~method_name:PyCommon.init__ info
                    []
                in
                let init = Ident.extend ~prefix:parent PyCommon.init__ in
                let init = Ident.to_qualified_procname init in
                let result_type = T.Typ.{typ= PyCommon.pyNone; attributes= []} in
                let init =
                  T.{ProcDecl.qualified_name= init; formals_types= None; result_type; attributes= []}
                in
                let parent_inits = T.Module.Procdecl init :: parent_inits in
                (env, parent_inits) )
              parent_inits
          in
          let ctor = constructor class_name loc has_init in
          (env, ctor :: parent_inits)
    in
    Ok (env, (t :: companion :: companion_const :: ctor_and_init) @ decls)


(** Process a single code unit (toplevel code, function body, ...) *)
and to_proc_desc env loc enclosing_class_name opt_name ({FFI.Code.instructions} as code) =
  let open IResult.Let_syntax in
  Debug.p "[to_proc_desc] %a %a\n" Ident.pp enclosing_class_name (Pp.option F.pp_print_string)
    opt_name ;
  let default annotations =
    let signature = {Env.Signature.is_static= false; is_abstract= false; annotations} in
    {Env.signature; default_arguments= []}
  in
  (* TODO: nested classes mangling. Should we insert something like `$nested` or is it always unambiguous ? *)
  let is_toplevel, name, method_info =
    match opt_name with
    | None ->
        let return_typ = {PyCommon.name= PyCommon.return; annotation= Ident.mk "None"} in
        let method_info = default [return_typ] in
        (true, PyCommon.toplevel_function, method_info)
    | Some name -> (
        let method_info = Env.lookup_method env ~enclosing_class:enclosing_class_name name in
        match method_info with
        | None ->
            (false, name, default [])
        | Some method_info ->
            (false, name, method_info) )
  in
  let {Env.signature; default_arguments} = method_info in
  let {Env.Signature.is_static; is_abstract; annotations} = signature in
  let proc_name = proc_name ~loc name in
  let enclosing_class = Ident.to_type_name ~static:is_static enclosing_class_name in
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
  let enclosing_class_name =
    Option.value_map ~default:enclosing_class_name
      ~f:(fun name -> Ident.extend ~prefix:enclosing_class_name name)
      opt_name
  in
  let env = Env.enter_proc ~is_toplevel ~is_static ~module_name:enclosing_class_name ~params env in
  let locals = FFI.Code.get_locals code in
  let params = List.map ~f:(var_name ~loc) params in
  let locals = Array.map ~f:(fun name -> (var_name ~loc name, pyObject)) locals |> Array.to_list in
  let types =
    List.map
      ~f:(fun {PyCommon.name; annotation} ->
        let annotation = annotated_type_of_annotation annotation in
        (name, annotation) )
      annotations
  in
  let env, formals_types =
    Env.map ~env
      ~f:(fun env {T.VarName.value; loc} ->
        let typ = List.Assoc.find types ~equal:String.equal value in
        let typ = Option.value typ ~default:pyObject in
        let id = Ident.mk ~loc value in
        let sym = {Symbol.kind= Name {is_imported= false; typ= typ.T.Typ.typ}; id; loc} in
        let env = Env.register_symbol env (Symbol.Local value) sym in
        (env, typ) )
      params
  in
  let result_type =
    List.Assoc.find types ~equal:String.equal PyCommon.return |> Option.value ~default:pyObject
  in
  let procdecl =
    {T.ProcDecl.qualified_name; formals_types= Some formals_types; result_type; attributes= []}
  in
  let* env, decls =
    if is_abstract then Ok (env, [T.Module.Procdecl procdecl])
    else
      let* specialized_decls =
        generate_specialized_proc_decl qualified_name formals_types result_type [] params
          default_arguments
      in
      let env, entry_label = Env.mk_fresh_label env in
      let label = node_name ~loc entry_label in
      let label_info = Env.Label.mk entry_label in
      let* env, nodes = nodes env label_info code instructions in
      Ok
        ( env
        , T.Module.Proc {T.ProcDesc.procdecl; nodes; start= label; params; locals; exit_loc= Unknown}
          :: specialized_decls )
  in
  let* env, nested_decls =
    if Option.is_some opt_name then to_proc_descs env enclosing_class_name code.FFI.Code.co_consts
    else Ok (env, [])
  in
  Ok (env, decls @ nested_decls)


(** Process multiple [code] objects. Usually called by the toplevel function or by a class
    definition. *)
and to_proc_descs env enclosing_class_id codes =
  let open IResult.Let_syntax in
  Debug.p "[to_proc_descs] %a\n" Ident.pp enclosing_class_id ;
  Array.fold codes
    ~init:(Ok (env, []))
    ~f:(fun acc const ->
      let* env, decls = acc in
      match FFI.Constant.as_code const with
      | None ->
          Ok (env, decls)
      | Some ({FFI.Code.co_name; instructions} as code) -> (
          let loc = first_loc_of_code instructions in
          (* TODO: supported nested classes *)
          let classes = Env.get_declared_classes env in
          match SMap.find_opt co_name classes with
          | Some {parents} ->
              let* env, new_decls = class_declaration env enclosing_class_id code loc parents in
              Ok (env, new_decls @ decls)
          | None ->
              let* env, specialized_decls =
                to_proc_desc env loc enclosing_class_id (Some co_name) code
              in
              Ok (env, specialized_decls @ decls) ) )


let python_attribute = T.Attr.mk_source_language T.Lang.Python

(** Entry point of the module: process a whole Python file / compilation unit into Textual *)
let to_module ~sourcefile ({FFI.Code.co_consts; co_name; co_filename; instructions} as code) =
  let open IResult.Let_syntax in
  Debug.p "[to_module] %a %s\n" T.SourceFile.pp sourcefile co_name ;
  if not (String.equal co_name "<module>") then Error (L.ExternalError, Error.TopLevelName co_name)
  else
    let loc = first_loc_of_code instructions in
    let* module_name =
      let filename =
        let sz = String.length co_filename in
        if sz >= 2 && String.equal "./" (String.sub co_filename ~pos:0 ~len:2) then
          String.sub co_filename ~pos:2 ~len:(sz - 2)
        else co_filename
      in
      let filename = Stdlib.Filename.remove_extension filename in
      match Ident.from_string ~loc filename with
      | None ->
          Error (L.ExternalError, Error.TopLevelInvalid filename)
      | Some module_name ->
          Ok module_name
    in
    let env = Env.empty module_name in
    (* Process top level module first, to gather all global definitions.
       This will also help us identify what is a class and what is a function,
       before processing the rest of the [co_consts]. There is no flag to
       distinguish class definitions from function definitions in the bytecode
       format.

       TODO: we should process classes (at least partially) first to get their
       method signatures and field types to get more type information while
       parsing the toplevel blob.


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
    let* env, specialized_decls = to_proc_desc env loc module_name None code in
    (* Translate globals to Textual *)
    let globals =
      Ident.Map.fold
        (fun _name {Symbol.kind; id} acc ->
          match (kind : Symbol.kind) with
          | Name {is_imported} ->
              if is_imported then acc
              else
                let varname = Ident.to_var_name id in
                let global = T.Global.{name= varname; typ= PyCommon.pyObject; attributes= []} in
                T.Module.Global global :: acc
          | Builtin | Code | Class | Import | ImportCall ->
              (* don't generate a global variable name, it will be declared as a toplevel decl *)
              acc )
        (Env.globals env) []
    in
    (* Then, process any code body that is in code.co_consts *)
    let* env, decls = to_proc_descs env module_name co_consts in
    let decls = List.rev decls in
    (* Declare all the import top level calls *)
    let imports = Env.get_textual_imports env in
    (* Declare all the python implicits *)
    let python_implicit_names =
      List.map
        ~f:(fun name ->
          T.Module.Global
            (let name = sprintf "%s::%s" python_implicit_names_prefix name in
             {T.Global.name= var_name name; typ= PyCommon.pyString; attributes= []} ) )
        python_implicit_names
    in
    (* Gather everything into a Textual module *)
    let decls =
      ((specialized_decls @ decls) @ globals @ imports @ python_implicit_names)
      @ Builtin.Set.to_textual (Env.get_used_builtins env)
    in
    Ok {T.Module.attrs= [python_attribute]; decls; sourcefile}
