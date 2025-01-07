(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PyIR

module Parameter = struct
  let locals = Textual.VarName.of_string "locals"

  let globals = Textual.VarName.of_string "globals"
end

let location_from_opt_line = function
  | None ->
      Textual.Location.Unknown
  | Some line ->
      Textual.Location.known ~line ~col:(-1)


let of_location loc = Location.line loc |> location_from_opt_line

module Typ = struct
  let locals = Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string "PyLocals")))

  let globals module_name =
    let str = F.asprintf "PyGlobals::%a" Ident.pp module_name in
    Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string str)))


  let class_companion_name module_name name =
    let str = F.asprintf "PyClassCompanion::%a::%s" Textual.TypeName.pp module_name name in
    Textual.(TypeName.of_string str)


  let class_companion module_name name =
    Textual.(Typ.Ptr (Typ.Struct (class_companion_name module_name name)))


  let module_attribute module_name attr_name =
    let str = F.asprintf "PyModuleAttr::%s::%s" module_name attr_name in
    Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string str)))


  let value = Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string "PyObject")))
end

let global_type_of_str str = Typ.globals (Ident.mk str)

let str_module_body = "__module_body__"

type proc_kind = ModuleBody of Ident.t | RegularFunction of QualName.t

let is_module_body = function ModuleBody _ -> true | _ -> false

let typename_of_ident ?loc ident = Textual.TypeName.of_string ?loc (F.asprintf "%a" Ident.pp ident)

let mk_qualified_proc_name ?loc kind : Textual.QualifiedProcName.t =
  let procname_of_ident ident = Textual.ProcName.of_string ?loc (F.asprintf "%a" Ident.pp ident) in
  match kind with
  | ModuleBody name ->
      { enclosing_class= Enclosing (typename_of_ident name)
      ; name= Textual.ProcName.of_string ?loc str_module_body }
  | RegularFunction {module_name; function_name} ->
      { enclosing_class= Enclosing (typename_of_ident ?loc module_name)
      ; name= procname_of_ident function_name }


let mk_procdecl_attributes {CodeInfo.co_argcount; co_varnames; is_async} =
  let attrs = if is_async then [Textual.Attr.mk_async] else [] in
  let values =
    List.init co_argcount ~f:(fun i -> co_varnames.(i)) |> List.map ~f:(F.asprintf "%a" Ident.pp)
  in
  if List.is_empty values then attrs else Textual.Attr.mk_python_args values :: attrs


let mk_procdecl ?loc kind code_info =
  let qualified_name = mk_qualified_proc_name ?loc kind in
  let formals_types =
    match kind with
    | ModuleBody name ->
        Some [Textual.Typ.mk_without_attributes (Typ.globals name)]
    | RegularFunction {module_name} ->
        Some
          [ Textual.Typ.mk_without_attributes (Typ.globals module_name)
          ; Textual.Typ.mk_without_attributes Typ.locals ]
  in
  let result_type = Textual.Typ.mk_without_attributes Typ.value in
  let attributes = mk_procdecl_attributes code_info in
  {Textual.ProcDecl.qualified_name; formals_types; result_type; attributes}


let mk_ident ssa = Textual.Ident.of_int (SSA.id ssa)

let builtin_qual_proc_name name : Textual.QualifiedProcName.t =
  { enclosing_class= Enclosing (Textual.TypeName.of_string "$builtins")
  ; name= Textual.ProcName.of_string name }


let call_builtin name args =
  let proc = builtin_qual_proc_name name in
  Textual.Exp.Call {proc; args; kind= NonVirtual}


let str_py_make_string = "py_make_string"

let of_const cst =
  let open Textual in
  let mk_const c = Exp.Const c in
  match (cst : PyIR.Const.t) with
  | Bool true ->
      call_builtin "py_bool_true" []
  | Bool false ->
      call_builtin "py_bool_false" []
  | Int i ->
      call_builtin "py_make_int" [mk_const (Const.Int i)]
  | Float f ->
      call_builtin "py_make_float" [mk_const (Const.Float f)]
  | Complex {real; imag} ->
      call_builtin "py_make_complex" [mk_const (Const.Float real); mk_const (Const.Float imag)]
  | String s ->
      call_builtin str_py_make_string [mk_const (Const.Str s)]
  | InvalidUnicode _ ->
      call_builtin "py_invalid_unicode" []
  | Bytes bytes ->
      call_builtin "py_make_bytes" [mk_const (Const.Str (Bytes.to_string bytes))]
  | None ->
      call_builtin "py_make_none" []


let exp_of_ident_str ident = Textual.Exp.Const (Str (F.asprintf "%a" Ident.pp ident))

let exp_locals = Textual.(Exp.Var (Ident.of_int 1))

let exp_globals = Textual.(Exp.Var (Ident.of_int 2))

let str_py_import_name = "py_import_name"

let str_py_import_from = "py_import_from"

let str_py_make_function = "py_make_function"

let rec of_exp exp : Textual.Exp.t =
  match (exp : Exp.t) with
  | AssertionError ->
      call_builtin "py_load_assertion_error" []
  | Const const ->
      of_const const
  | Var {scope= Global; ident} ->
      call_builtin "py_load_global" [exp_of_ident_str ident; exp_globals]
  | Var {scope= Fast; ident} ->
      call_builtin "py_load_fast" [exp_of_ident_str ident; exp_locals]
  | Var {scope= Name; ident} ->
      call_builtin "py_load_name" [exp_of_ident_str ident; exp_locals; exp_globals]
  | LoadClosure {name; slot= _} ->
      call_builtin "py_load_closure" [exp_of_ident_str name] (* TODO: more arg needed *)
  | LoadDeref {name; slot= _} ->
      call_builtin "py_load_deref" [exp_of_ident_str name] (* TODO: more arg needed *)
  | LoadClassDeref {name; slot= _} ->
      call_builtin "py_load_class_deref" [exp_of_ident_str name] (* TODO: more arg needed *)
  | ImportName {name; fromlist; level} ->
      let str = typename_of_ident name |> F.asprintf "%a" Textual.TypeName.pp in
      call_builtin str_py_import_name
        [exp_globals; Textual.Exp.Const (Str str); of_exp fromlist; of_exp level]
  | ImportFrom {name; exp} ->
      call_builtin str_py_import_from [exp_of_ident_str name; of_exp exp]
  | Temp ssa ->
      Var (mk_ident ssa)
  | MatchClass {subject; type_; count; names} ->
      let count = Textual.(Exp.Const (Const.Int (Z.of_int count))) in
      call_builtin "py_match_class" [of_exp subject; of_exp type_; count; of_exp names]
  | BoolOfMatchClass exp ->
      call_builtin "py_bool_of_match_class" [of_exp exp]
  | AttributesOfMatchClass exp ->
      call_builtin "py_attributes_of_match_class" [of_exp exp]
  | MatchSequence exp ->
      call_builtin "py_match_sequence" [of_exp exp]
  | GetLen exp ->
      call_builtin "py_get_len" [of_exp exp]
  | Subscript {exp; index} ->
      call_builtin "py_subscript" [of_exp exp; of_exp index]
  | BuildSlice args ->
      call_builtin "py_build_slice" (List.map args ~f:of_exp)
  | BuildString args ->
      call_builtin "py_build_string" (List.map args ~f:of_exp)
  | BuildFrozenSet args ->
      call_builtin "py_build_frozen_set" (List.map args ~f:of_exp)
  | Collection {kind; values; unpack} ->
      let kind =
        match kind with List -> "list" | Set -> "set" | Tuple -> "tuple" | Map -> "map"
      in
      let unpack = if unpack then "_unpack" else "" in
      let builtin_name = F.asprintf "py_build_%s%s" unpack kind in
      call_builtin builtin_name (List.map values ~f:of_exp)
  | GetAttr {exp; attr} ->
      let attr = exp_of_ident_str attr in
      call_builtin "py_get_attr" [of_exp exp; attr]
  | Function {qual_name; default_values; default_values_kw; annotations; cells_for_closure} ->
      let proc = mk_qualified_proc_name (RegularFunction qual_name) in
      let closure =
        Textual.Exp.Closure {proc; captured= [exp_globals]; params= [Parameter.locals]}
      in
      call_builtin str_py_make_function
        ( closure
        :: List.map ~f:of_exp [default_values; default_values_kw; annotations; cells_for_closure] )
  | Yield exp ->
      call_builtin "py_yield" [of_exp exp]


let mk_node_name node_name = F.asprintf "%a" NodeName.pp node_name |> Textual.NodeName.of_string

let mk_jump {Terminator.label; ssa_args} =
  Textual.Terminator.(Jump [{label= mk_node_name label; ssa_args= List.map ssa_args ~f:of_exp}])


let of_terminator terminator : Textual.Terminator.t =
  match (terminator : Terminator.t) with
  | Return exp ->
      Ret (of_exp exp)
  | Throw exp ->
      Throw (of_exp exp)
  | Jump node_call ->
      mk_jump node_call
  | If {exp; then_; else_} ->
      let exp = of_exp exp in
      let then_ = mk_jump then_ in
      let else_ = mk_jump else_ in
      If {bexp= Exp exp; then_; else_}


let binary_op_name op =
  match (op : BinaryOp.t) with
  | Add ->
      "add"
  | And ->
      "and"
  | FloorDivide ->
      "floor_divide"
  | LShift ->
      "lshift"
  | MatrixMultiply ->
      "matrix_multiply"
  | Modulo ->
      "modulo"
  | Multiply ->
      "multiply"
  | Or ->
      "or"
  | Power ->
      "power"
  | RShift ->
      "rshift"
  | Subtract ->
      "substract"
  | TrueDivide ->
      "true_divide"
  | Xor ->
      "xor"


let unary_op_name op =
  match (op : UnaryOp.t) with
  | Positive ->
      "positive"
  | Negative ->
      "negative"
  | Not ->
      "not"
  | Invert ->
      "invert"


let compare_op_name op =
  match (op : CompareOp.t) with
  | Lt ->
      "lt"
  | Le ->
      "le"
  | Eq ->
      "eq"
  | Neq ->
      "neq"
  | Gt ->
      "gt"
  | Ge ->
      "ge"
  | In ->
      "in"
  | NotIn ->
      "not_in"
  | Is ->
      "is"
  | IsNot ->
      "is_not"
  | Exception ->
      "exception"
  | BAD ->
      "bad"


let str_py_build_class = "py_build_class"

let builtin_name builtin =
  match (builtin : BuiltinCaller.t) with
  | BuildClass ->
      str_py_build_class
  | BuildConstKeyMap ->
      "py_build_const_key_map"
  | Format ->
      "py_format"
  | FormatFn Str ->
      "py_format_fn_str"
  | FormatFn Repr ->
      "py_format_fn_repr"
  | FormatFn Ascii ->
      "py_format_fn_ascii"
  | CallFunctionEx ->
      "py_call_function_ex"
  | Inplace op ->
      F.asprintf "py_inplace_%s" (binary_op_name op)
  | Binary op ->
      F.asprintf "py_binary_%s" (binary_op_name op)
  | Unary op ->
      F.asprintf "py_unary_%s" (unary_op_name op)
  | Compare op ->
      F.asprintf "py_compare_%s" (compare_op_name op)
  | GetAIter ->
      "py_get_aiter"
  | GetIter ->
      "py_get_iter"
  | NextIter ->
      "py_next_iter"
  | HasNextIter ->
      "py_has_next_iter"
  | IterData ->
      "py_iter_data"
  | GetYieldFromIter ->
      "py_get_yield_from_iter"
  | ListAppend ->
      "py_list_append"
  | ListExtend ->
      "py_list_extend"
  | ListToTuple ->
      "py_list_to_tuple"
  | SetAdd ->
      "py_set_add"
  | SetUpdate ->
      "py_set_update"
  | DictSetItem ->
      "py_dict_set_item"
  | DictUpdate ->
      "py_dict_update"
  | DictMerge ->
      "py_dict_merge"
  | DeleteSubscr ->
      "py_delete_subscr"
  | YieldFrom ->
      "py_yield_from"
  | GetAwaitable ->
      "py_get_awaitable"
  | UnpackEx ->
      "py_unpack_ex"
  | GetPreviousException ->
      "py_get_previous_exception"


let str_py_store_name = "py_store_name"

let str_py_call = "py_call"

let of_stmt loc stmt : Textual.Instr.t =
  match (stmt : Stmt.t) with
  | Let {lhs; rhs} ->
      Let {id= Some (mk_ident lhs); exp= of_exp rhs; loc}
  | SetAttr {lhs; attr; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_set_attr" [of_exp lhs; exp_of_ident_str attr; of_exp rhs]
        ; loc }
  | Store {lhs= {scope= Global; ident}; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_store_global" [exp_of_ident_str ident; exp_globals; of_exp rhs]
        ; loc }
  | Store {lhs= {scope= Fast; ident}; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_store_fast" [exp_of_ident_str ident; exp_locals; of_exp rhs]
        ; loc }
  | Store {lhs= {scope= Name; ident}; rhs} ->
      Let
        { id= None
        ; exp=
            call_builtin str_py_store_name
              [exp_of_ident_str ident; exp_locals; exp_globals; of_exp rhs]
        ; loc }
  | StoreDeref {name; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_store_deref" [exp_of_ident_str name; (* TODO: add arg *) of_exp rhs]
        ; loc }
  | StoreSubscript {lhs; index; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_store_subscript" [of_exp lhs; of_exp index; of_exp rhs]
        ; loc }
  | Call {lhs; exp; args; arg_names} ->
      let args = of_exp exp :: of_exp arg_names :: List.map ~f:of_exp args in
      Let {id= Some (mk_ident lhs); exp= call_builtin str_py_call args; loc}
  | CallMethod {lhs; name; self_if_needed; args; arg_names} ->
      Let
        { id= Some (mk_ident lhs)
        ; exp=
            call_builtin "py_call_method"
              ( exp_of_ident_str name :: of_exp self_if_needed :: of_exp arg_names
              :: List.map ~f:of_exp args )
        ; loc }
  | BuiltinCall {lhs; call; args; arg_names= _} ->
      let args = List.map ~f:of_exp args in
      Let {id= Some (mk_ident lhs); exp= call_builtin (builtin_name call) args; loc}
  | Delete {scope= Global; ident} ->
      Let {id= None; exp= call_builtin "py_delete_global" [exp_of_ident_str ident; exp_globals]; loc}
  | Delete {scope= Fast; ident} ->
      Let {id= None; exp= call_builtin "py_delete_fast" [exp_of_ident_str ident; exp_locals]; loc}
  | Delete {scope= Name; ident} ->
      Let
        { id= None
        ; exp= call_builtin "py_delete_name" [exp_of_ident_str ident; exp_locals; exp_globals]
        ; loc }
  | DeleteDeref {name} ->
      Let
        { id= None
        ; exp= call_builtin "py_delete_deref" [exp_of_ident_str name (* TODO: add arg *)]
        ; loc }
  | DeleteAttr {exp; attr} ->
      Let {id= None; exp= call_builtin "py_delete_attr" [of_exp exp; exp_of_ident_str attr]; loc}
  | SetupAnnotations ->
      Let {id= None; exp= call_builtin "py_setup_annotations" []; loc}
  | ImportStar exp ->
      Let {id= None; exp= call_builtin "py_import_star" [of_exp exp]; loc}
  | GenStart {kind} ->
      let kind =
        match kind with
        | Generator ->
            "generator"
        | Coroutine ->
            "coroutine"
        | AsyncGenerator ->
            "async_generator"
      in
      Let {id= None; exp= call_builtin ("py_gen_start_" ^ kind) []; loc}


let of_node is_module_body nullify_locals entry
    {Node.name; first_loc; last_loc; ssa_parameters; stmts; last} =
  let label = mk_node_name name in
  let label_loc = of_location first_loc in
  let last_loc = of_location last_loc in
  let last = of_terminator last in
  let instrs =
    List.map stmts ~f:(fun (loc, stmt) ->
        let loc = of_location loc in
        of_stmt loc stmt )
    |> nullify_locals last_loc last
  in
  let instrs =
    if is_module_body && NodeName.equal name entry then
      let loc = label_loc in
      Textual.(
        Instr.Let
          {id= Some (Ident.of_int 2); exp= Load {exp= Lvar Parameter.globals; typ= None}; loc}
        :: Instr.Store {exp1= Lvar Parameter.locals; exp2= exp_globals; typ= None; loc}
        :: Instr.Let
             {id= Some (Ident.of_int 1); exp= Load {exp= Lvar Parameter.locals; typ= None}; loc}
        :: instrs )
    else if NodeName.equal name entry then
      let loc = label_loc in
      Textual.(
        Instr.Let
          {id= Some (Ident.of_int 2); exp= Load {exp= Lvar Parameter.globals; typ= None}; loc}
        :: Instr.Let
             {id= Some (Ident.of_int 1); exp= Load {exp= Lvar Parameter.locals; typ= None}; loc}
        :: instrs )
    else instrs
  in
  let exn_succs = [] (* TODO *) in
  let ssa_parameters = List.map ssa_parameters ~f:(fun ssa -> (mk_ident ssa, Typ.value)) in
  {Textual.Node.label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc}


let mk_procdesc proc_kind
    {CFG.entry; nodes; code_info= {co_firstlineno; co_argcount; co_varnames} as code_info} =
  let loc = Textual.Location.known ~line:co_firstlineno ~col:(-1) in
  let procdecl = mk_procdecl ~loc proc_kind code_info in
  let is_module_body = is_module_body proc_kind in
  let nodes_bindings = NodeName.Map.bindings nodes in
  let nullify_locals loc last instrs =
    let nb_vars = Array.length co_varnames in
    if co_argcount >= nb_vars then instrs
    else
      match (last : Textual.Terminator.t) with
      | Ret _ ->
          let args =
            exp_locals
            :: List.init (nb_vars - co_argcount) ~f:(fun i ->
                   let name = co_varnames.(i + co_argcount) in
                   exp_of_ident_str name )
          in
          instrs @ [Textual.Instr.Let {id= None; exp= call_builtin "py_nullify_locals" args; loc}]
      | _ ->
          instrs
  in
  let nodes =
    List.map nodes_bindings ~f:(fun (_node_name, node) ->
        of_node is_module_body nullify_locals entry node )
  in
  let start = mk_node_name entry in
  let params =
    if is_module_body then [Parameter.globals] else [Parameter.globals; Parameter.locals]
  in
  let locals =
    if is_module_body then [(Parameter.locals, Textual.Typ.mk_without_attributes Typ.locals)]
    else []
  in
  let exit_loc =
    let last_loc =
      List.fold nodes_bindings ~init:None ~f:(fun acc (_, {Node.last_loc}) ->
          match Location.line last_loc with
          | None ->
              acc
          | Some line ->
              Some (Option.value_map acc ~default:line ~f:(fun acc_line -> Int.max acc_line line)) )
    in
    location_from_opt_line last_loc
  in
  {Textual.ProcDesc.procdecl; nodes; start; params; locals; exit_loc}


let mk_module {Module.name; toplevel; functions} =
  let filename =
    F.asprintf "%a.py" Ident.pp name |> String.substr_replace_all ~pattern:"::" ~with_:"/"
  in
  let sourcefile = Textual.SourceFile.create filename in
  let decls =
    List.map (QualName.Map.bindings functions) ~f:(fun (qual_name, cfg) ->
        Textual.Module.Proc (mk_procdesc (RegularFunction qual_name) cfg) )
  in
  let decls = Textual.Module.Proc (mk_procdesc (ModuleBody name) toplevel) :: decls in
  let attrs = [Textual.Attr.mk_source_language Python] in
  {Textual.Module.attrs; decls; sourcefile}


module DefaultType : sig
  type decl =
    | Class of {name: string; target: string}
    | Import of {name: string; target: string}
    | ImportFrom of {module_name: string; attr_name: string; target: string}
    | Fundef of {typ: Textual.Typ.t; target: string}

  type acc

  val empty : acc

  val add_decl : decl -> acc -> acc

  val add_allocate : Textual.Ident.t -> Textual.Typ.t -> acc -> acc

  val add_class_body : Textual.Ident.t -> string -> acc -> acc

  val add_fun_ptr : Textual.Ident.t -> Textual.Typ.t -> acc -> acc

  val add_import : Textual.Ident.t -> string -> acc -> acc

  val add_import_from : Textual.Ident.t -> module_name:string -> attr_name:string -> acc -> acc

  val add_string_constant : Textual.Ident.t -> string -> acc -> acc

  val add_tuple : Textual.Ident.t -> acc -> acc

  val get_allocate : Textual.Ident.t -> acc -> Textual.Typ.t option

  val get_class_body : Textual.Ident.t -> acc -> string option

  val get_fun_ptr : Textual.Ident.t -> acc -> Textual.Typ.t option

  val get_import : Textual.Ident.t -> acc -> string option

  val get_import_from : Textual.Ident.t -> acc -> (string * string) option

  val get_string_constant : Textual.Ident.t -> acc -> string option

  val is_allocate : Textual.Ident.t -> acc -> bool

  val is_class_body : Textual.Ident.t -> acc -> bool

  val is_fun_ptr : Textual.Ident.t -> acc -> bool

  val is_import : Textual.Ident.t -> acc -> bool

  val is_import_from : Textual.Ident.t -> acc -> bool

  val is_string_constant : Textual.Ident.t -> acc -> bool

  val is_tuple : Textual.Ident.t -> acc -> bool

  val export : acc -> decl list
end = struct
  type decl =
    | Class of {name: string; target: string}
    | Import of {name: string; target: string}
    | ImportFrom of {module_name: string; attr_name: string; target: string}
    | Fundef of {typ: Textual.Typ.t; target: string}

  type exp =
    | Tuple (* we just need to remember if a value must-be a tuple *)
    | Allocate of Textual.Typ.t
    | ClassBody of string
    | FuncPtr of Textual.Typ.t
    | Import of string
    | ImportFrom of {module_name: string; attr_name: string}
    | StringConstant of string

  type acc = {default_type: decl list; exps: exp Textual.Ident.Map.t}

  let empty = {default_type= []; exps= Textual.Ident.Map.empty}

  let add_decl decl ({default_type} as acc) = {acc with default_type= decl :: default_type}

  let add_allocate ident typ ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (Allocate typ) exps}


  let add_fun_ptr ident typ ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (FuncPtr typ) exps}


  let add_import ident str ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (Import str) exps}


  let add_import_from ident ~module_name ~attr_name ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (ImportFrom {module_name; attr_name}) exps}


  let add_class_body ident str ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (ClassBody str) exps}


  let add_string_constant ident str ({exps} as acc) =
    {acc with exps= Textual.Ident.Map.add ident (StringConstant str) exps}


  let add_tuple ident ({exps} as acc) = {acc with exps= Textual.Ident.Map.add ident Tuple exps}

  let is_allocate ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (Allocate _) -> true | _ -> false


  let is_class_body ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (ClassBody _) -> true | _ -> false


  let is_fun_ptr ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (FuncPtr _) -> true | _ -> false


  let is_import ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (Import _) -> true | _ -> false


  let is_import_from ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (ImportFrom _) -> true | _ -> false


  let is_string_constant ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (StringConstant _) -> true | _ -> false


  let is_tuple ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some Tuple -> true | _ -> false


  let get_allocate ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (Allocate typ) -> Some typ | _ -> None


  let get_class_body ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (ClassBody str) -> Some str | _ -> None


  let get_fun_ptr ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (FuncPtr typ) -> Some typ | _ -> None


  let get_import ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with Some (Import str) -> Some str | _ -> None


  let get_import_from ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with
    | Some (ImportFrom {module_name; attr_name}) ->
        Some (module_name, attr_name)
    | _ ->
        None


  let get_string_constant ident {exps} =
    match Textual.Ident.Map.find_opt ident exps with
    | Some (StringConstant str) ->
        Some str
    | _ ->
        None


  let export {default_type} = List.rev default_type
end

let py_import_name = builtin_qual_proc_name str_py_import_name

let py_import_from = builtin_qual_proc_name str_py_import_from

let py_build_class = builtin_qual_proc_name str_py_build_class

let py_build_tuple = builtin_qual_proc_name "py_build_tuple"

let py_call = builtin_qual_proc_name str_py_call

let py_store_name = builtin_qual_proc_name str_py_store_name

let py_make_function = builtin_qual_proc_name str_py_make_function

let py_make_string = builtin_qual_proc_name str_py_make_string

let sil_allocate : Textual.QualifiedProcName.t =
  {enclosing_class= TopLevel; name= Textual.ProcName.of_string "__sil_allocate"}


let root_of_name str =
  String.substr_index str ~pattern:"::"
  |> Option.value_map ~default:str ~f:(fun pos ->
         let package_name = String.sub ~pos:0 ~len:pos str in
         package_name ^ "::__init__" )


let gen_type module_name ~allow_classes name node =
  let mk_class_companion str = Typ.class_companion module_name str in
  let mk_module_attribute_name module_name attr_name = Typ.module_attribute module_name attr_name in
  let open Textual in
  let rec find_next_declaration acc = function
    | Instr.Let {id= Some ident; exp= Call {proc}} :: instrs
      when QualifiedProcName.equal py_build_tuple proc ->
        let acc = DefaultType.add_tuple ident acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= _ :: Const (Str name) :: Var id_from :: _}}
      :: instrs
      when QualifiedProcName.equal py_import_name proc ->
        let is_from_import = DefaultType.is_tuple id_from acc in
        let name = if is_from_import then name else root_of_name name in
        let acc = DefaultType.add_import ident name acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Const (Str attr_name); Var id_module]}}
      :: instrs
      when QualifiedProcName.equal py_import_from proc && DefaultType.is_import id_module acc ->
        let module_name = DefaultType.get_import id_module acc |> Option.value_exn in
        let acc = DefaultType.add_import_from ident ~module_name ~attr_name acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Const (Str attr_name); Var id_from]}}
      :: instrs
      when QualifiedProcName.equal py_import_from proc && DefaultType.is_import_from id_from acc ->
        let module_name, attr_name0 = DefaultType.get_import_from id_from acc |> Option.value_exn in
        let acc =
          String.chop_suffix module_name ~suffix:"__init__"
          |> Option.value_map ~default:acc ~f:(fun module_name ->
                 let module_name = module_name ^ attr_name0 in
                 DefaultType.add_import_from ident ~module_name ~attr_name acc )
        in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Typ typ]}} :: instrs
      when QualifiedProcName.equal sil_allocate proc ->
        let acc = DefaultType.add_allocate ident typ acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= Var ident_allocate :: _}} :: instrs
      when QualifiedProcName.equal py_make_function proc
           && DefaultType.is_allocate ident_allocate acc ->
        let typ = DefaultType.get_allocate ident_allocate acc |> Option.value_exn in
        let acc = DefaultType.add_fun_ptr ident typ acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Const (Str str)]}} :: instrs
      when QualifiedProcName.equal py_make_string proc ->
        let acc = DefaultType.add_string_constant ident str acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [_; Var ident_str_const]}} :: instrs
      when QualifiedProcName.equal py_build_class proc
           && DefaultType.is_string_constant ident_str_const acc ->
        let name = DefaultType.get_string_constant ident_str_const acc |> Option.value_exn in
        let acc = DefaultType.add_class_body ident name acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Var _id_decorator; _; Var id_fun_ptr]}}
      :: instrs
      when QualifiedProcName.equal py_call proc && DefaultType.is_fun_ptr id_fun_ptr acc ->
        (* note: we could filter the decorator in id_decorator if needed *)
        let typ = DefaultType.get_fun_ptr id_fun_ptr acc |> Option.value_exn in
        let acc = DefaultType.add_fun_ptr ident typ acc in
        find_next_declaration acc instrs
    | Instr.Let {id= Some ident; exp= Call {proc; args= [Var _id_decorator; _; Var id_class]}}
      :: instrs
      when QualifiedProcName.equal py_call proc && DefaultType.is_class_body id_class acc ->
        (* note: we could filter the decorator in id_decorator if needed *)
        let name = DefaultType.get_class_body id_class acc |> Option.value_exn in
        let acc = DefaultType.add_class_body ident name acc in
        find_next_declaration acc instrs
    | Instr.Let {exp= Call {proc; args= [Const (Str target); _; _; Var ident]}} :: instrs
      when QualifiedProcName.equal py_store_name proc && DefaultType.is_fun_ptr ident acc ->
        let typ = DefaultType.get_fun_ptr ident acc |> Option.value_exn in
        let acc = DefaultType.add_decl (Fundef {typ; target}) acc in
        find_next_declaration acc instrs
    | Instr.Let {exp= Call {proc; args= [Const (Str target); _; _; Var ident]}} :: instrs
      when QualifiedProcName.equal py_store_name proc && DefaultType.is_import ident acc ->
        let name = DefaultType.get_import ident acc |> Option.value_exn in
        let acc = DefaultType.add_decl (Import {name; target}) acc in
        find_next_declaration acc instrs
    | Instr.Let {exp= Call {proc; args= [Const (Str target); _; _; Var ident]}} :: instrs
      when QualifiedProcName.equal py_store_name proc && DefaultType.is_import_from ident acc ->
        let module_name, attr_name = DefaultType.get_import_from ident acc |> Option.value_exn in
        let acc = DefaultType.add_decl (ImportFrom {module_name; attr_name; target}) acc in
        find_next_declaration acc instrs
    | Instr.Let {exp= Call {proc; args= [Const (Str target); _; _; Var ident]}} :: instrs
      when QualifiedProcName.equal py_store_name proc && DefaultType.is_class_body ident acc ->
        let name = DefaultType.get_class_body ident acc |> Option.value_exn in
        let acc = DefaultType.add_decl (Class {name; target}) acc in
        find_next_declaration acc instrs
    | _ :: instrs ->
        find_next_declaration acc instrs
    | [] ->
        DefaultType.export acc
  in
  let decls = find_next_declaration DefaultType.empty node in
  let mk_fieldname str : qualified_fieldname =
    {enclosing_class= name; name= FieldName.of_string str}
  in
  let fields =
    List.filter_map decls ~f:(fun (decl : DefaultType.decl) ->
        match decl with
        | Class {name; target} when allow_classes ->
            Some
              { FieldDecl.qualified_name= mk_fieldname target
              ; typ= mk_class_companion name
              ; attributes= [] }
        | Class _ ->
            None
        | Import {name; target} ->
            Some
              { FieldDecl.qualified_name= mk_fieldname target
              ; typ= global_type_of_str name
              ; attributes= [] }
        | ImportFrom {module_name; attr_name; target} ->
            Some
              { FieldDecl.qualified_name= mk_fieldname target
              ; typ= mk_module_attribute_name module_name attr_name
              ; attributes= [] }
        | Fundef {typ; target} ->
            Some {FieldDecl.qualified_name= mk_fieldname target; typ= Typ.Ptr typ; attributes= []} )
  in
  let classes =
    List.filter_map decls ~f:(fun (decl : DefaultType.decl) ->
        match decl with Class {name} -> Some name | _ -> None )
  in
  ({Struct.name; supers= []; fields; attributes= []}, classes)


let gen_module_default_type {Textual.Module.decls} =
  let open IOption.Let_syntax in
  let module_body_name = Textual.ProcName.of_string str_module_body in
  let opt, classes_map =
    List.fold decls ~init:(None, Textual.ProcName.Map.empty) ~f:(fun (opt, map) decl ->
        match decl with
        | Textual.Module.Proc
            { procdecl= {qualified_name= {enclosing_class= Enclosing module_name; name}}
            ; start
            ; nodes }
          when Textual.ProcName.equal name module_body_name ->
            let opt =
              List.find nodes ~f:(fun node -> Textual.NodeName.equal start node.Textual.Node.label)
              |> Option.map ~f:(fun node -> (module_name, node.Textual.Node.instrs))
            in
            (opt, map)
        | Textual.Module.Proc {procdecl= {qualified_name= {name}}; start; nodes} ->
            let map =
              List.find nodes ~f:(fun node -> Textual.NodeName.equal start node.Textual.Node.label)
              |> Option.value_map ~default:map ~f:(fun node ->
                     Textual.ProcName.Map.add name node.Textual.Node.instrs map )
            in
            (opt, map)
        | _ ->
            (opt, map) )
  in
  let* module_name, module_body = opt in
  let name =
    F.asprintf "PyGlobals::%a" Textual.TypeName.pp module_name |> Textual.TypeName.of_string
  in
  let default_type, classes = gen_type module_name ~allow_classes:true name module_body in
  let* other_type_decls =
    List.fold classes ~init:(Some []) ~f:(fun decls name ->
        let* decls in
        let proc_name = Textual.ProcName.of_string name in
        let+ body = Textual.ProcName.Map.find_opt proc_name classes_map in
        let type_name = Typ.class_companion_name module_name name in
        let type_decl, _ = gen_type module_name ~allow_classes:false type_name body in
        Textual.Module.Struct type_decl :: decls )
  in
  Some (Textual.Module.Struct default_type :: other_type_decls)


let add_module_default_type textual =
  let open Textual.Module in
  gen_module_default_type textual
  |> Option.value_map ~default:textual ~f:(fun extra_type_decls ->
         {textual with decls= extra_type_decls @ textual.decls} )
