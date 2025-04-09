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

  let ident_to_typename ident =
    let name = Ident.to_textual_base_type_name ident in
    {Textual.TypeName.name; args= []}


  let globals module_name =
    let name = Textual.BaseTypeName.of_string "PyGlobals" in
    let args = [ident_to_typename module_name] in
    Textual.(Typ.Ptr (Typ.Struct {TypeName.name; args}))


  let class_companion_name module_name name =
    let args = [module_name; Textual.TypeName.of_string name] in
    let name = Textual.BaseTypeName.of_string "PyClassCompanion" in
    {Textual.TypeName.name; args}


  let class_companion module_name name =
    Textual.(Typ.Ptr (Typ.Struct (class_companion_name module_name name)))


  let module_attribute module_name attr_name =
    let name = Textual.BaseTypeName.of_string "PyModuleAttr" in
    let args = Textual.TypeName.[of_string module_name; of_string attr_name] in
    Textual.(Typ.Ptr (Typ.Struct {TypeName.name; args}))


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
  | InvalidUnicode ->
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
  | LoadFastCheck {name} ->
      call_builtin "py_load_fast_check" [exp_of_ident_str name; exp_locals]
  | LoadFastAndClear {name} ->
      call_builtin "py_load_fast_and_clear" [exp_of_ident_str name; exp_locals]
  | LoadLocals ->
      call_builtin "py_load_locals" [exp_locals]
  | LoadFromDictOrDeref {slot; mapping} ->
      let slot = Textual.(Exp.Const (Const.Int (Z.of_int slot))) in
      call_builtin "py_load_from_dict_or_deref" [slot; of_exp mapping]
  | LoadSuperAttr {attr; super; class_; self} ->
      call_builtin "py_load_super_attr"
        [exp_of_ident_str attr; of_exp super; of_exp class_; of_exp self]
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
      let str_qual_name = F.asprintf "%a" QualName.pp qual_name in
      let attr : Textual.Attr.t =
        {name= "name"; values= [str_qual_name]; loc= Textual.Location.Unknown}
      in
      let closure =
        Textual.Exp.Closure
          {proc; captured= [exp_globals]; params= [Parameter.locals]; attributes= [attr]}
      in
      call_builtin str_py_make_function
        ( closure
        :: List.map ~f:of_exp [default_values; default_values_kw; annotations; cells_for_closure] )


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
      let exp = call_builtin "py_bool" [of_exp exp] in
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
  | Format ->
      "py_format"
  | FormatFn Str ->
      "py_format_fn_str"
  | FormatFn Repr ->
      "py_format_fn_repr"
  | FormatFn Ascii ->
      "py_format_fn_ascii"
  | Inplace op ->
      F.asprintf "py_inplace_%s" (binary_op_name op)
  | Binary op ->
      F.asprintf "py_binary_%s" (binary_op_name op)
  | BinarySlice ->
      "py_binary_slice"
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
  | UnaryIntrinsic PrintExpr ->
      "py_print_expr"
  | UnaryIntrinsic ImportStar ->
      "py_import_star"
  | UnaryIntrinsic StopiterationError ->
      "stopiteration_error"
  | UnaryIntrinsic AsyncGenValueWrapperNew ->
      "py_async_gen_value_wrapper_new"
  | UnaryIntrinsic UnaryPos ->
      "py_unary_pos"
  | UnaryIntrinsic ListToTuple ->
      "py_list_to_tuple"
  | UnaryIntrinsic MakeTypevar ->
      "make_typevar"
  | UnaryIntrinsic MakeParamspec ->
      "py_make_paramspec"
  | UnaryIntrinsic MakeTypevartuple ->
      "py_make_typevartuple"
  | UnaryIntrinsic SubscriptGeneric ->
      "py_subscript_generic"
  | UnaryIntrinsic MakeTypealias ->
      "py_make_typealias"
  | BinaryIntrinsic PrepReraiseStar ->
      "py_prep_reraise_star"
  | BinaryIntrinsic TypevarWithBound ->
      "py_typevar_with_bound"
  | BinaryIntrinsic TypevarWithConstraints ->
      "py_typevar_with_constraints"
  | BinaryIntrinsic SetFunctionTypeParams ->
      "py_set_function_type_params"


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
  | StoreSlice {container; start; end_; rhs} ->
      Let
        { id= None
        ; exp=
            call_builtin "py_store_slice" [of_exp container; of_exp start; of_exp end_; of_exp rhs]
        ; loc }
  | StoreSubscript {lhs; index; rhs} ->
      Let
        { id= None
        ; exp= call_builtin "py_store_subscript" [of_exp lhs; of_exp index; of_exp rhs]
        ; loc }
  | Call {lhs; exp; args; arg_names} ->
      let args = of_exp exp :: of_exp arg_names :: List.map ~f:of_exp args in
      Let {id= Some (mk_ident lhs); exp= call_builtin str_py_call args; loc}
  | CallEx {lhs; exp; kargs; arg_names} ->
      let args = [of_exp exp; of_exp kargs; of_exp arg_names] in
      Let {id= Some (mk_ident lhs); exp= call_builtin "py_call_function_ex" args; loc}
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
  | MakeCell i ->
      let arg = Textual.(Exp.Const (Const.Int (Z.of_int i))) in
      Let {id= None; exp= call_builtin "py_make_cell" [arg]; loc}
  | CopyFreeVars i ->
      let arg = Textual.(Exp.Const (Const.Int (Z.of_int i))) in
      Let {id= None; exp= call_builtin "py_copy_free_vars" [arg]; loc}
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
  | Yield {lhs; rhs} ->
      Let {id= Some (mk_ident lhs); exp= call_builtin "py_yield" [of_exp rhs]; loc}


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


let pyir_type_to_textual module_name struct_types =
  let mk_global_typename str =
    let open Textual in
    let name = TypeName.of_string str in
    {TypeName.name= BaseTypeName.of_string "PyGlobals"; args= [name]}
  in
  let mk_closure_typename str =
    let open Textual in
    let typename = TypeName.of_string_no_dot_escape str in
    Typ.(Ptr (Struct {TypeName.name= BaseTypeName.of_string "PyClosure"; args= [typename]}))
  in
  let module_name = F.asprintf "%a" PyIR.Ident.pp module_name |> Textual.TypeName.of_string in
  let mk_module_attribute_name module_name attr_name = Typ.module_attribute module_name attr_name in
  List.map struct_types ~f:(fun {PyIRTypeInference.name; kind; fields} ->
      let name =
        match kind with
        | Global ->
            mk_global_typename name
        | ClassCompanion ->
            Typ.class_companion_name module_name name
      in
      let mk_fieldname str : Textual.qualified_fieldname =
        Textual.{enclosing_class= name; name= FieldName.of_string str}
      in
      let fields =
        List.map fields ~f:(fun {PyIRTypeInference.name; typ} : Textual.FieldDecl.t ->
            match typ with
            | Class {class_name} ->
                { qualified_name= mk_fieldname class_name
                ; typ= Typ.class_companion module_name class_name
                ; attributes= [] }
            | Import {module_name} ->
                { qualified_name= mk_fieldname name
                ; typ= global_type_of_str module_name
                ; attributes= [] }
            | ImportFrom {module_name; attr_name} ->
                { qualified_name= mk_fieldname name
                ; typ= mk_module_attribute_name module_name attr_name
                ; attributes= [] }
            | Fundef {qual_name} ->
                let typ = mk_closure_typename qual_name in
                {qualified_name= mk_fieldname name; typ; attributes= []} )
      in
      Textual.(Module.Struct {name; supers= []; fields; attributes= []}) )


let add_pyir_type types ~module_name ({Textual.Module.decls} as textual) =
  let open Textual.Module in
  let extra_type_decls = pyir_type_to_textual module_name types in
  {textual with decls= extra_type_decls @ decls}
