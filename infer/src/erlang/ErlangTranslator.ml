(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module Block = ErlangBlock
module Env = ErlangEnvironment
module L = Logging
module Node = ErlangNode

let maps_is_key = Procname.make_erlang ~module_name:"maps" ~function_name:"is_key" ~arity:2

let maps_put = Procname.make_erlang ~module_name:"maps" ~function_name:"put" ~arity:3

let maps_get = Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:2

let lists_append2 = Procname.make_erlang ~module_name:"lists" ~function_name:"append" ~arity:2

let lists_reverse = Procname.make_erlang ~module_name:"lists" ~function_name:"reverse" ~arity:1

let erlang_send2 = Procname.make_erlang ~module_name:"erlang" ~function_name:"send" ~arity:2

let mangled_arg (n : int) : Mangled.t = Mangled.from_string (Printf.sprintf "$arg%d" n)

let any_typ = Env.ptr_typ_of_name Any

let mk_fresh_id () = Ident.create_fresh Ident.knormal

let ( |~~> ) = ErlangBlock.( |~~> )

let update_location (loc : Ast.location) (env : (_, _) Env.t) =
  let location = {env.location with line= loc.line; col= loc.col} in
  {env with location}


let has_type (env : (_, _) Env.t) ~result ~value (name : ErlangTypeName.t) : Sil.instr =
  let fun_exp : Exp.t = Const (Cfun BuiltinDecl.__instanceof) in
  let args : (Exp.t * Typ.t) list =
    [ (value, any_typ)
    ; ( Sizeof
          { typ= Env.typ_of_name name
          ; nbytes= None
          ; dynamic_length= None
          ; subtype= Subtype.subtypes_instof }
      , any_typ ) ]
  in
  Call ((result, Typ.mk (Tint IBool)), fun_exp, args, env.location, CallFlags.default)


let check_type env value typ : Block.t =
  let is_right_type_id = mk_fresh_id () in
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value:(Var value) typ] in
  let exit_success = Node.make_if env true (Var is_right_type_id) in
  let exit_failure = Node.make_if env false (Var is_right_type_id) in
  start |~~> [exit_success; exit_failure] ;
  {start; exit_success; exit_failure}


(** into_id=expr.field_name *)
let load_field_from_expr (env : (_, _) Env.t) into_id expr field_name typ : Sil.instr =
  let field = Fieldname.make (ErlangType typ) field_name in
  Load
    { id= into_id
    ; e= Lfield (expr, field, Env.typ_of_name typ)
    ; root_typ= any_typ
    ; typ= any_typ
    ; loc= env.location }


(** into_id=value_id.field_name *)
let load_field_from_id (env : (_, _) Env.t) into_id value_id field_name typ : Sil.instr =
  load_field_from_expr env into_id (Var value_id) field_name typ


let mk_atom_call (env : (_, _) Env.t) ret_var atom =
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_atom) in
  let args =
    let value_exp = Exp.Const (Cstr atom) in
    let hash = ErlangTypeName.calculate_hash atom in
    let hash_exp = Exp.Const (Cint (IntLit.of_int hash)) in
    [(value_exp, any_typ); (hash_exp, any_typ)]
  in
  Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)


(** Boxes a SIL integer expression (interpreted as a Boolean) into an Erlang atom (true/false). *)
let box_bool env into_id expr : Block.t =
  let start = Node.make_nop env in
  let exit_success = Node.make_nop env in
  let true_branch = Node.make_if env true expr in
  let false_branch = Node.make_if env false expr in
  let mk_true_atom = Node.make_stmt env [mk_atom_call env into_id ErlangTypeName.atom_true] in
  let mk_false_atom = Node.make_stmt env [mk_atom_call env into_id ErlangTypeName.atom_false] in
  start |~~> [true_branch; false_branch] ;
  true_branch |~~> [mk_true_atom] ;
  false_branch |~~> [mk_false_atom] ;
  mk_true_atom |~~> [exit_success] ;
  mk_false_atom |~~> [exit_success] ;
  {start; exit_success; exit_failure= Node.make_nop env}


(** Unboxes an Erlang atom (true/false) into a SIL integer expression (1/0). Currently we do not
    check for type errors: we assume that we get an atom and we treat anything besides "true" as if
    it was "false". But if we do plan to check for such errors, this is the place to do so. *)
let unbox_bool env expr : Exp.t * Block.t =
  let is_atom = mk_fresh_id () in
  let start = Node.make_stmt env [has_type env ~result:is_atom ~value:expr Atom] in
  let prune_node = Node.make_if env true (Var is_atom) in
  let hash_id = mk_fresh_id () in
  let load =
    Node.make_stmt env [load_field_from_expr env hash_id expr ErlangTypeName.atom_hash Atom]
  in
  let check_hash_expr =
    let hash = ErlangTypeName.calculate_hash ErlangTypeName.atom_true in
    let hash_expr = Exp.Const (Cint (IntLit.of_int hash)) in
    Exp.BinOp (Eq, Var hash_id, hash_expr)
  in
  start |~~> [prune_node] ;
  prune_node |~~> [load] ;
  (check_hash_expr, {start; exit_success= load; exit_failure= Node.make_nop env})


(** Main entry point for patterns. Result is a block where if the pattern-match succeeds, the
    [exit_success] node is reached and the pattern variables are storing the corresponding values;
    otherwise, the [exit_failure] node is reached. *)
let rec translate_pattern env (value : Ident.t) {Ast.location; simple_expression} : Block.t =
  let env = update_location location env in
  match simple_expression with
  | Cons {head; tail} ->
      translate_pattern_cons env value head tail
  | Literal (Atom atom) ->
      translate_pattern_literal_atom env value atom
  | Literal (Int i) ->
      let e = Exp.Const (Cint (IntLit.of_string i)) in
      Block.make_branch env (Exp.BinOp (Eq, Var value, e))
  | Literal (String s) ->
      translate_pattern_literal_string env value s
  | Map {updates; _} ->
      translate_pattern_map env value updates
  | Match {pattern; body} ->
      translate_pattern_match env value pattern body
  | Nil ->
      translate_pattern_nil env value
  | RecordIndex {name; field} ->
      translate_pattern_record_index env value name field
  | RecordUpdate {name; updates; _} ->
      translate_pattern_record_update env value name updates
  | Tuple exprs ->
      translate_pattern_tuple env value exprs
  | UnaryOperator _ ->
      translate_pattern_unary_expression env value location simple_expression
  | Variable {vname; scope} ->
      translate_pattern_variable env value vname scope
  | e ->
      (* TODO: Cover all cases. *)
      L.debug Capture Verbose "@[todo ErlangTranslator.translate_pattern %s@."
        (Sexp.to_string (Ast.sexp_of_simple_expression e)) ;
      Block.all env [Block.make_unsupported env; Block.make_failure env]


and translate_pattern_cons env value head tail : Block.t =
  let head_value = mk_fresh_id () in
  let tail_value = mk_fresh_id () in
  let head_load = load_field_from_id env head_value value ErlangTypeName.cons_head Cons in
  let tail_load = load_field_from_id env tail_value value ErlangTypeName.cons_tail Cons in
  let unpack_node = Node.make_stmt env [head_load; tail_load] in
  let head_matcher = translate_pattern env head_value head in
  let tail_matcher = translate_pattern env tail_value tail in
  let submatcher = Block.all env [head_matcher; tail_matcher] in
  let exit_failure = Node.make_nop env in
  let type_checker = check_type env value Cons in
  type_checker.exit_success |~~> [unpack_node] ;
  unpack_node |~~> [submatcher.start] ;
  type_checker.exit_failure |~~> [exit_failure] ;
  submatcher.exit_failure |~~> [exit_failure] ;
  {start= type_checker.start; exit_success= submatcher.exit_success; exit_failure}


and translate_pattern_literal_atom (env : (_, _) Env.t) value atom : Block.t =
  (* We create a temporary atom and read back its hash. We could just directly compute
     the hash, but this way we have better readability for the CFG and pulse results,
     because it is not just a hash appearing there, but also the atom as a string. *)
  let expected_id = mk_fresh_id () in
  let expected_block : Block.t = translate_expression_literal_atom env expected_id atom in
  let actual_hash = mk_fresh_id () in
  let load_actual = load_field_from_id env actual_hash value ErlangTypeName.atom_hash Atom in
  let expected_hash = mk_fresh_id () in
  let load_expected =
    load_field_from_id env expected_hash expected_id ErlangTypeName.atom_hash Atom
  in
  let load_node = Node.make_stmt env [load_actual; load_expected] in
  let hash_checker = Block.make_branch env (Exp.BinOp (Eq, Var actual_hash, Var expected_hash)) in
  let type_checker = check_type env value Atom in
  let exit_failure = Node.make_nop env in
  type_checker.exit_success |~~> [expected_block.start] ;
  expected_block.exit_failure |~~> [exit_failure] ;
  expected_block.exit_success |~~> [load_node] ;
  load_node |~~> [hash_checker.start] ;
  type_checker.exit_failure |~~> [exit_failure] ;
  hash_checker.exit_failure |~~> [exit_failure] ;
  {start= type_checker.start; exit_success= hash_checker.exit_success; exit_failure}


and translate_pattern_literal_string (env : (_, _) Env.t) value s : Block.t =
  let expected_id = mk_fresh_id () in
  let expected_block : Block.t = translate_expression_literal_string env expected_id s in
  let equals_id = mk_fresh_id () in
  (* TODO: add Pulse model for this function T93361792 *)
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_str_equal) in
  let call_block =
    let args = [(Exp.Var value, any_typ); (Exp.Var expected_id, any_typ)] in
    let instr = Sil.Call ((equals_id, any_typ), fun_exp, args, env.location, CallFlags.default) in
    Block.make_instruction env [instr]
  in
  let checker_block = Block.make_branch env (Var equals_id) in
  Block.all env [expected_block; call_block; checker_block]


and translate_pattern_nil env value : Block.t = check_type env value Nil

and translate_pattern_match (env : (_, _) Env.t) value pattern body : Block.t =
  (* A pattern like [P1 = P2] means comparing both P1 and P2 against the value.
     We might be tempted to match P1 against P2 and the value, but P2 is
     also a pattern and can have unbound variables. For example,
     [(A = B) = 2] should read as [A = 2, B = 2] and not [A = B, A = 2]
     because B can be unbound. *)
  let subpattern_block = translate_pattern env value pattern in
  let body_block = translate_pattern env value body in
  Block.all env [body_block; subpattern_block]


and translate_pattern_map (env : (_, _) Env.t) value updates : Block.t =
  (* For each update, check if key is there and if yes, match against value *)
  let make_submatcher (one_update : Ast.association) =
    let key_id, key_expr_block = translate_expression_to_fresh_id env one_update.key in
    let args = [(Exp.Var key_id, any_typ); (Exp.Var value, any_typ)] in
    let has_key_block =
      let has_key_id = mk_fresh_id () in
      let has_key_fun_exp = Exp.Const (Cfun maps_is_key) in
      let call_block =
        Block.make_instruction env
          [Sil.Call ((has_key_id, any_typ), has_key_fun_exp, args, env.location, CallFlags.default)]
      in
      let unboxed, unbox_block = unbox_bool env (Exp.Var has_key_id) in
      let check_block : Block.t =
        let start = Node.make_nop env in
        let exit_success = Node.make_if env true unboxed in
        let exit_failure = Node.make_if env false unboxed in
        start |~~> [exit_success; exit_failure] ;
        {start; exit_success; exit_failure}
      in
      Block.all env [call_block; unbox_block; check_block]
    in
    let value_id = mk_fresh_id () in
    let lookup_fun_exp = Exp.Const (Cfun maps_get) in
    let lookup_block =
      Block.make_instruction env
        [Sil.Call ((value_id, any_typ), lookup_fun_exp, args, env.location, CallFlags.default)]
    in
    let match_value_block = translate_pattern env value_id one_update.value in
    Block.all env [key_expr_block; has_key_block; lookup_block; match_value_block]
  in
  let type_checker = check_type env value Map in
  let submatchers = Block.all env (List.map ~f:make_submatcher updates) in
  type_checker.exit_success |~~> [submatchers.start] ;
  type_checker.exit_failure |~~> [submatchers.exit_failure] ;
  { start= type_checker.start
  ; exit_success= submatchers.exit_success
  ; exit_failure= submatchers.exit_failure }


and translate_pattern_record_index (env : (_, _) Env.t) value name field : Block.t =
  let record_info = String.Map.find_exn env.records name in
  let field_info = String.Map.find_exn record_info.field_info field in
  let index_expr = Exp.Const (Cint (IntLit.of_int field_info.index)) in
  Block.make_branch env (Exp.BinOp (Eq, Var value, index_expr))


and match_record_name env value name (record_info : Env.record_info) : Block.t =
  let tuple_size = 1 + List.length record_info.field_names in
  let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
  let type_checker = check_type env value tuple_typ in
  let name_id = mk_fresh_id () in
  let name_load = load_field_from_id env name_id value (ErlangTypeName.tuple_elem 1) tuple_typ in
  let unpack_node = Node.make_stmt env [name_load] in
  let name_checker = translate_pattern_literal_atom env name_id name in
  let exit_failure = Node.make_nop env in
  type_checker.exit_success |~~> [unpack_node] ;
  unpack_node |~~> [name_checker.start] ;
  type_checker.exit_failure |~~> [exit_failure] ;
  name_checker.exit_failure |~~> [exit_failure] ;
  {start= type_checker.start; exit_success= name_checker.exit_success; exit_failure}


and translate_pattern_record_update (env : (_, _) Env.t) value name updates : Block.t =
  let record_info = String.Map.find_exn env.records name in
  (* Match the type and the record name *)
  let record_name_matcher = match_record_name env value name record_info in
  (* Match each specified field *)
  let tuple_size = 1 + List.length record_info.field_names in
  let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
  let make_one_field_matcher (one_update : Ast.record_update) =
    match one_update.field with
    | Some name ->
        let field_info = String.Map.find_exn record_info.field_info name in
        let value_id = mk_fresh_id () in
        let tuple_elem = ErlangTypeName.tuple_elem field_info.index in
        let load_instr = load_field_from_id env value_id value tuple_elem tuple_typ in
        let unpack_node = Node.make_stmt env [load_instr] in
        let submatcher = translate_pattern env value_id one_update.expression in
        unpack_node |~~> [submatcher.start] ;
        { Block.start= unpack_node
        ; exit_success= submatcher.exit_success
        ; exit_failure= submatcher.exit_failure }
    | None ->
        Block.make_success env
  in
  let record_field_matchers = List.map ~f:make_one_field_matcher updates in
  Block.all env (record_name_matcher :: record_field_matchers)


and translate_pattern_tuple env value exprs : Block.t =
  let tuple_typ : ErlangTypeName.t = Tuple (List.length exprs) in
  let type_checker = check_type env value tuple_typ in
  let value_ids = List.map ~f:(function _ -> mk_fresh_id ()) exprs in
  let field_names = ErlangTypeName.tuple_field_names (List.length exprs) in
  let load_instructions =
    List.map
      ~f:(function
        | one_value, one_field -> load_field_from_id env one_value value one_field tuple_typ )
      (List.zip_exn value_ids field_names)
  in
  let unpack_node = Node.make_stmt env load_instructions in
  let matchers =
    List.map
      ~f:(function one_expr, one_value -> translate_pattern env one_value one_expr)
      (List.zip_exn exprs value_ids)
  in
  let submatcher = Block.all env matchers in
  let exit_failure = Node.make_nop env in
  type_checker.exit_success |~~> [unpack_node] ;
  unpack_node |~~> [submatcher.start] ;
  type_checker.exit_failure |~~> [exit_failure] ;
  submatcher.exit_failure |~~> [exit_failure] ;
  {start= type_checker.start; exit_success= submatcher.exit_success; exit_failure}


and translate_pattern_unary_expression (env : (_, _) Env.t) value location simple_expression :
    Block.t =
  (* Unary op pattern must evaluate to number, so just delegate to expression translation *)
  let id, expr_block = translate_expression_to_fresh_id env {Ast.location; simple_expression} in
  let branch_block = Block.make_branch env (Exp.BinOp (Eq, Var value, Var id)) in
  Block.all env [expr_block; branch_block]


and translate_pattern_variable (env : (_, _) Env.t) value vname scope : Block.t =
  match vname with
  | "_" ->
      Block.make_success env
  | _ ->
      let procname =
        match scope with
        | Some name ->
            name
        | None ->
            L.die InternalError "Scope not found for variable, probably missing annotation."
      in
      let store : Sil.instr =
        let e1 : Exp.t = Lvar (Pvar.mk (Mangled.from_string vname) procname) in
        let e2 : Exp.t = Var value in
        Store {e1; root_typ= any_typ; typ= any_typ; e2; loc= env.location}
      in
      let exit_success = Node.make_stmt env [store] in
      let exit_failure = Node.make_nop env in
      {start= exit_success; exit_success; exit_failure}


and translate_guard_expression (env : (_, _) Env.t) (expression : Ast.expression) : Exp.t * Block.t
    =
  let id, guard_block = translate_expression_to_fresh_id env expression in
  (* If we'd like to catch "silent" errors later, we might do it here *)
  let unboxed, unbox_block = unbox_bool env (Exp.Var id) in
  (unboxed, Block.all env [guard_block; unbox_block])


and translate_guard env (expressions : Ast.expression list) : Block.t =
  match expressions with
  | [] ->
      Block.make_success env
  | _ ->
      let exprs_blocks = List.map ~f:(translate_guard_expression env) expressions in
      let exprs, blocks = List.unzip exprs_blocks in
      let make_and e1 e2 = Exp.BinOp (LAnd, e1, e2) in
      let condition = List.reduce_exn exprs ~f:make_and in
      let branch_block = Block.make_branch env condition in
      Block.all env (blocks @ [branch_block])


(** Main entry point for translating guards. *)
and translate_guard_sequence env (guards : Ast.expression list list) : Block.t =
  match guards with
  | [] ->
      Block.make_success env
  | _ ->
      Block.any env (List.map ~f:(translate_guard env) guards)


(** Main entry point for translating an expression. The result is a block, which has the expression
    translated. The result is put into the variable as specified by the environment (or a fresh
    value). *)
and translate_expression env {Ast.location; simple_expression} =
  let env = update_location location env in
  let (Env.Present result) = env.result in
  let ret_var = match result with Exp.Var ret_var -> ret_var | _ -> mk_fresh_id () in
  let env = {env with result= Env.Present (Exp.Var ret_var)} in
  let expression_block : Block.t =
    match simple_expression with
    | BinaryOperator (e1, op, e2) ->
        translate_expression_binary_operator env ret_var e1 op e2
    | Block body ->
        translate_body env body
    | Call
        { module_= None
        ; function_= {Ast.location= _; simple_expression= Literal (Atom function_name)}
        ; args } ->
        translate_expression_call_static env ret_var None function_name args
    | Call
        { module_= Some {Ast.location= _; simple_expression= Literal (Atom module_name)}
        ; function_= {Ast.location= _; simple_expression= Literal (Atom function_name)}
        ; args } ->
        translate_expression_call_static env ret_var (Some module_name) function_name args
    | Call {module_= None; function_; args} ->
        translate_expression_call_dynamic env ret_var function_ args
    | Case {expression; cases} ->
        translate_expression_case env expression cases
    | Catch expression ->
        (* TODO: handle exceptions T95448111 *)
        Block.all env [Block.make_unsupported env; translate_expression env expression]
    | Cons {head; tail} ->
        translate_expression_cons env ret_var head tail
    | Fun {module_= ModuleName module_name; function_= FunctionName function_name; arity} ->
        let name = Procname.make_erlang ~module_name ~function_name ~arity in
        Block.make_load env ret_var (Exp.Closure {name; captured_vars= []}) any_typ
    | Fun {module_= ModuleMissing; function_= FunctionName function_name; arity} ->
        let name = Procname.make_erlang ~module_name:env.current_module ~function_name ~arity in
        Block.make_load env ret_var (Exp.Closure {name; captured_vars= []}) any_typ
    | If clauses ->
        translate_expression_if env clauses
    | Lambda {name= None; cases; procname; captured} -> (
      match captured with
      | Some captured ->
          translate_expression_lambda env ret_var cases procname captured
      | None ->
          L.die InternalError
            "Captured variables are missing for lambda. The scoping preprocessing step might be \
             missing." )
    | ListComprehension {expression; qualifiers} ->
        translate_expression_listcomprehension env ret_var expression qualifiers
    | Literal (Atom atom) ->
        translate_expression_literal_atom env ret_var atom
    | Literal (Int i) ->
        let e = Exp.Const (Cint (IntLit.of_string i)) in
        Block.make_load env ret_var e any_typ
    | Literal (String s) ->
        translate_expression_literal_string env ret_var s
    | Map {map= None; updates} ->
        translate_expression_map_create env ret_var updates
    | Map {map= Some map; updates} ->
        translate_expression_map_update env ret_var map updates
    | Match {pattern; body} ->
        translate_expression_match env ret_var pattern body
    | Nil ->
        translate_expression_nil env ret_var
    | RecordAccess {record; name; field} ->
        translate_expression_record_access env ret_var record name field
    | RecordIndex {name; field} ->
        translate_expression_record_index env ret_var name field
    | RecordUpdate {record; name; updates} ->
        translate_expression_record_update env ret_var record name updates
    | TryCatch {body; ok_cases; catch_cases; after} ->
        translate_expression_trycatch env ret_var body ok_cases catch_cases after
    | Tuple exprs ->
        translate_expression_tuple env ret_var exprs
    | UnaryOperator (op, e) ->
        translate_expression_unary_operator env ret_var op e
    | Variable {vname; scope} ->
        translate_expression_variable env ret_var vname scope
    | todo ->
        L.debug Capture Verbose "@[todo ErlangTranslator.translate_expression %s@."
          (Sexp.to_string (Ast.sexp_of_simple_expression todo)) ;
        Block.all env [Block.make_unsupported env; Block.make_success env]
  in
  (* Add extra nodes/instructions to store return value if needed *)
  match result with
  | Exp.Var _ ->
      expression_block
  | _ ->
      let store_instr =
        Sil.Store {e1= result; root_typ= any_typ; typ= any_typ; e2= Var ret_var; loc= env.location}
      in
      let store_block = Block.make_instruction env [store_instr] in
      Block.all env [expression_block; store_block]


(** Helper function for translating an expression assinging the result into the given identifier. *)
and translate_expression_to_id (env : (_, _) Env.t) id expression : Block.t =
  translate_expression {env with result= Env.Present (Exp.Var id)} expression


(** Helper function for translating an expression while creating a fresh identifier for the result. *)
and translate_expression_to_fresh_id (env : (_, _) Env.t) expression : Ident.t * Block.t =
  let id = mk_fresh_id () in
  let block = translate_expression_to_id env id expression in
  (id, block)


and translate_expression_binary_operator (env : (_, _) Env.t) ret_var e1 (op : Ast.binary_operator)
    e2 : Block.t =
  let id1, block1 = translate_expression_to_fresh_id env e1 in
  let id2, block2 = translate_expression_to_fresh_id env e2 in
  let make_simple_eager sil_op =
    let op_block = Block.make_load env ret_var (Exp.BinOp (sil_op, Var id1, Var id2)) any_typ in
    Block.all env [block1; block2; op_block]
  in
  let make_simple_eager_bool_unbox sil_op =
    let unbox1, unbox_block1 = unbox_bool env (Exp.Var id1) in
    let unbox2, unbox_block2 = unbox_bool env (Exp.Var id2) in
    let op_block = box_bool env ret_var (Exp.BinOp (sil_op, unbox1, unbox2)) in
    Block.all env [block1; unbox_block1; block2; unbox_block2; op_block]
  in
  let make_simple_eager_bool sil_op =
    let op_block = box_bool env ret_var (Exp.BinOp (sil_op, Var id1, Var id2)) in
    Block.all env [block1; block2; op_block]
  in
  let make_short_circuit_logic ~short_circuit_when_lhs_is =
    let unbox1, unbox_block1 = unbox_bool env (Exp.Var id1) in
    let start = Node.make_nop env in
    let id1_cond = Node.make_if env short_circuit_when_lhs_is unbox1 in
    let id2_cond = Node.make_if env (not short_circuit_when_lhs_is) unbox1 in
    let store_id1 = Node.make_load env ret_var (Var id1) any_typ in
    let store_id2 = Node.make_load env ret_var (Var id2) any_typ in
    let exit_success = Node.make_nop env in
    let exit_failure = Node.make_nop env in
    start |~~> [id1_cond; id2_cond] ;
    id1_cond |~~> [store_id1] ;
    store_id1 |~~> [exit_success] ;
    id2_cond |~~> [block2.start] ;
    block2.exit_success |~~> [store_id2] ;
    block2.exit_failure |~~> [exit_failure] ;
    store_id2 |~~> [exit_success] ;
    Block.all env [block1; unbox_block1; {start; exit_success; exit_failure}]
  in
  let make_func_call fun_exp =
    let args : (Exp.t * Typ.t) list = [(Var id1, any_typ); (Var id2, any_typ)] in
    let call_instr =
      Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)
    in
    Block.all env [block1; block2; Block.make_instruction env [call_instr]]
  in
  match op with
  | Add ->
      make_simple_eager (PlusA None)
  | And ->
      make_simple_eager_bool_unbox LAnd
  | AndAlso ->
      make_short_circuit_logic ~short_circuit_when_lhs_is:false
  | AtLeast ->
      make_simple_eager_bool Ge
  | AtMost ->
      make_simple_eager_bool Le
  | BAnd ->
      make_simple_eager BAnd
  | BOr ->
      make_simple_eager BOr
  | Bsl ->
      make_simple_eager Shiftlt
  | Bsr ->
      make_simple_eager Shiftrt
  | BXor ->
      make_simple_eager BXor
  (* TODO: proper modeling of equal vs exactly equal T95767672 *)
  | Equal | ExactlyEqual ->
      make_simple_eager_bool Eq
  (* TODO: proper modeling of not equal vs exactly not equal T95767672 *)
  | ExactlyNotEqual | NotEqual ->
      make_simple_eager_bool Ne
  | Greater ->
      make_simple_eager_bool Gt
  | IDiv ->
      make_simple_eager DivI
  | Less ->
      make_simple_eager_bool Lt
  | ListAdd ->
      make_func_call (Exp.Const (Cfun lists_append2))
  | Mul ->
      make_simple_eager (Mult None)
  | Or ->
      make_simple_eager_bool_unbox LOr
  | OrElse ->
      make_short_circuit_logic ~short_circuit_when_lhs_is:true
  | Rem ->
      make_simple_eager Mod (* TODO: check semantics of Rem vs Mod *)
  | Send ->
      make_func_call (Exp.Const (Cfun erlang_send2))
  | Sub ->
      make_simple_eager (MinusA None)
  | Xor ->
      let unbox1, unbox_block1 = unbox_bool env (Exp.Var id1) in
      let unbox2, unbox_block2 = unbox_bool env (Exp.Var id2) in
      let expr =
        Exp.BinOp
          ( LOr
          , Exp.BinOp (LAnd, unbox1, Exp.UnOp (LNot, unbox2, None))
          , Exp.BinOp (LAnd, Exp.UnOp (LNot, unbox1, None), unbox2) )
      in
      let op_block = box_bool env ret_var expr in
      Block.all env [block1; unbox_block1; block2; unbox_block2; op_block]
  | todo ->
      L.debug Capture Verbose "@[todo ErlangTranslator.translate_expression_binary_operator %s@."
        (Sexp.to_string (Ast.sexp_of_binary_operator todo)) ;
      Block.all env [block1; block2; Block.make_unsupported env]


and lookup_module_for_unqualified (env : (_, _) Env.t) function_name arity =
  (* First check if the function is imported or local (order does not matter
     as compiler should enforce that both cannot hold at the same time).
     Then assume it's built-in. *)
  let uf_name = {Env.UnqualifiedFunction.name= function_name; arity} in
  match Env.UnqualifiedFunction.Map.find env.imports uf_name with
  | Some name ->
      name
  | None ->
      if Env.UnqualifiedFunction.Set.mem env.functions uf_name then env.current_module
      else ErlangTypeName.erlang_namespace


and translate_expression_call (env : (_, _) Env.t) ret_var fun_exp args : Block.t =
  let args_with_ids = List.map ~f:(fun a -> (a, mk_fresh_id ())) args in
  let args_blocks =
    let f (one_arg_expression, one_arg_ret_var) =
      translate_expression_to_id env one_arg_ret_var one_arg_expression
    in
    List.map ~f args_with_ids
  in
  let args_ids_and_types = List.map ~f:(function _, id -> (Exp.Var id, any_typ)) args_with_ids in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, args_ids_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (args_blocks @ [call_block])


and translate_expression_call_dynamic (env : (_, _) Env.t) ret_var expression args : Block.t =
  let fun_expr_id, fun_expr_block = translate_expression_to_fresh_id env expression in
  let call_block = translate_expression_call env ret_var (Exp.Var fun_expr_id) args in
  Block.all env [fun_expr_block; call_block]


and translate_expression_call_static (env : (_, _) Env.t) ret_var module_name_opt function_name args
    : Block.t =
  let arity = List.length args in
  let callee_procname =
    let module_name =
      match module_name_opt with
      (* If we have a module name just use that *)
      | Some name ->
          name
      (* Otherwise we need a module name *)
      | None ->
          lookup_module_for_unqualified env function_name arity
    in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let fun_exp = Exp.Const (Cfun callee_procname) in
  translate_expression_call env ret_var fun_exp args


and translate_expression_case (env : (_, _) Env.t) expression cases : Block.t =
  let id, expr_block = translate_expression_to_fresh_id env expression in
  let blocks = Block.any env (List.map ~f:(translate_case_clause env [id]) cases) in
  let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_case_clause in
  blocks.exit_failure |~~> [crash_node] ;
  let blocks = {blocks with exit_failure= crash_node} in
  Block.all env [expr_block; blocks]


and translate_expression_cons (env : (_, _) Env.t) ret_var head tail : Block.t =
  let head_var, head_block = translate_expression_to_fresh_id env head in
  let tail_var, tail_block = translate_expression_to_fresh_id env tail in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_cons) in
  let args : (Exp.t * Typ.t) list = [(Var head_var, any_typ); (Var tail_var, any_typ)] in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)
  in
  Block.all env [head_block; tail_block; Block.make_instruction env [call_instruction]]


and translate_expression_if env clauses : Block.t =
  let blocks = Block.any env (List.map ~f:(translate_case_clause env []) clauses) in
  let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_if_clause in
  blocks.exit_failure |~~> [crash_node] ;
  {blocks with exit_failure= crash_node}


and translate_expression_lambda (env : (_, _) Env.t) ret_var cases procname_opt captured : Block.t =
  let arity =
    match cases with
    | c :: _ ->
        List.length c.Ast.patterns
    | _ ->
        L.die InternalError "Lambda has no clauses, cannot determine arity"
  in
  let name =
    match procname_opt with
    | Some name ->
        name
    | None ->
        L.die InternalError "Procname not found for lambda, probably missing annotation."
  in
  let captured_vars = Pvar.Set.elements captured in
  let attributes =
    let default = ProcAttributes.default env.location.file name in
    let access : ProcAttributes.access = Private in
    let formals = List.init ~f:(fun i -> (mangled_arg i, any_typ, Annot.Item.empty)) arity in
    let mk_capt_var (pvar : Pvar.t) = {CapturedVar.pvar; typ= any_typ; capture_mode= ByReference} in
    let captured = List.map ~f:mk_capt_var captured_vars in
    {default with access; formals; is_defined= true; loc= env.location; ret_type= any_typ; captured}
  in
  let procdesc =
    let procdesc = Cfg.create_proc_desc env.cfg attributes in
    let start_node = Procdesc.create_node procdesc env.location Start_node [] in
    let exit_node = Procdesc.create_node procdesc env.location Exit_node [] in
    Procdesc.set_start_node procdesc start_node ;
    Procdesc.set_exit_node procdesc exit_node ;
    procdesc
  in
  let sub_env =
    { env with
      procdesc= Env.Present procdesc
    ; result= Env.Present (Exp.Lvar (Pvar.get_ret_pvar name)) }
  in
  let () = translate_function_clauses sub_env procdesc attributes name cases in
  let closure =
    let mk_capt_var (var : Pvar.t) = (Exp.Lvar var, var, any_typ, CapturedVar.ByReference) in
    let captured_vars = List.map ~f:mk_capt_var captured_vars in
    Exp.Closure {name; captured_vars}
  in
  Block.make_load env ret_var closure any_typ


and translate_expression_listcomprehension (env : (_, _) Env.t) ret_var expression qualifiers :
    Block.t =
  let list_var = mk_fresh_id () in
  (* Start with en empty list L := Nil *)
  let init_block = translate_expression_nil env list_var in
  (* Compute one iteration of the expression and add to list: L := Cons(Expr, L) *)
  let loop_body =
    (* Compute result of the expression *)
    let expr_id, expr_block = translate_expression_to_fresh_id env expression in
    (* Prepend to list *)
    let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_cons) in
    let args : (Exp.t * Typ.t) list = [(Var expr_id, any_typ); (Var list_var, any_typ)] in
    let call_instr =
      Sil.Call ((list_var, any_typ), fun_exp, args, env.location, CallFlags.default)
    in
    Block.all env [expr_block; Block.make_instruction env [call_instr]]
  in
  (* Surround expression with filters *)
  let extract_filter (qual : Ast.qualifier) =
    match qual with Filter expr -> Some expr | _ -> None
  in
  let filters = List.filter_map ~f:extract_filter qualifiers in
  let apply_one_filter expr (acc : Block.t) : Block.t =
    (* Check expression, execute inner block (accumulator) only if true *)
    let result_id, filter_expr_block = translate_expression_to_fresh_id env expr in
    let unboxed, unbox_block = unbox_bool env (Exp.Var result_id) in
    let true_node = Node.make_if env true unboxed in
    let false_node = Node.make_if env false unboxed in
    let fail_node = Node.make_nop env in
    let succ_node = Node.make_nop env in
    let filter_expr_block = Block.all env [filter_expr_block; unbox_block] in
    filter_expr_block.exit_success |~~> [true_node; false_node] ;
    filter_expr_block.exit_failure |~~> [fail_node] ;
    true_node |~~> [acc.start] ;
    false_node |~~> [succ_node] ;
    acc.exit_success |~~> [succ_node] ;
    acc.exit_failure |~~> [fail_node] ;
    {start= filter_expr_block.start; exit_success= succ_node; exit_failure= fail_node}
  in
  let loop_body_with_filters = List.fold_right filters ~f:apply_one_filter ~init:loop_body in
  (* Translate generators *)
  let extract_generator (qual : Ast.qualifier) =
    match qual with Generator {pattern; expression} -> Some (pattern, expression) | _ -> None
  in
  let generators = List.filter_map ~f:extract_generator qualifiers in
  (* Wrap filtered expression with loops for generators*)
  let apply_one_gen (pat, expr) (acc : Block.t) : Block.t =
    (* Initialize generator *)
    let gen_var, init_block = translate_expression_to_fresh_id env expr in
    (* Check if there are still elements in the generator *)
    let join_node = Node.make_join env in
    let is_cons_id = mk_fresh_id () in
    let check_cons_node =
      Node.make_stmt env [has_type env ~result:is_cons_id ~value:(Var gen_var) Cons]
    in
    let is_cons_node = Node.make_if env true (Var is_cons_id) in
    let no_cons_node = Node.make_if env false (Var is_cons_id) in
    (* Load head, overwrite list with tail for next iteration *)
    let head_var = mk_fresh_id () in
    let head_load = load_field_from_id env head_var gen_var ErlangTypeName.cons_head Cons in
    let tail_load = load_field_from_id env gen_var gen_var ErlangTypeName.cons_tail Cons in
    let unpack_node = Node.make_stmt env [head_load; tail_load] in
    (* Match head and evaluate expression *)
    let head_matcher = translate_pattern env head_var pat in
    let fail_node = Node.make_nop env in
    init_block.exit_success |~~> [join_node] ;
    init_block.exit_failure |~~> [fail_node] ;
    join_node |~~> [check_cons_node] ;
    check_cons_node |~~> [is_cons_node; no_cons_node] ;
    is_cons_node |~~> [unpack_node] ;
    unpack_node |~~> [head_matcher.start] ;
    head_matcher.exit_success |~~> [acc.start] ;
    head_matcher.exit_failure |~~> [join_node] ;
    acc.exit_success |~~> [join_node] ;
    acc.exit_failure |~~> [fail_node] ;
    {start= init_block.start; exit_success= no_cons_node; exit_failure= fail_node}
  in
  let loop_block = List.fold_right generators ~f:apply_one_gen ~init:loop_body_with_filters in
  (* Store lists:reverse(L) in return variable *)
  let store_return_block =
    let fun_exp = Exp.Const (Cfun lists_reverse) in
    let args : (Exp.t * Typ.t) list = [(Var list_var, any_typ)] in
    let call_instr =
      Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)
    in
    Block.make_instruction env [call_instr]
  in
  Block.all env [init_block; loop_block; store_return_block]


and translate_expression_literal_atom (env : (_, _) Env.t) ret_var atom =
  let call_instruction = mk_atom_call env ret_var atom in
  Block.make_instruction env [call_instruction]


and translate_expression_literal_string (env : (_, _) Env.t) ret_var s =
  (* TODO: add Pulse model for this function T93361792 *)
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_str_const) in
  let args =
    let value = Exp.Const (Cstr s) in
    [(value, any_typ)]
  in
  let instr = Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default) in
  Block.make_instruction env [instr]


and translate_expression_map_create (env : (_, _) Env.t) ret_var updates : Block.t =
  (* Get keys and values as an alternating list of expressions: [K1; V1; K2; V2; ...] *)
  let exprs = List.concat_map ~f:(fun (a : Ast.association) -> [a.key; a.value]) updates in
  let exprs_with_ids = List.map ~f:(fun e -> (e, mk_fresh_id ())) exprs in
  let expr_blocks =
    let translate_one_expr (one_expr, one_id) = translate_expression_to_id env one_id one_expr in
    List.map ~f:translate_one_expr exprs_with_ids
  in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_map) in
  let exprs_ids_and_types =
    List.map ~f:(function _, id -> (Exp.Var id, any_typ)) exprs_with_ids
  in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, exprs_ids_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (expr_blocks @ [call_block])


and translate_expression_map_update (env : (_, _) Env.t) ret_var map updates : Block.t =
  let map_id, map_block = translate_expression_to_fresh_id env map in
  let check_map_type_block : Block.t =
    let type_checker = check_type env map_id Map in
    let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badmap in
    type_checker.exit_failure |~~> [crash_node] ;
    {start= type_checker.start; exit_success= type_checker.exit_success; exit_failure= crash_node}
  in
  (* Translate updates one-by-one, also check key if exact association *)
  let translate_update (one_update : Ast.association) =
    let key_id, key_expr_block = translate_expression_to_fresh_id env one_update.key in
    let value_id, value_expr_block = translate_expression_to_fresh_id env one_update.value in
    let update_fun_exp = Exp.Const (Cfun maps_put) in
    let update_args =
      [(Exp.Var key_id, any_typ); (Exp.Var value_id, any_typ); (Exp.Var map_id, any_typ)]
    in
    let update_block =
      Block.make_instruction env
        [Sil.Call ((ret_var, any_typ), update_fun_exp, update_args, env.location, CallFlags.default)]
    in
    let has_key_block : Block.t list =
      match one_update.kind with
      | Arrow ->
          []
      | Exact ->
          let has_key_id = mk_fresh_id () in
          let has_key_fun_exp = Exp.Const (Cfun maps_is_key) in
          let args = [(Exp.Var key_id, any_typ); (Exp.Var map_id, any_typ)] in
          let call_block =
            Block.make_instruction env
              [ Sil.Call
                  ((has_key_id, any_typ), has_key_fun_exp, args, env.location, CallFlags.default) ]
          in
          let unboxed, unbox_block = unbox_bool env (Exp.Var has_key_id) in
          let check_block : Block.t =
            let start = Node.make_nop env in
            let exit_success = Node.make_if env true unboxed in
            let no_key_node = Node.make_if env false unboxed in
            let exit_failure = Node.make_fail env BuiltinDecl.__erlang_error_badkey in
            start |~~> [exit_success; no_key_node] ;
            no_key_node |~~> [exit_failure] ;
            {start; exit_success; exit_failure}
          in
          [Block.all env [call_block; unbox_block; check_block]]
    in
    Block.all env ([key_expr_block; value_expr_block] @ has_key_block @ [update_block])
  in
  (* TODO: what should be the order of updates? *)
  let update_blocks = List.map ~f:translate_update updates in
  Block.all env ([map_block; check_map_type_block] @ update_blocks)


and translate_expression_match (env : (_, _) Env.t) ret_var pattern body : Block.t =
  let body_block = translate_expression_to_id env ret_var body in
  let pattern_block = translate_pattern env ret_var pattern in
  let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badmatch in
  pattern_block.exit_failure |~~> [crash_node] ;
  let pattern_block = {pattern_block with exit_failure= crash_node} in
  Block.all env [body_block; pattern_block]


and translate_expression_nil (env : (_, _) Env.t) ret_var : Block.t =
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_nil) in
  let instruction = Sil.Call ((ret_var, any_typ), fun_exp, [], env.location, CallFlags.default) in
  Block.make_instruction env [instruction]


and translate_expression_record_access (env : (_, _) Env.t) ret_var record name field : Block.t =
  (* Under the hood, a record is a tagged tuple, the first element is the name,
     and then the fields follow in the order as in the record definition. *)
  let record_info = String.Map.find_exn env.records name in
  let record_id = mk_fresh_id () in
  let record_block =
    let value_block = translate_expression_to_id env record_id record in
    let matcher_block = match_record_name env record_id name record_info in
    let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badrecord in
    matcher_block.exit_failure |~~> [crash_node] ;
    let matcher_block = {matcher_block with exit_failure= crash_node} in
    Block.all env [value_block; matcher_block]
  in
  let field_info = String.Map.find_exn record_info.field_info field in
  let field_no = field_info.index in
  let tuple_typ : ErlangTypeName.t = Tuple (1 + List.length record_info.field_names) in
  let field_load =
    load_field_from_id env ret_var record_id (ErlangTypeName.tuple_elem field_no) tuple_typ
  in
  let load_block = Block.make_instruction env [field_load] in
  Block.all env [record_block; load_block]


and translate_expression_record_index (env : (_, _) Env.t) ret_var name field : Block.t =
  let record_info = String.Map.find_exn env.records name in
  let field_info = String.Map.find_exn record_info.field_info field in
  let expr = Exp.Const (Cint (IntLit.of_int field_info.index)) in
  Block.make_load env ret_var expr any_typ


and translate_expression_record_update (env : (_, _) Env.t) ret_var record name updates : Block.t =
  (* Under the hood, a record is a tagged tuple, the first element is the name,
     and then the fields follow in the order as in the record definition. *)
  let record_info = String.Map.find_exn env.records name in
  let tuple_typ : ErlangTypeName.t = Tuple (1 + List.length record_info.field_names) in
  (* First collect all the fields that are updated *)
  let collect_updates map (one_update : Ast.record_update) =
    match one_update.field with
    | Some name ->
        Map.add_exn ~key:name ~data:one_update.expression map
    | None ->
        (* '_' stands for 'everything else' *)
        Map.add_exn ~key:"_" ~data:one_update.expression map
  in
  let updates_map = List.fold ~init:String.Map.empty ~f:collect_updates updates in
  (* Translate record expression if it is an update *)
  let record_id = mk_fresh_id () in
  let record_block =
    match record with
    | Some expr ->
        let value_block = translate_expression_to_id env record_id expr in
        let matcher_block = match_record_name env record_id name record_info in
        let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badrecord in
        matcher_block.exit_failure |~~> [crash_node] ;
        let matcher_block = {matcher_block with exit_failure= crash_node} in
        [Block.all env [value_block; matcher_block]]
    | None ->
        []
  in
  (* Translate each field: the value can come from 5 different sources *)
  let translate_one_field ((one_field_name, one_id) : string * Ident.t) =
    (* (1) Check if field is explicitly set *)
    match String.Map.find updates_map one_field_name with
    | Some expr ->
        translate_expression_to_id env one_id expr
    | None -> (
      (* (2) Check if field is set using 'everything else' *)
      match String.Map.find updates_map "_" with
      | Some expr ->
          translate_expression_to_id env one_id expr
      | None -> (
          let field_info = String.Map.find_exn record_info.field_info one_field_name in
          (* (3) Check if we have to copy over from record that is being updated *)
          match record with
          | Some _ ->
              let field_load =
                load_field_from_id env one_id record_id
                  (ErlangTypeName.tuple_elem field_info.index)
                  tuple_typ
              in
              Block.make_instruction env [field_load]
          | None -> (
            (* (4) Check if there is an initializer *)
            match field_info.initializer_ with
            | Some expr ->
                translate_expression_to_id env one_id expr
            | None ->
                (* (5) Finally, it's undefined *)
                let undef_atom = mk_fresh_id () in
                let mk_undef_block = translate_expression_literal_atom env undef_atom "undefined" in
                let load_undef_block = Block.make_load env one_id (Exp.Var undef_atom) any_typ in
                Block.all env [mk_undef_block; load_undef_block] ) ) )
  in
  let field_names = record_info.field_names in
  let field_ids = List.map ~f:(function _ -> mk_fresh_id ()) field_names in
  let field_blocks = List.map ~f:translate_one_field (List.zip_exn field_names field_ids) in
  let field_ids_and_types = List.map ~f:(fun id -> (Exp.Var id, any_typ)) field_ids in
  let name_atom = mk_fresh_id () in
  let mk_name_block = translate_expression_literal_atom env name_atom name in
  let args_and_types = (Exp.Var name_atom, any_typ) :: field_ids_and_types in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_tuple) in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, args_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (record_block @ field_blocks @ [mk_name_block; call_block])


and translate_expression_trycatch (env : (_, _) Env.t) ret_var body ok_cases _catch_cases after :
    Block.t =
  let body_id = mk_fresh_id () in
  let body_block = translate_body {env with result= Env.Present (Exp.Var body_id)} body in
  let ok_blocks : Block.t =
    match ok_cases with
    | [] ->
        (* No ok cases: result comes from the body expression *)
        Block.make_load env ret_var (Var body_id) any_typ
    | _ ->
        (* Ok cases present: treat as case expression *)
        let cases = Block.any env (List.map ~f:(translate_case_clause env [body_id]) ok_cases) in
        let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_try_clause in
        cases.exit_failure |~~> [crash_node] ;
        {cases with exit_failure= crash_node}
  in
  let after_id = mk_fresh_id () in
  let after_block = translate_body {env with result= Env.Present (Exp.Var after_id)} after in
  (* TODO: handle exceptions T95448111 *)
  let catch_blocks = Block.make_unsupported env in
  Block.all env [body_block; ok_blocks; catch_blocks; after_block]


and translate_expression_tuple (env : (_, _) Env.t) ret_var exprs : Block.t =
  let exprs_with_ids = List.map ~f:(fun e -> (e, mk_fresh_id ())) exprs in
  let expr_blocks =
    let f (one_expr, one_id) = translate_expression_to_id env one_id one_expr in
    List.map ~f exprs_with_ids
  in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_tuple) in
  let exprs_ids_and_types =
    List.map ~f:(function _, id -> (Exp.Var id, any_typ)) exprs_with_ids
  in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, exprs_ids_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (expr_blocks @ [call_block])


and translate_expression_unary_operator (env : (_, _) Env.t) ret_var (op : Ast.unary_operator) e :
    Block.t =
  let id, block = translate_expression_to_fresh_id env e in
  let make_simple_op_block sil_op =
    Block.make_load env ret_var (Exp.UnOp (sil_op, Var id, None)) any_typ
  in
  let make_simple_bool_unbox sil_op =
    let unbox, unbox_block = unbox_bool env (Exp.Var id) in
    let op_block = box_bool env ret_var (Exp.UnOp (sil_op, unbox, None)) in
    Block.all env [unbox_block; op_block]
  in
  let op_block =
    match op with
    | UBNot ->
        make_simple_op_block BNot
    | UMinus ->
        make_simple_op_block Neg
    | UNot ->
        make_simple_bool_unbox LNot
  in
  Block.all env [block; op_block]


and translate_expression_variable (env : (_, _) Env.t) ret_var vname scope : Block.t =
  let procname =
    match scope with
    | Some name ->
        name
    | None ->
        L.die InternalError "Scope not found for variable, probably missing annotation."
  in
  let e = Exp.Lvar (Pvar.mk (Mangled.from_string vname) procname) in
  let load_instr = Sil.Load {id= ret_var; e; root_typ= any_typ; typ= any_typ; loc= env.location} in
  Block.make_instruction env [load_instr]


and translate_body (env : (_, _) Env.t) body : Block.t =
  let blocks =
    let f rev_blocks one_expr =
      (* We ignore the return value of intermediate expressions *)
      let _id, block = translate_expression_to_fresh_id env one_expr in
      block :: rev_blocks
    in
    (* Last expression needs separate treatment to use its result as the overall result *)
    let f_last rev_blocks one_expr = translate_expression env one_expr :: rev_blocks in
    List.rev (IList.fold_last body ~init:[] ~f ~f_last)
  in
  Block.all env blocks


(** Translate one clause (can come from functions, lambdas, case expressions, ...). Assumes that the
    values on which patterns should be matched have been loaded into the identifiers listed in
    [values]. *)
and translate_case_clause (env : (_, _) Env.t) (values : Ident.t list)
    {Ast.location= _; patterns; guards; body} : Block.t =
  let f (one_value, one_pattern) = translate_pattern env one_value one_pattern in
  let matchers = List.map ~f (List.zip_exn values patterns) in
  let guard_block = translate_guard_sequence env guards in
  let matchers_and_guards = Block.all env [Block.all env matchers; guard_block] in
  let body_block = translate_body env body in
  matchers_and_guards.exit_success |~~> [body_block.start] ;
  let () =
    let (Env.Present procdesc) = env.procdesc in
    body_block.exit_failure |~~> [Procdesc.get_exit_node procdesc]
  in
  { start= matchers_and_guards.start
  ; exit_failure= matchers_and_guards.exit_failure
  ; exit_success= body_block.exit_success }


(** Translate all clauses of a function (top-level or lambda). *)
and translate_function_clauses (env : (_, _) Env.t) procdesc (attributes : ProcAttributes.t)
    procname clauses =
  (* Load formals into fresh identifiers. *)
  let idents, loads =
    let load (formal, typ, _) =
      let id = mk_fresh_id () in
      let pvar = Pvar.mk formal procname in
      let load = Sil.Load {id; e= Exp.Lvar pvar; root_typ= typ; typ; loc= attributes.loc} in
      (id, load)
    in
    List.unzip (List.map ~f:load attributes.formals)
  in
  (* Translate each clause using the idents we load into. *)
  let ({start; exit_success; exit_failure} : Block.t) =
    let blocks = List.map ~f:(translate_case_clause env idents) clauses in
    Block.any env blocks
  in
  let () =
    (* Put the loading node before the translated clauses. *)
    let loads_node = Node.make_stmt env ~kind:ErlangCaseClause loads in
    Procdesc.get_start_node procdesc |~~> [loads_node] ;
    loads_node |~~> [start]
  in
  let () =
    (* Finally, if all patterns fail, report error. *)
    let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_function_clause in
    exit_failure |~~> [crash_node] ;
    crash_node |~~> [Procdesc.get_exit_node procdesc]
  in
  exit_success |~~> [Procdesc.get_exit_node procdesc]


(** Translate one top-level function. *)
let translate_one_function (env : (_, _) Env.t) function_ clauses =
  let uf_name, procname = Env.func_procname env function_ in
  let attributes =
    let default = ProcAttributes.default env.location.file procname in
    let access : ProcAttributes.access = if Set.mem env.exports uf_name then Public else Private in
    let formals =
      List.init ~f:(fun i -> (mangled_arg i, any_typ, Annot.Item.empty)) uf_name.arity
    in
    {default with access; formals; is_defined= true; loc= env.location; ret_type= any_typ}
  in
  let procdesc =
    let procdesc = Cfg.create_proc_desc env.cfg attributes in
    let start_node = Procdesc.create_node procdesc env.location Start_node [] in
    let exit_node = Procdesc.create_node procdesc env.location Exit_node [] in
    Procdesc.set_start_node procdesc start_node ;
    Procdesc.set_exit_node procdesc exit_node ;
    procdesc
  in
  let env =
    { env with
      procdesc= Env.Present procdesc
    ; result= Env.Present (Exp.Lvar (Pvar.get_ret_pvar procname)) }
  in
  translate_function_clauses env procdesc attributes procname clauses


(** Translate and store each function of a module. *)
let translate_functions (env : (_, _) Env.t) module_ =
  let f {Ast.location; simple_form} =
    let env = update_location location env in
    match simple_form with
    | Function {function_; clauses} ->
        translate_one_function env function_ clauses
    | _ ->
        ()
  in
  List.iter module_ ~f ;
  DB.Results_dir.init env.location.file ;
  let tenv = Tenv.FileLocal (Tenv.create ()) in
  SourceFiles.add env.location.file env.cfg tenv None


let translate_module = translate_functions
