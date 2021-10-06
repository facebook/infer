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

let mangled_arg (n : int) : Mangled.t = Mangled.from_string (Printf.sprintf "$arg%d" n)

let any_typ = Env.ptr_typ_of_name Any

let mk_fresh_id () = Ident.create_fresh Ident.knormal

let ( |~~> ) = ErlangBlock.( |~~> )

let update_location line (env : (_, _) Env.t) =
  let location = {env.location with line; col= -1} in
  {env with location}


let has_type (env : (_, _) Env.t) ~result ~value (name : ErlangTypeName.t) : Sil.instr =
  let fun_exp : Exp.t = Const (Cfun BuiltinDecl.__instanceof) in
  let args : (Exp.t * Typ.t) list =
    [ (Var value, any_typ)
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
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value typ] in
  let exit_success = Node.make_if env true (Var is_right_type_id) in
  let exit_failure = Node.make_if env false (Var is_right_type_id) in
  start |~~> [exit_success; exit_failure] ;
  {start; exit_success; exit_failure}


let translate_atom_literal (atom : string) : Exp.t =
  (* With this hack, an atom may accidentaly be considered equal to an unrelated integer.
      The [lsl] below makes this less likely. Proper fix is TODO (T93513105). *)
  let hash = String.hash atom lsl 16 in
  Exp.Const (Cint (IntLit.of_int hash))


(** into_id=value_id.field_name *)
let load_field (env : (_, _) Env.t) into_id value_id field_name typ : Sil.instr =
  let field = Fieldname.make (ErlangType typ) field_name in
  Load
    { id= into_id
    ; e= Lfield (Var value_id, field, Env.typ_of_name typ)
    ; root_typ= any_typ
    ; typ= any_typ
    ; loc= env.location }


let match_record_name env value name (record_info : Env.record_info) : Block.t =
  let tuple_size = 1 + List.length record_info.field_names in
  let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
  let type_checker = check_type env value tuple_typ in
  let name_id = mk_fresh_id () in
  let name_load = load_field env name_id value (ErlangTypeName.tuple_elem 1) tuple_typ in
  let unpack_node = Node.make_stmt env [name_load] in
  let name_cond = Exp.BinOp (Eq, Var name_id, translate_atom_literal name) in
  let right_name_node = Node.make_if env true name_cond in
  let wrong_name_node = Node.make_if env false name_cond in
  let exit_failure = Node.make_nop env in
  type_checker.exit_success |~~> [unpack_node] ;
  unpack_node |~~> [right_name_node; wrong_name_node] ;
  type_checker.exit_failure |~~> [exit_failure] ;
  wrong_name_node |~~> [exit_failure] ;
  {start= type_checker.start; exit_success= right_name_node; exit_failure}


(** If the pattern-match succeeds, then the [exit_success] node is reached and the pattern variables
    are storing the corresponding values; otherwise, the [exit_failure] node is reached. *)
let rec translate_pattern env (value : Ident.t) {Ast.line; simple_expression} : Block.t =
  let env = update_location line env in
  match simple_expression with
  | Cons {head; tail} ->
      translate_pattern_cons env value head tail
  | Literal (Atom atom) ->
      let e = translate_atom_literal atom in
      Block.make_branch env (Exp.BinOp (Eq, Var value, e))
  | Literal (Int i) ->
      let e = Exp.Const (Cint (IntLit.of_string i)) in
      Block.make_branch env (Exp.BinOp (Eq, Var value, e))
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
      translate_pattern_unary_expression env value line simple_expression
  | Variable vname ->
      translate_pattern_variable env value vname
  | e ->
      (* TODO: Cover all cases. *)
      L.debug Capture Verbose "@[todo ErlangTranslator.translate_pattern %s@."
        (Sexp.to_string (Ast.sexp_of_simple_expression e)) ;
      Block.all env [Block.make_unsupported env; Block.make_failure env]


and translate_pattern_cons env value head tail : Block.t =
  let head_value = mk_fresh_id () in
  let tail_value = mk_fresh_id () in
  let head_load = load_field env head_value value ErlangTypeName.cons_head Cons in
  let tail_load = load_field env tail_value value ErlangTypeName.cons_tail Cons in
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
    let has_key_id = mk_fresh_id () in
    let key_id, key_expr_block = translate_expression_to_fresh_id env one_update.key in
    let has_key_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_is_key) in
    let args = [(Exp.Var key_id, any_typ); (Exp.Var value, any_typ)] in
    let has_key_block : Block.t =
      let start =
        Node.make_stmt env
          [Sil.Call ((has_key_id, any_typ), has_key_fun_exp, args, env.location, CallFlags.default)]
      in
      let exit_success = Node.make_if env true (Var has_key_id) in
      let exit_failure = Node.make_if env false (Var has_key_id) in
      start |~~> [exit_success; exit_failure] ;
      {start; exit_success; exit_failure}
    in
    let value_id = mk_fresh_id () in
    let lookup_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_get) in
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
        let load_instr = load_field env value_id value tuple_elem tuple_typ in
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
      ~f:(function one_value, one_field -> load_field env one_value value one_field tuple_typ)
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


and translate_pattern_unary_expression (env : (_, _) Env.t) value line simple_expression : Block.t =
  (* Unary op pattern must evaluate to number, so just delegate to expression translation *)
  let id, expr_block = translate_expression_to_fresh_id env {Ast.line; simple_expression} in
  let branch_block = Block.make_branch env (Exp.BinOp (Eq, Var value, Var id)) in
  Block.all env [expr_block; branch_block]


and translate_pattern_variable (env : (_, _) Env.t) value vname : Block.t =
  match vname with
  | "_" ->
      Block.make_success env
  | _ ->
      let (Env.Present procdesc) = env.procdesc in
      let procname = Procdesc.get_proc_name procdesc in
      let store : Sil.instr =
        let e1 : Exp.t = Lvar (Pvar.mk (Mangled.from_string vname) procname) in
        let e2 : Exp.t = Var value in
        Store {e1; root_typ= any_typ; typ= any_typ; e2; loc= env.location}
      in
      let exit_success = Node.make_stmt env [store] in
      let exit_failure = Node.make_nop env in
      {start= exit_success; exit_success; exit_failure}


and translate_guard_expression (env : (_, _) Env.t) (expression : Ast.expression) :
    Ident.t * Block.t =
  let id, block = translate_expression_to_fresh_id env expression in
  (* If we'd like to catch "silent" errors later, we might do it here *)
  (id, block)


and translate_guard env (expressions : Ast.expression list) : Block.t =
  match expressions with
  | [] ->
      Block.make_success env
  | _ ->
      let ids_blocks = List.map ~f:(translate_guard_expression env) expressions in
      let ids, expr_blocks = List.unzip ids_blocks in
      let make_and (e1 : Exp.t) (e2 : Exp.t) = Exp.BinOp (LAnd, e1, e2) in
      let make_var (id : Ident.t) : Exp.t = Var id in
      let cond = List.reduce_exn (List.map ids ~f:make_var) ~f:make_and in
      let start = Node.make_nop env in
      let exit_success = Node.make_if env true cond in
      let exit_failure = Node.make_if env false cond in
      start |~~> [exit_success; exit_failure] ;
      Block.all env (expr_blocks @ [{Block.start; exit_success; exit_failure}])


and translate_guard_sequence env (guards : Ast.expression list list) : Block.t =
  match guards with
  | [] ->
      Block.make_success env
  | _ ->
      Block.any env (List.map ~f:(translate_guard env) guards)


and translate_expression env {Ast.line; simple_expression} =
  let env = update_location line env in
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
        ; function_= {Ast.line= _; simple_expression= Literal (Atom function_name)}
        ; args } ->
        translate_expression_call env ret_var None function_name args
    | Call
        { module_= Some {Ast.line= _; simple_expression= Literal (Atom module_name)}
        ; function_= {Ast.line= _; simple_expression= Literal (Atom function_name)}
        ; args } ->
        translate_expression_call env ret_var (Some module_name) function_name args
    | Case {expression; cases} ->
        translate_expression_case env expression cases
    | Catch expression ->
        (* TODO: handle exceptions T95448111 *)
        Block.all env [Block.make_unsupported env; translate_expression env expression]
    | Cons {head; tail} ->
        translate_expression_cons env ret_var head tail
    | If clauses ->
        translate_expression_if env clauses
    | ListComprehension {expression; qualifiers} ->
        translate_expression_listcomprehension env ret_var expression qualifiers
    | Literal (Atom atom) ->
        let e = translate_atom_literal atom in
        Block.make_load env ret_var e any_typ
    | Literal (Int i) ->
        let e = Exp.Const (Cint (IntLit.of_string i)) in
        Block.make_load env ret_var e any_typ
    | Literal (String s) ->
        let e = Exp.Const (Cstr s) in
        Block.make_load env ret_var e any_typ
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
    | Variable vname ->
        translate_expression_variable env ret_var vname
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


(** Translate an expression assinging the result into the given identifier. *)
and translate_expression_to_id (env : (_, _) Env.t) id expression : Block.t =
  translate_expression {env with result= Env.Present (Exp.Var id)} expression


(** Translate an expression while creating a fresh identifier to store the result. *)
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
  let make_short_circuit_logic ~short_circuit_when_lhs_is =
    let start = Node.make_nop env in
    let id1_cond = Node.make_if env short_circuit_when_lhs_is (Var id1) in
    let id2_cond = Node.make_if env (not short_circuit_when_lhs_is) (Var id1) in
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
    Block.all env [block1; {start; exit_success; exit_failure}]
  in
  match op with
  | Add ->
      make_simple_eager (PlusA None)
  | And ->
      make_simple_eager LAnd
  | AndAlso ->
      make_short_circuit_logic ~short_circuit_when_lhs_is:false
  | AtLeast ->
      make_simple_eager Ge
  | AtMost ->
      make_simple_eager Le
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
      make_simple_eager Eq
  (* TODO: proper modeling of not equal vs exactly not equal T95767672 *)
  | ExactlyNotEqual | NotEqual ->
      make_simple_eager Ne
  | Greater ->
      make_simple_eager Gt
  | IDiv ->
      make_simple_eager Div
  | Less ->
      make_simple_eager Lt
  | ListAdd ->
      let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_list_append2) in
      let args : (Exp.t * Typ.t) list = [(Var id1, any_typ); (Var id2, any_typ)] in
      let call_instr =
        Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)
      in
      Block.all env [block1; block2; Block.make_instruction env [call_instr]]
  | Mul ->
      make_simple_eager (Mult None)
  | Or ->
      make_simple_eager LOr
  | OrElse ->
      make_short_circuit_logic ~short_circuit_when_lhs_is:true
  | Rem ->
      make_simple_eager Mod (* TODO: check semantics of Rem vs Mod *)
  | Sub ->
      make_simple_eager (MinusA None)
  | Xor ->
      let expr =
        Exp.BinOp
          ( LOr
          , Exp.BinOp (LAnd, Var id1, Exp.UnOp (LNot, Var id2, None))
          , Exp.BinOp (LAnd, Exp.UnOp (LNot, Var id1, None), Var id2) )
      in
      let op_block = Block.make_load env ret_var expr any_typ in
      Block.all env [block1; block2; op_block]
  | todo ->
      L.debug Capture Verbose "@[todo ErlangTranslator.translate_expression_binary_operator %s@."
        (Sexp.to_string (Ast.sexp_of_binary_operator todo)) ;
      Block.all env [block1; block2; Block.make_unsupported env]


and translate_expression_call (env : (_, _) Env.t) ret_var module_name function_name args : Block.t
    =
  let arity = List.length args in
  let callee_procname =
    let module_name_lookup =
      match module_name with
      | Some name ->
          name
      | None -> (
          let uf_name = {Env.UnqualifiedFunction.T.name= function_name; arity} in
          match Env.UnqualifiedFunction.Map.find env.imports uf_name with
          | Some name ->
              name
          | None ->
              env.current_module )
    in
    Procname.make_erlang ~module_name:module_name_lookup ~function_name ~arity
  in
  let args_with_ids = List.map ~f:(fun a -> (a, mk_fresh_id ())) args in
  let args_blocks =
    let f (one_arg_expression, one_arg_ret_var) =
      translate_expression_to_id env one_arg_ret_var one_arg_expression
    in
    List.map ~f args_with_ids
  in
  let fun_exp = Exp.Const (Cfun callee_procname) in
  let args_ids_and_types = List.map ~f:(function _, id -> (Exp.Var id, any_typ)) args_with_ids in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, args_ids_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (args_blocks @ [call_block])


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
    let true_node = Node.make_if env true (Var result_id) in
    let false_node = Node.make_if env false (Var result_id) in
    let fail_node = Node.make_nop env in
    let succ_node = Node.make_nop env in
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
      Node.make_stmt env [has_type env ~result:is_cons_id ~value:gen_var Cons]
    in
    let is_cons_node = Node.make_if env true (Var is_cons_id) in
    let no_cons_node = Node.make_if env false (Var is_cons_id) in
    (* Load head, overwrite list with tail for next iteration *)
    let head_var = mk_fresh_id () in
    let head_load = load_field env head_var gen_var ErlangTypeName.cons_head Cons in
    let tail_load = load_field env gen_var gen_var ErlangTypeName.cons_tail Cons in
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
    let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_list_reverse) in
    let args : (Exp.t * Typ.t) list = [(Var list_var, any_typ)] in
    let call_instr =
      Sil.Call ((ret_var, any_typ), fun_exp, args, env.location, CallFlags.default)
    in
    Block.make_instruction env [call_instr]
  in
  Block.all env [init_block; loop_block; store_return_block]


and translate_expression_map_create (env : (_, _) Env.t) ret_var updates : Block.t =
  (* Get keys and values as an alternating list of expressions: [K1; V1; K2; V2; ...] *)
  let exprs = List.concat_map ~f:(fun (a : Ast.association) -> [a.key; a.value]) updates in
  let exprs_with_ids = List.map ~f:(fun e -> (e, mk_fresh_id ())) exprs in
  let expr_blocks =
    let translate_one_expr (one_expr, one_id) = translate_expression_to_id env one_id one_expr in
    List.map ~f:translate_one_expr exprs_with_ids
  in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_create) in
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
    let update_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_put) in
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
          let has_key_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_is_key) in
          let has_key_args = [(Exp.Var key_id, any_typ); (Exp.Var map_id, any_typ)] in
          let start =
            Node.make_stmt env
              [ Sil.Call
                  ( (has_key_id, any_typ)
                  , has_key_fun_exp
                  , has_key_args
                  , env.location
                  , CallFlags.default ) ]
          in
          let exit_success = Node.make_if env true (Var has_key_id) in
          let no_key_node = Node.make_if env false (Var has_key_id) in
          let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badkey in
          start |~~> [exit_success; no_key_node] ;
          no_key_node |~~> [crash_node] ;
          [{start; exit_success; exit_failure= crash_node}]
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
    load_field env ret_var record_id (ErlangTypeName.tuple_elem field_no) tuple_typ
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
                load_field env one_id record_id
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
                Block.make_load env one_id (translate_atom_literal "undefined") any_typ ) ) )
  in
  let field_names = record_info.field_names in
  let field_ids = List.map ~f:(function _ -> mk_fresh_id ()) field_names in
  let field_blocks = List.map ~f:translate_one_field (List.zip_exn field_names field_ids) in
  let field_ids_and_types = List.map ~f:(fun id -> (Exp.Var id, any_typ)) field_ids in
  let args_and_types = (translate_atom_literal name, any_typ) :: field_ids_and_types in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_tuple) in
  let call_instruction =
    Sil.Call ((ret_var, any_typ), fun_exp, args_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (record_block @ field_blocks @ [call_block])


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
  let op_block =
    match op with
    | UBNot ->
        make_simple_op_block BNot
    | UMinus ->
        make_simple_op_block Neg
    | UNot ->
        make_simple_op_block LNot
  in
  Block.all env [block; op_block]


and translate_expression_variable (env : (_, _) Env.t) ret_var vname : Block.t =
  let (Env.Present procdesc) = env.procdesc in
  let procname = Procdesc.get_proc_name procdesc in
  let e = Exp.Lvar (Pvar.mk (Mangled.from_string vname) procname) in
  let load_instr = Sil.Load {id= ret_var; e; root_typ= any_typ; typ= any_typ; loc= env.location} in
  Block.make_instruction env [load_instr]


and translate_body (env : (_, _) Env.t) body : Block.t =
  let blocks =
    let f rev_blocks one_expression =
      let id = mk_fresh_id () in
      translate_expression_to_id env id one_expression :: rev_blocks
    in
    (* Last needs separate treatment to use its result as the overall result *)
    let f_last rev_blocks one_expression = translate_expression env one_expression :: rev_blocks in
    List.rev (IList.fold_last body ~init:[] ~f ~f_last)
  in
  Block.all env blocks


(** Assumes that the values on which patterns should be matched have been loaded into the
    identifiers listed in [values]. *)
and translate_case_clause (env : (_, _) Env.t) (values : Ident.t list)
    {Ast.line= _; patterns; guards; body} : Block.t =
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


let translate_one_function (env : (_, _) Env.t) cfg function_ clauses =
  let uf_name = Env.UnqualifiedFunction.of_ast function_ in
  let {Env.UnqualifiedFunction.T.name= function_name; arity} = uf_name in
  let name =
    let module_name = env.current_module in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let attributes =
    let default = ProcAttributes.default env.location.file name in
    let access : ProcAttributes.access = if Set.mem env.exports uf_name then Public else Private in
    let formals = List.init ~f:(fun i -> (mangled_arg i, any_typ)) arity in
    {default with access; formals; is_defined= true; loc= env.location; ret_type= any_typ}
  in
  let procdesc =
    let procdesc = Cfg.create_proc_desc cfg attributes in
    let start_node = Procdesc.create_node procdesc env.location Start_node [] in
    let exit_node = Procdesc.create_node procdesc env.location Exit_node [] in
    Procdesc.set_start_node procdesc start_node ;
    Procdesc.set_exit_node procdesc exit_node ;
    procdesc
  in
  let env =
    { env with
      procdesc= Env.Present procdesc
    ; result= Env.Present (Exp.Lvar (Pvar.get_ret_pvar name)) }
  in
  let idents, loads =
    let load (formal, typ) =
      let id = mk_fresh_id () in
      let pvar = Pvar.mk formal name in
      let load = Sil.Load {id; e= Exp.Lvar pvar; root_typ= typ; typ; loc= attributes.loc} in
      (id, load)
    in
    List.unzip (List.map ~f:load attributes.formals)
  in
  let ({start; exit_success; exit_failure} : Block.t) =
    let blocks = List.map ~f:(translate_case_clause env idents) clauses in
    Block.any env blocks
  in
  let () =
    (* Add a node that loads all values on which we pattern-match into idents. *)
    let loads_node = Node.make_stmt env ~kind:ErlangCaseClause loads in
    Procdesc.get_start_node procdesc |~~> [loads_node] ;
    loads_node |~~> [start]
  in
  let () =
    (* If all patterns fail, call special method *)
    let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_function_clause in
    exit_failure |~~> [crash_node] ;
    crash_node |~~> [Procdesc.get_exit_node procdesc]
  in
  exit_success |~~> [Procdesc.get_exit_node procdesc]


let translate_functions (env : (_, _) Env.t) cfg module_ =
  let f {Ast.line; simple_form} =
    let env = update_location line env in
    match simple_form with
    | Function {function_; clauses} ->
        translate_one_function env cfg function_ clauses
    | _ ->
        ()
  in
  List.iter module_ ~f ;
  DB.Results_dir.init env.location.file ;
  let tenv = Tenv.FileLocal (Tenv.create ()) in
  SourceFiles.add env.location.file cfg tenv None


let translate_module env module_ =
  let cfg = Cfg.create () in
  translate_functions env cfg module_
