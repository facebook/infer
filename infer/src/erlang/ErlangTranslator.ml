(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module L = Logging

let mangled_arg (n : int) : Mangled.t = Mangled.from_string (Printf.sprintf "$arg%d" n)

let typ_of_name (name : ErlangTypeName.t) : Typ.t = Typ.mk (Tstruct (ErlangType name))

let ptr_typ_of_name (name : ErlangTypeName.t) : Typ.t = Typ.mk (Tptr (typ_of_name name, Pk_pointer))

module UnqualifiedFunction = struct
  module T = struct
    type t = {name: string; arity: int} [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let of_ast (f : Ast.function_) : t =
    match f with
    | {module_= ModuleMissing; function_= FunctionName name; arity} ->
        {name; arity}
    | _ ->
        L.die InternalError "expected unqualified function"
end

type module_name = string [@@deriving sexp_of]

type absent = Absent

type 'a present = Present of 'a

type record_field_info = {index: int; initializer_: Ast.expression option} [@@deriving sexp_of]

type record_info = {field_names: string list; field_info: record_field_info String.Map.t}
[@@deriving sexp_of]

type ('procdesc, 'result) environment =
  { current_module: module_name  (** used to qualify function names *)
  ; exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; records: record_info String.Map.t  (** used to get fields, indexes and initializers *)
  ; location: Location.t  (** used to tag nodes and instructions being created *)
  ; procdesc: ('procdesc[@sexp.opaque])
  ; result: ('result[@sexp.opaque]) }
[@@deriving sexp_of]

let get_environment module_ =
  let init =
    { current_module= Printf.sprintf "%s:unknown_module" __FILE__
    ; exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; records= String.Map.empty
    ; location= Location.dummy
    ; procdesc= Absent
    ; result= Absent }
  in
  let f env (form : Ast.form) =
    match form.simple_form with
    | Export functions ->
        let f exports function_ = Set.add exports (UnqualifiedFunction.of_ast function_) in
        let exports = List.fold ~init:env.exports ~f functions in
        {env with exports}
    | Import {module_name; functions} ->
        let f imports function_ =
          let key = UnqualifiedFunction.of_ast function_ in
          match Map.add ~key ~data:module_name imports with
          | `Ok imports ->
              imports
          | `Duplicate ->
              L.die InternalError "repeated import: %s/%d" key.name key.arity
        in
        let imports = List.fold ~init:env.imports ~f functions in
        {env with imports}
    | Record {name; fields} -> (
        let process_one_field one_index map (one_field : Ast.record_field) =
          (* Tuples are indexed from 1 and the first one is the name, hence start from 2 *)
          match
            Map.add ~key:one_field.field_name
              ~data:{index= one_index + 2; initializer_= one_field.initializer_}
              map
          with
          | `Ok map ->
              map
          | `Duplicate ->
              L.die InternalError "repeated field in record: %s" one_field.field_name
        in
        let field_info = List.foldi ~init:String.Map.empty ~f:process_one_field fields in
        let field_names = List.map ~f:(fun (rf : Ast.record_field) -> rf.field_name) fields in
        match Map.add ~key:name ~data:{field_names; field_info} env.records with
        | `Ok records ->
            {env with records}
        | `Duplicate ->
            L.die InternalError "repeated record: %s" name )
    | Module current_module ->
        {env with current_module}
    | File {path} ->
        let file = SourceFile.create path in
        let location = Location.none file in
        {env with location}
    | _ ->
        env
  in
  List.fold ~init ~f module_


let ( |~~> ) from to_ = Procdesc.set_succs from ~normal:(Some to_) ~exn:None

let update_location line env =
  let location = {env.location with line; col= -1} in
  {env with location}


(** Groups several helpers used to create nodes. *)
module Node = struct
  let make (env : (Procdesc.t present, _) environment) kind instructions =
    let (Present procdesc) = env.procdesc in
    Procdesc.create_node procdesc env.location kind instructions


  let make_stmt env ?(kind = Procdesc.Node.Erlang) instructions =
    make env (Stmt_node kind) instructions


  let make_load env id e typ =
    let (Present procdesc) = env.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let temp_pvar = Pvar.mk_tmp "LoadBlock" procname in
    let instructions =
      [ Sil.Store {e1= Lvar temp_pvar; e2= e; root_typ= typ; typ; loc= env.location}
      ; Sil.Load {id; e= Lvar temp_pvar; root_typ= typ; typ; loc= env.location} ]
    in
    make_stmt env ~kind:ErlangExpression instructions


  let make_nop env = make_stmt env []

  let make_join env = make env Join_node []

  let make_throw env one_instruction = make env Procdesc.Node.throw_kind [one_instruction]

  let make_if env branch expr =
    let prune_kind : Procdesc.Node.prune_node_kind =
      if branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch
    in
    let condition : Exp.t =
      if branch then expr else UnOp (LNot, expr, Some (Typ.mk (Tint IBool)))
    in
    let kind : Procdesc.Node.nodekind = Prune_node (branch, Ik_if, prune_kind) in
    let prune : Sil.instr = Prune (condition, env.location, branch, Ik_if) in
    make env kind [prune]


  let make_fail env fail_function =
    let any = typ_of_name Any in
    let crash_instruction =
      let ret_var = Ident.create_fresh Ident.knormal (* not used: nothing returned *) in
      let pattern_fail_fun = Exp.Const (Cfun fail_function) in
      Sil.Call ((ret_var, any), pattern_fail_fun, [], env.location, CallFlags.default)
    in
    make_throw env crash_instruction
end

(** Groups several helpers used to create blocks. *)
module Block = struct
  type t = {start: Procdesc.Node.t; exit_success: Procdesc.Node.t; exit_failure: Procdesc.Node.t}

  let make_success env =
    let exit_success, exit_failure = (Node.make_nop env, Node.make_nop env) in
    {start= exit_success; exit_success; exit_failure}


  let make_failure env =
    let exit_success, exit_failure = (Node.make_nop env, Node.make_nop env) in
    {start= exit_failure; exit_success; exit_failure}


  (** Makes one block of a list of blocks. Meant to be used only by the functions [all_blocks] and
      [any_block] defined immediately below. If [b] comes before [c] in the list [blocks], then an
      edge is added from [continue b] to [c.start]. For all blocks [b] in the list [blocks], an edge
      is added from [stop b] to [new_stop], where [new_stop] is a new node of type join. If there is
      only one block, then it is returned with no modification.*)
  let sequence ~(continue : t -> Procdesc.Node.t) ~(stop : t -> Procdesc.Node.t) env
      (blocks : t list) =
    match blocks with
    | [] ->
        L.die InternalError "blocks should not be empty"
    | [one_block] ->
        (one_block.start, continue one_block, stop one_block)
    | first_block :: next_blocks ->
        let continue_node =
          let f previous b =
            previous |~~> [b.start] ;
            continue b
          in
          List.fold ~f ~init:(continue first_block) next_blocks
        in
        let new_stop = Node.make_join env in
        List.iter ~f:(fun b -> stop b |~~> [new_stop]) blocks ;
        (first_block.start, continue_node, new_stop)


  (** Chain a list of blocks together in a conjunctive style: a failure in any block leads to a
      global failure, and successes lead to the next block. *)
  let all env (blocks : t list) : t =
    match blocks with
    | [] ->
        make_success env
    | _ ->
        let continue b = b.exit_success in
        let stop b = b.exit_failure in
        let start, exit_success, exit_failure = sequence ~continue ~stop env blocks in
        {start; exit_success; exit_failure}


  (** Chain a list of blocks together in a disjunctive style: a success in any block leads to a
      global success, and failures lead to the next block. *)
  let any env (blocks : t list) : t =
    match blocks with
    | [] ->
        make_failure env
    | _ ->
        let continue b = b.exit_failure in
        let stop b = b.exit_success in
        let start, exit_failure, exit_success = sequence ~continue ~stop env blocks in
        {start; exit_success; exit_failure}


  let make_instruction env instructions =
    let exit_success = Node.make_stmt env ~kind:ErlangExpression instructions in
    let exit_failure = Node.make_nop env in
    {start= exit_success; exit_success; exit_failure}


  let make_load env id e typ =
    let exit_success = Node.make_load env id e typ in
    let exit_failure = Node.make_nop env in
    {start= exit_success; exit_success; exit_failure}


  (** Make a branch based on the condition: go to success if true, go to failure if false *)
  let make_branch env condition =
    let start = Node.make_nop env in
    let exit_success = Node.make_if env true condition in
    let exit_failure = Node.make_if env false condition in
    start |~~> [exit_success; exit_failure] ;
    {start; exit_success; exit_failure}
end

let has_type env ~result ~value (name : ErlangTypeName.t) : Sil.instr =
  let fun_exp : Exp.t = Const (Cfun BuiltinDecl.__instanceof) in
  let any = ptr_typ_of_name Any in
  let args : (Exp.t * Typ.t) list =
    [ (Var value, any)
    ; ( Sizeof
          { typ= typ_of_name name
          ; nbytes= None
          ; dynamic_length= None
          ; subtype= Subtype.subtypes_instof }
      , any ) ]
  in
  Call ((result, Typ.mk (Tint IBool)), fun_exp, args, env.location, CallFlags.default)


let translate_atom_literal (atom : string) : Exp.t =
  (* With this hack, an atom may accidentaly be considered equal to an unrelated integer.
      The [lsl] below makes this less likely. Proper fix is TODO (T93513105). *)
  let hash = String.hash atom lsl 16 in
  Exp.Const (Cint (IntLit.of_int hash))


(** into_id=value_id.field_name *)
let load_field env into_id value_id field_name typ : Sil.instr =
  let any = ptr_typ_of_name Any in
  let field = Fieldname.make (ErlangType typ) field_name in
  Load
    { id= into_id
    ; e= Lfield (Var value_id, field, typ_of_name typ)
    ; root_typ= any
    ; typ= any
    ; loc= env.location }


let match_record_name env value name record_info : Block.t =
  let tuple_size = 1 + List.length record_info.field_names in
  let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
  let is_right_type_id = Ident.create_fresh Ident.knormal in
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value tuple_typ] in
  let right_type_node = Node.make_if env true (Var is_right_type_id) in
  let wrong_type_node = Node.make_if env false (Var is_right_type_id) in
  let name_id = Ident.create_fresh Ident.knormal in
  let name_load = load_field env name_id value (ErlangTypeName.tuple_elem 1) tuple_typ in
  let unpack_node = Node.make_stmt env [name_load] in
  let name_cond = Exp.BinOp (Eq, Var name_id, translate_atom_literal name) in
  let right_name_node = Node.make_if env true name_cond in
  let wrong_name_node = Node.make_if env false name_cond in
  let exit_failure = Node.make_nop env in
  start |~~> [right_type_node; wrong_type_node] ;
  right_type_node |~~> [unpack_node] ;
  unpack_node |~~> [right_name_node; wrong_name_node] ;
  wrong_type_node |~~> [exit_failure] ;
  wrong_name_node |~~> [exit_failure] ;
  {start; exit_success= right_name_node; exit_failure}


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
      Block.make_failure env


and translate_pattern_cons env value head tail : Block.t =
  let is_right_type_id = Ident.create_fresh Ident.knormal in
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value Cons] in
  let right_type_node = Node.make_if env true (Var is_right_type_id) in
  let wrong_type_node = Node.make_if env false (Var is_right_type_id) in
  let head_value = Ident.create_fresh Ident.knormal in
  let tail_value = Ident.create_fresh Ident.knormal in
  let head_load = load_field env head_value value ErlangTypeName.cons_head Cons in
  let tail_load = load_field env tail_value value ErlangTypeName.cons_tail Cons in
  let unpack_node = Node.make_stmt env [head_load; tail_load] in
  let head_matcher = translate_pattern env head_value head in
  let tail_matcher = translate_pattern env tail_value tail in
  let submatcher = Block.all env [head_matcher; tail_matcher] in
  let exit_failure = Node.make_nop env in
  start |~~> [right_type_node; wrong_type_node] ;
  right_type_node |~~> [unpack_node] ;
  unpack_node |~~> [submatcher.start] ;
  wrong_type_node |~~> [exit_failure] ;
  submatcher.exit_failure |~~> [exit_failure] ;
  {start; exit_success= submatcher.exit_success; exit_failure}


and translate_pattern_nil env value : Block.t =
  let id = Ident.create_fresh Ident.knormal in
  let start = Node.make_stmt env [has_type env ~result:id ~value Nil] in
  let exit_success = Node.make_if env true (Var id) in
  let exit_failure = Node.make_if env false (Var id) in
  start |~~> [exit_success; exit_failure] ;
  {start; exit_success; exit_failure}


and translate_pattern_map env value updates : Block.t =
  let any = ptr_typ_of_name Any in
  let is_right_type_id = Ident.create_fresh Ident.knormal in
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value Map] in
  let right_type_node = Node.make_if env true (Var is_right_type_id) in
  let wrong_type_node = Node.make_if env false (Var is_right_type_id) in
  (* For each update, check if key is there and if yes, match against value *)
  let make_submatcher (one_update : Ast.association) =
    let has_key_id = Ident.create_fresh Ident.knormal in
    let key_id = Ident.create_fresh Ident.knormal in
    let key_expr_block =
      translate_expression {env with result= Present (Exp.Var key_id)} one_update.key
    in
    let has_key_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_has_key) in
    let args = [(Exp.Var key_id, any); (Exp.Var value, any)] in
    let has_key_block : Block.t =
      let start =
        Node.make_stmt env
          [Sil.Call ((has_key_id, any), has_key_fun_exp, args, env.location, CallFlags.default)]
      in
      let exit_success = Node.make_if env true (Var has_key_id) in
      let exit_failure = Node.make_if env false (Var has_key_id) in
      start |~~> [exit_success; exit_failure] ;
      {start; exit_success; exit_failure}
    in
    let value_id = Ident.create_fresh Ident.knormal in
    let lookup_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_lookup) in
    let lookup_block =
      Block.make_instruction env
        [Sil.Call ((value_id, any), lookup_fun_exp, args, env.location, CallFlags.default)]
    in
    let match_value_block = translate_pattern env value_id one_update.value in
    Block.all env [key_expr_block; has_key_block; lookup_block; match_value_block]
  in
  let submatchers = Block.all env (List.map ~f:make_submatcher updates) in
  start |~~> [right_type_node; wrong_type_node] ;
  right_type_node |~~> [submatchers.start] ;
  wrong_type_node |~~> [submatchers.exit_failure] ;
  {start; exit_success= submatchers.exit_success; exit_failure= submatchers.exit_failure}


and translate_pattern_record_index env value name field : Block.t =
  match String.Map.find env.records name with
  | None ->
      L.debug Capture Verbose "@[Unknown record %s@." name ;
      Block.make_failure env
  | Some record_info ->
      let field_info = String.Map.find_exn record_info.field_info field in
      let index_expr = Exp.Const (Cint (IntLit.of_int field_info.index)) in
      Block.make_branch env (Exp.BinOp (Eq, Var value, index_expr))


and translate_pattern_record_update env value name updates : Block.t =
  match String.Map.find env.records name with
  | None ->
      L.debug Capture Verbose "@[Unknown record %s@." name ;
      Block.make_failure env
  | Some record_info ->
      (* Match the type and the record name *)
      let record_name_matcher = match_record_name env value name record_info in
      (* Match each specified field *)
      let tuple_size = 1 + List.length record_info.field_names in
      let tuple_typ : ErlangTypeName.t = Tuple tuple_size in
      let make_one_field_matcher (one_update : Ast.record_update) =
        match one_update.field with
        | Some name ->
            let field_info = String.Map.find_exn record_info.field_info name in
            let value_id = Ident.create_fresh Ident.knormal in
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
  let is_right_type_id = Ident.create_fresh Ident.knormal in
  let tuple_typ : ErlangTypeName.t = Tuple (List.length exprs) in
  let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value tuple_typ] in
  let right_type_node = Node.make_if env true (Var is_right_type_id) in
  let wrong_type_node = Node.make_if env false (Var is_right_type_id) in
  let value_ids = List.map ~f:(function _ -> Ident.create_fresh Ident.knormal) exprs in
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
  start |~~> [right_type_node; wrong_type_node] ;
  right_type_node |~~> [unpack_node] ;
  unpack_node |~~> [submatcher.start] ;
  wrong_type_node |~~> [exit_failure] ;
  submatcher.exit_failure |~~> [exit_failure] ;
  {start; exit_success= submatcher.exit_success; exit_failure}


and translate_pattern_unary_expression env value line simple_expression : Block.t =
  (* Unary op pattern must evaluate to number, so just delegate to expression translation *)
  let id = Ident.create_fresh Ident.knormal in
  let expr_block =
    translate_expression {env with result= Present (Exp.Var id)} {Ast.line; simple_expression}
  in
  let branch_block = Block.make_branch env (Exp.BinOp (Eq, Var value, Var id)) in
  Block.all env [expr_block; branch_block]


and translate_pattern_variable env value vname : Block.t =
  match vname with
  | "_" ->
      Block.make_success env
  | _ ->
      let (Present procdesc) = env.procdesc in
      let procname = Procdesc.get_proc_name procdesc in
      let any = ptr_typ_of_name Any in
      let store : Sil.instr =
        let e1 : Exp.t = Lvar (Pvar.mk (Mangled.from_string vname) procname) in
        let e2 : Exp.t = Var value in
        Store {e1; root_typ= any; typ= any; e2; loc= env.location}
      in
      let exit_success = Node.make_stmt env [store] in
      let exit_failure = Node.make_nop env in
      {start= exit_success; exit_success; exit_failure}


and translate_guard_expression env (expression : Ast.expression) : Ident.t * Block.t =
  let id = Ident.create_fresh Ident.knormal in
  let block = translate_expression {env with result= Present (Exp.Var id)} expression in
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
  let any = ptr_typ_of_name Any in
  let (Present result) = env.result in
  let (Present procdesc) = env.procdesc in
  let procname = Procdesc.get_proc_name procdesc in
  let ret_var =
    match result with Exp.Var ret_var -> ret_var | _ -> Ident.create_fresh Ident.knormal
  in
  let expression_block : Block.t =
    match simple_expression with
    | BinaryOperator (e1, op, e2) -> (
        let id1 = Ident.create_fresh Ident.knormal in
        let id2 = Ident.create_fresh Ident.knormal in
        let block1 : Block.t = translate_expression {env with result= Present (Exp.Var id1)} e1 in
        let block2 : Block.t = translate_expression {env with result= Present (Exp.Var id2)} e2 in
        let make_simple_eager sil_op =
          let op_block = Block.make_load env ret_var (Exp.BinOp (sil_op, Var id1, Var id2)) any in
          Block.all env [block1; block2; op_block]
        in
        let make_short_circuit_logic ~short_circuit_when_lhs_is =
          let start = Node.make_nop env in
          let id1_cond = Node.make_if env short_circuit_when_lhs_is (Var id1) in
          let id2_cond = Node.make_if env (not short_circuit_when_lhs_is) (Var id1) in
          let store_id1 = Node.make_load env ret_var (Var id1) any in
          let store_id2 = Node.make_load env ret_var (Var id2) any in
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
            let op_block = Block.make_load env ret_var expr any in
            Block.all env [block1; block2; op_block]
        | todo ->
            L.debug Capture Verbose
              "@[todo ErlangTranslator.translate_expression(BinaryOperator) %s@."
              (Sexp.to_string (Ast.sexp_of_binary_operator todo)) ;
            Block.all env [block1; block2; Block.make_success env] )
    | Block body ->
        translate_body env body
    | Call
        { module_= None
        ; function_= {Ast.line= _; simple_expression= Literal (Atom function_name)}
        ; args } ->
        let arity = List.length args in
        let callee_procname =
          let module_name =
            let uf_name = {UnqualifiedFunction.name= function_name; arity} in
            match UnqualifiedFunction.Map.find env.imports uf_name with
            | Some module_name ->
                module_name
            | None ->
                env.current_module
          in
          Procname.make_erlang ~module_name ~function_name ~arity
        in
        let args_with_ids = List.map ~f:(fun a -> (a, Ident.create_fresh Ident.knormal)) args in
        let args_blocks =
          let f (one_arg_expression, one_arg_ret_var) =
            let result = Present (Exp.Var one_arg_ret_var) in
            translate_expression {env with result} one_arg_expression
          in
          List.map ~f args_with_ids
        in
        let fun_exp = Exp.Const (Cfun callee_procname) in
        let args_ids_and_types =
          List.map ~f:(function _, id -> (Exp.Var id, any)) args_with_ids
        in
        let call_instruction =
          Sil.Call ((ret_var, any), fun_exp, args_ids_and_types, env.location, CallFlags.default)
        in
        let call_block = Block.make_instruction env [call_instruction] in
        Block.all env (args_blocks @ [call_block])
    | Case {expression; cases} ->
        let id = Ident.create_fresh Ident.knormal in
        let expr_block = translate_expression {env with result= Present (Exp.Var id)} expression in
        let blocks = Block.any env (List.map ~f:(translate_case_clause env [id]) cases) in
        let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_case_clause in
        blocks.exit_failure |~~> [crash_node] ;
        let blocks = {blocks with exit_failure= crash_node} in
        Block.all env [expr_block; blocks]
    | Cons {head; tail} ->
        let head_var = Ident.create_fresh Ident.knormal in
        let tail_var = Ident.create_fresh Ident.knormal in
        let head_block = translate_expression {env with result= Present (Exp.Var head_var)} head in
        let tail_block = translate_expression {env with result= Present (Exp.Var tail_var)} tail in
        let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_cons) in
        let args : (Exp.t * Typ.t) list = [(Var head_var, any); (Var tail_var, any)] in
        let call_instruction =
          Sil.Call ((ret_var, any), fun_exp, args, env.location, CallFlags.default)
        in
        Block.all env [head_block; tail_block; Block.make_instruction env [call_instruction]]
    | If clauses ->
        let blocks = Block.any env (List.map ~f:(translate_case_clause env []) clauses) in
        let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_if_clause in
        blocks.exit_failure |~~> [crash_node] ;
        let blocks = {blocks with exit_failure= crash_node} in
        blocks
    | Literal (Atom atom) ->
        let e = translate_atom_literal atom in
        Block.make_load env ret_var e any
    | Literal (Int i) ->
        let e = Exp.Const (Cint (IntLit.of_string i)) in
        Block.make_load env ret_var e any
    | Literal (String s) ->
        let e = Exp.Const (Cstr s) in
        Block.make_load env ret_var e any
    | Map {map= None; updates} ->
        translate_expression_map_create env ret_var updates
    | Map {map= Some map; updates} ->
        translate_expression_map_update env ret_var map updates
    | Match {pattern; body} ->
        let body_block = translate_expression {env with result= Present (Exp.Var ret_var)} body in
        let pattern_block = translate_pattern env ret_var pattern in
        let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badmatch in
        pattern_block.exit_failure |~~> [crash_node] ;
        let pattern_block = {pattern_block with exit_failure= crash_node} in
        Block.all env [body_block; pattern_block]
    | Nil ->
        let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_nil) in
        let instruction = Sil.Call ((ret_var, any), fun_exp, [], env.location, CallFlags.default) in
        Block.make_instruction env [instruction]
    | RecordAccess {record; name; field} -> (
      (* Under the hood, a record is a tagged tuple, the first element is the name,
         and then the fields follow in the order as in the record definition. *)
      match String.Map.find env.records name with
      | None ->
          L.debug Capture Verbose "@[Unknown record %s@." name ;
          Block.make_success env
      | Some record_info ->
          let record_id = Ident.create_fresh Ident.knormal in
          let record_block =
            let result = Present (Exp.Var record_id) in
            let value_block = translate_expression {env with result} record in
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
          Block.all env [record_block; load_block] )
    | RecordIndex {name; field} -> (
      match String.Map.find env.records name with
      | None ->
          L.debug Capture Verbose "@[Unknown record %s@." name ;
          Block.make_success env
      | Some record_info ->
          let field_info = String.Map.find_exn record_info.field_info field in
          let expr = Exp.Const (Cint (IntLit.of_int field_info.index)) in
          Block.make_load env ret_var expr any )
    | RecordUpdate {record; name; updates} -> (
      (* Under the hood, a record is a tagged tuple, the first element is the name,
         and then the fields follow in the order as in the record definition. *)
      match String.Map.find env.records name with
      | None ->
          L.debug Capture Verbose "@[Unknown record %s@." name ;
          Block.make_success env
      | Some record_info ->
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
          let record_id = Ident.create_fresh Ident.knormal in
          let record_block =
            match record with
            | Some expr ->
                let result = Present (Exp.Var record_id) in
                let value_block = translate_expression {env with result} expr in
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
                let result = Present (Exp.Var one_id) in
                translate_expression {env with result} expr
            | None -> (
              (* (2) Check if field is set using 'everything else' *)
              match String.Map.find updates_map "_" with
              | Some expr ->
                  let result = Present (Exp.Var one_id) in
                  translate_expression {env with result} expr
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
                        let result = Present (Exp.Var one_id) in
                        translate_expression {env with result} expr
                    | None ->
                        (* (5) Finally, it's undefined *)
                        Block.make_load env one_id (translate_atom_literal "undefined") any ) ) )
          in
          let field_names = record_info.field_names in
          let field_ids =
            List.map ~f:(function _ -> Ident.create_fresh Ident.knormal) field_names
          in
          let field_blocks = List.map ~f:translate_one_field (List.zip_exn field_names field_ids) in
          let field_ids_and_types = List.map ~f:(fun id -> (Exp.Var id, any)) field_ids in
          let args_and_types = (translate_atom_literal name, any) :: field_ids_and_types in
          let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_tuple) in
          let call_instruction =
            Sil.Call ((ret_var, any), fun_exp, args_and_types, env.location, CallFlags.default)
          in
          let call_block = Block.make_instruction env [call_instruction] in
          Block.all env (record_block @ field_blocks @ [call_block]) )
    | Tuple exprs ->
        let exprs_with_ids = List.map ~f:(fun e -> (e, Ident.create_fresh Ident.knormal)) exprs in
        let expr_blocks =
          let f (one_expr, one_id) =
            let result = Present (Exp.Var one_id) in
            translate_expression {env with result} one_expr
          in
          List.map ~f exprs_with_ids
        in
        let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_make_tuple) in
        let exprs_ids_and_types =
          List.map ~f:(function _, id -> (Exp.Var id, any)) exprs_with_ids
        in
        let call_instruction =
          Sil.Call ((ret_var, any), fun_exp, exprs_ids_and_types, env.location, CallFlags.default)
        in
        let call_block = Block.make_instruction env [call_instruction] in
        Block.all env (expr_blocks @ [call_block])
    | UnaryOperator (op, e) ->
        let id = Ident.create_fresh Ident.knormal in
        let block = translate_expression {env with result= Present (Exp.Var id)} e in
        let make_simple_op_block sil_op =
          Block.make_load env ret_var (Exp.UnOp (sil_op, Var id, None)) any
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
    | Variable vname ->
        let e = Exp.Lvar (Pvar.mk (Mangled.from_string vname) procname) in
        let load_instr = Sil.Load {id= ret_var; e; root_typ= any; typ= any; loc= env.location} in
        Block.make_instruction env [load_instr]
    | todo ->
        L.debug Capture Verbose "@[todo ErlangTranslator.translate_expression %s@."
          (Sexp.to_string (Ast.sexp_of_simple_expression todo)) ;
        Block.make_success env
  in
  (* Add extra nodes/instructions to store return value if needed *)
  match result with
  | Exp.Var _ ->
      expression_block
  | _ ->
      let store_instr =
        Sil.Store {e1= result; root_typ= any; typ= any; e2= Var ret_var; loc= env.location}
      in
      let store_block = Block.make_instruction env [store_instr] in
      Block.all env [expression_block; store_block]


and translate_expression_map_create env ret_var updates : Block.t =
  (* Get keys and values as an alternating list of expressions: [K1; V1; K2; V2; ...] *)
  let exprs = List.concat_map ~f:(fun (a : Ast.association) -> [a.key; a.value]) updates in
  let exprs_with_ids = List.map ~f:(fun e -> (e, Ident.create_fresh Ident.knormal)) exprs in
  let expr_blocks =
    let translate_one_expr (one_expr, one_id) =
      translate_expression {env with result= Present (Exp.Var one_id)} one_expr
    in
    List.map ~f:translate_one_expr exprs_with_ids
  in
  let any = ptr_typ_of_name Any in
  let fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_create) in
  let exprs_ids_and_types = List.map ~f:(function _, id -> (Exp.Var id, any)) exprs_with_ids in
  let call_instruction =
    Sil.Call ((ret_var, any), fun_exp, exprs_ids_and_types, env.location, CallFlags.default)
  in
  let call_block = Block.make_instruction env [call_instruction] in
  Block.all env (expr_blocks @ [call_block])


and translate_expression_map_update env ret_var map updates : Block.t =
  let any = ptr_typ_of_name Any in
  let map_id = Ident.create_fresh Ident.knormal in
  let map_block = translate_expression {env with result= Present (Exp.Var map_id)} map in
  let check_map_type_block : Block.t =
    let is_right_type_id = Ident.create_fresh Ident.knormal in
    let start = Node.make_stmt env [has_type env ~result:is_right_type_id ~value:map_id Map] in
    let right_type_node = Node.make_if env true (Var is_right_type_id) in
    let wrong_type_node = Node.make_if env false (Var is_right_type_id) in
    let crash_node = Node.make_fail env BuiltinDecl.__erlang_error_badmap in
    start |~~> [right_type_node; wrong_type_node] ;
    wrong_type_node |~~> [crash_node] ;
    {start; exit_success= right_type_node; exit_failure= crash_node}
  in
  (* Translate updates one-by-one, also check key if exact association *)
  let translate_update (one_update : Ast.association) =
    let key_id = Ident.create_fresh Ident.knormal in
    let value_id = Ident.create_fresh Ident.knormal in
    let key_expr_block =
      translate_expression {env with result= Present (Exp.Var key_id)} one_update.key
    in
    let value_expr_block =
      translate_expression {env with result= Present (Exp.Var value_id)} one_update.value
    in
    let update_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_update) in
    let update_args = [(Exp.Var key_id, any); (Exp.Var value_id, any); (Exp.Var map_id, any)] in
    let update_block =
      Block.make_instruction env
        [Sil.Call ((ret_var, any), update_fun_exp, update_args, env.location, CallFlags.default)]
    in
    let has_key_block : Block.t list =
      match one_update.kind with
      | Arrow ->
          []
      | Exact ->
          let has_key_id = Ident.create_fresh Ident.knormal in
          let has_key_fun_exp = Exp.Const (Cfun BuiltinDecl.__erlang_map_has_key) in
          let has_key_args = [(Exp.Var key_id, any); (Exp.Var map_id, any)] in
          let start =
            Node.make_stmt env
              [ Sil.Call
                  ((has_key_id, any), has_key_fun_exp, has_key_args, env.location, CallFlags.default)
              ]
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


and translate_body env body : Block.t =
  let blocks =
    let f rev_blocks one_expression =
      let id = Ident.create_fresh Ident.knormal in
      let env = {env with result= Present (Exp.Var id)} in
      translate_expression env one_expression :: rev_blocks
    in
    let f_last rev_blocks one_expression = translate_expression env one_expression :: rev_blocks in
    List.rev (IList.fold_last body ~init:[] ~f ~f_last)
  in
  Block.all env blocks


(** Assumes that the values on which patterns should be matched have been loaded into the
    identifiers listed in [values]. *)
and translate_case_clause env (values : Ident.t list) {Ast.line= _; patterns; guards; body} :
    Block.t =
  let f (one_value, one_pattern) = translate_pattern env one_value one_pattern in
  let matchers = List.map ~f (List.zip_exn values patterns) in
  let guard_block = translate_guard_sequence env guards in
  let matchers_and_guards = Block.all env [Block.all env matchers; guard_block] in
  let body_block = translate_body env body in
  matchers_and_guards.exit_success |~~> [body_block.start] ;
  let () =
    let (Present procdesc) = env.procdesc in
    body_block.exit_failure |~~> [Procdesc.get_exit_node procdesc]
  in
  { start= matchers_and_guards.start
  ; exit_failure= matchers_and_guards.exit_failure
  ; exit_success= body_block.exit_success }


let translate_one_function env cfg function_ clauses =
  let uf_name = UnqualifiedFunction.of_ast function_ in
  let {UnqualifiedFunction.name= function_name; arity} = uf_name in
  let name =
    let module_name = env.current_module in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let any = ptr_typ_of_name Any in
  let attributes =
    let default = ProcAttributes.default env.location.file name in
    let access : ProcAttributes.access = if Set.mem env.exports uf_name then Public else Private in
    let formals = List.init ~f:(fun i -> (mangled_arg i, any)) arity in
    {default with access; formals; is_defined= true; loc= env.location; ret_type= any}
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
    {env with procdesc= Present procdesc; result= Present (Exp.Lvar (Pvar.get_ret_pvar name))}
  in
  let idents, loads =
    let load (formal, typ) =
      let id = Ident.create_fresh Ident.knormal in
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


let translate_functions env cfg module_ =
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


let translate_module module_ =
  let cfg = Cfg.create () in
  let env = get_environment module_ in
  translate_functions env cfg module_
