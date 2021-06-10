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

let typ_of_name (name : ErlangTypeName.t) : Typ.t =
  Typ.mk (Tptr (Typ.mk (Tstruct (ErlangType name)), Pk_pointer))


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

type block = {start: Procdesc.Node.t; exit_success: Procdesc.Node.t; exit_failure: Procdesc.Node.t}

type environment =
  { current_module: module_name  (** used to qualify function names *)
  ; exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; location: Location.t  (** used to tag nodes and instructions being created *)
  ; procdesc: (Procdesc.t option[@sexp.opaque])  (** imperative, being built *)
  ; result: (Exp.t option[@sexp.opaque])  (** where to store the result value (if any) *) }
[@@deriving sexp_of]

let get_environment module_ : environment =
  let init =
    { current_module= Printf.sprintf "%s:unknown_module" __FILE__
    ; exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; location= Location.dummy
    ; procdesc= None
    ; result= None }
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
  let make env kind instructions =
    let procdesc = Option.value_exn env.procdesc in
    Procdesc.create_node procdesc env.location kind instructions


  let make_stmt env ?(kind = Procdesc.Node.Erlang) instructions =
    make env (Stmt_node kind) instructions


  let make_nop env = make_stmt env []

  let make_join env = make env Join_node []

  let make_throw env one_instruction = make env Procdesc.Node.throw_kind [one_instruction]

  let make_if env branch id =
    let prune_kind : Procdesc.Node.prune_node_kind =
      if branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch
    in
    let condition : Exp.t =
      if branch then Var id else UnOp (LNot, Var id, Some (Typ.mk (Tint IBool)))
    in
    let kind : Procdesc.Node.nodekind = Prune_node (branch, Ik_if, prune_kind) in
    let prune : Sil.instr = Prune (condition, env.location, branch, Ik_if) in
    make env kind [prune]
end

let make_block_success env =
  let exit_success, exit_failure = (Node.make_nop env, Node.make_nop env) in
  {start= exit_success; exit_success; exit_failure}


let make_block_failure env =
  let exit_success, exit_failure = (Node.make_nop env, Node.make_nop env) in
  {start= exit_failure; exit_success; exit_failure}


(** Makes one block of a list of blocks. Meant to be used only by the functions [all_blocks] and
    [any_block] defined immediately below. If [b] comes before [c] in the list [blocks], then an
    edge is added from [continue b] to [c.start]. For all blocks [b] in the list [blocks], an edge
    is added from [stop b] to [new_stop], where [new_stop] is a new node of type join. If there is
    only one block, then it is returned with no modification.*)
let sequence_blocks ~(continue : block -> Procdesc.Node.t) ~(stop : block -> Procdesc.Node.t) env
    (blocks : block list) =
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


let all_blocks env (blocks : block list) : block =
  match blocks with
  | [] ->
      make_block_success env
  | _ ->
      let continue b = b.exit_success in
      let stop b = b.exit_failure in
      let start, exit_success, exit_failure = sequence_blocks ~continue ~stop env blocks in
      {start; exit_success; exit_failure}


let any_block env (blocks : block list) : block =
  match blocks with
  | [] ->
      make_block_failure env
  | _ ->
      let continue b = b.exit_failure in
      let stop b = b.exit_success in
      let start, exit_failure, exit_success = sequence_blocks ~continue ~stop env blocks in
      {start; exit_success; exit_failure}


let has_type env ~result ~value (name : ErlangTypeName.t) : Sil.instr =
  let fun_exp : Exp.t = Const (Cfun BuiltinDecl.__instanceof) in
  let any = typ_of_name Any in
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


(** If the pattern-match succeeds, then the [exit_success] node is reached and the pattern variables
    are storing the corresponding values; otherwise, the [exit_failure] node is reached. *)
let rec translate_pattern env (value : Ident.t) {Ast.line; simple_expression} =
  let env = update_location line env in
  let any = typ_of_name Any in
  let procdesc = Option.value_exn env.procdesc in
  let procname = Procdesc.get_proc_name procdesc in
  match simple_expression with
  | Cons {head; tail} ->
      let id = Ident.create_fresh Ident.knormal in
      let start = Node.make_stmt env [has_type env ~result:id ~value Cons] in
      let right_type_node = Node.make_if env true id in
      let wrong_type_node = Node.make_if env false id in
      let load id field : Sil.instr =
        (* x=value.field *)
        let field = Fieldname.make (ErlangType Cons) field in
        Load
          { id
          ; e= Lfield (Var value, field, typ_of_name Cons)
          ; root_typ= any
          ; typ= any
          ; loc= env.location }
      in
      let head_value = Ident.create_fresh Ident.knormal in
      let tail_value = Ident.create_fresh Ident.knormal in
      let head_load = load head_value "head" in
      let tail_load = load tail_value "tail" in
      let unpack_node = Node.make_stmt env [head_load; tail_load] in
      let head_matcher = translate_pattern env head_value head in
      let tail_matcher = translate_pattern env tail_value tail in
      let submatcher = all_blocks env [head_matcher; tail_matcher] in
      let exit_failure = Node.make_nop env in
      start |~~> [right_type_node; wrong_type_node] ;
      right_type_node |~~> [unpack_node] ;
      unpack_node |~~> [submatcher.start] ;
      wrong_type_node |~~> [exit_failure] ;
      submatcher.exit_failure |~~> [exit_failure] ;
      {start; exit_success= submatcher.exit_success; exit_failure}
  | Nil ->
      let id = Ident.create_fresh Ident.knormal in
      let start = Node.make_stmt env [has_type env ~result:id ~value Nil] in
      let exit_success = Node.make_if env true id in
      let exit_failure = Node.make_if env false id in
      start |~~> [exit_success; exit_failure] ;
      {start; exit_success; exit_failure}
  | Variable vname when String.equal vname "_" ->
      make_block_success env
  | Variable vname ->
      let store : Sil.instr =
        let e1 : Exp.t = Lvar (Pvar.mk (Mangled.from_string vname) procname) in
        let e2 : Exp.t = Var value in
        Store {e1; root_typ= any; typ= any; e2; loc= env.location}
      in
      let exit_success = Node.make_stmt env [store] in
      let exit_failure = Node.make_nop env in
      {start= exit_success; exit_success; exit_failure}
  | _ ->
      (* TODO: Cover all cases. *)
      make_block_failure env


let translate_body env _body =
  (* TODO: This is a dummy implementation. *)
  make_block_success env


(** Assumes that the values on which patterns should be matched have been loaded into the
    identifiers listed in [values]. *)
let translate_case_clause env (values : Ident.t list) {Ast.line= _; patterns; guards= _; body} :
    block =
  let matchers_block =
    let f (one_value, one_pattern) = translate_pattern env one_value one_pattern in
    let matchers = List.map ~f (List.zip_exn values patterns) in
    all_blocks env matchers
  in
  let body_block = translate_body env body in
  (* TODO: Evaluate the guards. *)
  matchers_block.exit_success |~~> [body_block.start] ;
  let () =
    let procdesc = Option.value_exn env.procdesc in
    body_block.exit_failure |~~> [Procdesc.get_exit_node procdesc]
  in
  { start= matchers_block.start
  ; exit_failure= matchers_block.exit_failure
  ; exit_success= body_block.exit_success }


let translate_one_function env cfg function_ clauses =
  let uf_name = UnqualifiedFunction.of_ast function_ in
  let {UnqualifiedFunction.name= function_name; arity} = uf_name in
  let name =
    let module_name = env.current_module in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let any = typ_of_name Any in
  let attributes =
    let default = ProcAttributes.default env.location.file name in
    let access : ProcAttributes.access = if Set.mem env.exports uf_name then Public else Private in
    let formals = List.init ~f:(fun i -> (mangled_arg i, any)) arity in
    {default with access; formals; loc= env.location; ret_type= any}
  in
  let procdesc = Cfg.create_proc_desc cfg attributes in
  let env = {env with procdesc= Some procdesc; result= Some (Exp.Lvar (Pvar.get_ret_pvar name))} in
  let idents, loads =
    let load (formal, typ) =
      let id = Ident.create_fresh Ident.knormal in
      let pvar = Pvar.mk formal name in
      let load = Sil.Load {id; e= Exp.Lvar pvar; root_typ= typ; typ; loc= attributes.loc} in
      (id, load)
    in
    List.unzip (List.map ~f:load attributes.formals)
  in
  let {start; exit_success; exit_failure} =
    let blocks = List.map ~f:(translate_case_clause env idents) clauses in
    any_block env blocks
  in
  let () =
    (* Add a node that loads all values on which we pattern-match into idents. *)
    let loads_node = Node.make_stmt env ~kind:ErlangCaseClause loads in
    Procdesc.get_start_node procdesc |~~> [loads_node] ;
    loads_node |~~> [start]
  in
  let () =
    (* If all patterns fail, call BuiltinDecl.__erlang_pattern_fail *)
    let crash_instruction =
      let ret_var = Ident.create_fresh Ident.knormal (* not used: nothing returned *) in
      let pattern_fail_fun = Exp.Const (Cfun BuiltinDecl.__erlang_pattern_fail) in
      Sil.Call ((ret_var, any), pattern_fail_fun, [], attributes.loc, CallFlags.default)
    in
    let crash_node = Node.make_throw env crash_instruction in
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
  Cfg.store env.location.file cfg ;
  SourceFiles.add env.location.file cfg Tenv.Global None


let translate_module module_ =
  let cfg = Cfg.create () in
  let env = get_environment module_ in
  translate_functions env cfg module_
