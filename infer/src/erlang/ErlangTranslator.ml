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
  ; procdesc: (Procdesc.t option[@sexp.opaque])  (** imperative, being built *)
  ; result: (Exp.t option[@sexp.opaque])  (** where to store the result value (if any) *)
  ; source: (SourceFile.t[@sexp.opaque])  (** used to add location information *) }
[@@deriving sexp_of]

let get_environment module_ : environment =
  let init =
    { exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; current_module= Printf.sprintf "%s:unknown_module" __FILE__
    ; source= SourceFile.invalid __FILE__
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
        {env with source= SourceFile.create path}
    | _ ->
        env
  in
  List.fold ~init ~f module_


let ( |~~> ) from to_ = Procdesc.set_succs from ~normal:(Some to_) ~exn:None

(** Groups several helpers used to create nodes. *)
module Node = struct
  let make ?line ?loc env kind instructions =
    let procdesc = Option.value_exn env.procdesc in
    let loc =
      match (loc, line) with
      | Some loc, _ ->
          loc
      | None, Some line ->
          {Location.line; col= -1; file= env.source}
      | None, None ->
          Procdesc.get_loc procdesc
    in
    Procdesc.create_node procdesc loc kind instructions


  let make_stmt ?line ?loc env (kind : Procdesc.Node.stmt_nodekind) instructions =
    make ?line ?loc env (Stmt_node kind) instructions


  let make_nop ?line ?loc env = make_stmt ?line ?loc env MethodBody []

  let make_join ?line ?loc env = make ?line ?loc env Join_node []

  let make_throw ?line ?loc env one_instruction =
    make ?line ?loc env Procdesc.Node.throw_kind [one_instruction]


  let make_if ?line ?loc env branch one_instruction =
    let prune_kind : Procdesc.Node.prune_node_kind =
      if branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch
    in
    let kind : Procdesc.Node.nodekind = Prune_node (branch, Ik_switch, prune_kind) in
    make ?line ?loc env kind [one_instruction]
end

let make_block_success ?line ?loc env =
  let exit_success, exit_failure = (Node.make_nop ?line ?loc env, Node.make_nop ?line ?loc env) in
  {start= exit_success; exit_success; exit_failure}


let make_block_failure ?line ?loc env =
  let exit_success, exit_failure = (Node.make_nop ?line ?loc env, Node.make_nop ?line ?loc env) in
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


let translate_pattern env (_value : Ident.t) _pattern =
  (* TODO: This is a dummy implementation. *)
  let start = Node.make_nop env in
  let make_dummy_prune branch : Sil.instr =
    let id = Ident.create_fresh Ident.knormal in
    Prune (Exp.Var id, Location.dummy, branch, Ik_if)
  in
  let prune_true = Node.make_if env true (make_dummy_prune true) in
  let prune_false = Node.make_if env false (make_dummy_prune false) in
  let exit_success = Node.make_nop env in
  let exit_failure = Node.make_nop env in
  start |~~> [prune_true; prune_false] ;
  prune_true |~~> [exit_success] ;
  prune_false |~~> [exit_failure] ;
  {start; exit_success; exit_failure}


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


let translate_one_function env cfg line function_ clauses =
  let uf_name = UnqualifiedFunction.of_ast function_ in
  let {UnqualifiedFunction.name= function_name; arity} = uf_name in
  let name =
    let module_name = env.current_module in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let any = typ_of_name Any in
  let attributes =
    let default = ProcAttributes.default env.source name in
    let access : ProcAttributes.access = if Set.mem env.exports uf_name then Public else Private in
    let formals = List.init ~f:(fun i -> (mangled_arg i, any)) arity in
    let loc = {Location.line; col= -1; file= env.source} in
    {default with access; formals; loc; ret_type= any}
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
    let loads_node = Node.make_stmt env CaseStmt loads in
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
  let f (form : Ast.form) =
    match form.simple_form with
    | Function {function_; clauses} ->
        translate_one_function env cfg form.line function_ clauses
    | _ ->
        ()
  in
  List.iter module_ ~f ;
  DB.Results_dir.init env.source ;
  Cfg.store env.source cfg ;
  SourceFiles.add env.source cfg Tenv.Global None


let translate_module module_ =
  let cfg = Cfg.create () in
  let env = get_environment module_ in
  translate_functions env cfg module_
