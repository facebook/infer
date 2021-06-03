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

type block =
  { start: Procdesc.Node.t
  ; exit_success: Procdesc.Node.t
  ; exit_failure: Procdesc.Node.t
  ; value: Ident.t }

type environment =
  { exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; current_module: module_name  (** used to qualify function names *)
  ; source: (SourceFile.t[@sexp.opaque])  (** used to add location information *)
  ; procdesc: (Procdesc.t option[@sexp.opaque])  (** imperative, being built *) }
[@@deriving sexp_of]

let get_environment module_ : environment =
  let init =
    { exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; current_module= Printf.sprintf "%s:unknown_module" __FILE__
    ; source= SourceFile.invalid __FILE__
    ; procdesc= None }
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

(** Assumes that the values on which patterns should be matched have been loaded into the
    identifiers listed in [values]. *)
let translate_case_clause env (_values : Ident.t list) {Ast.line; patterns= _; guards= _; body= _} :
    block =
  (* TODO: This is just a dummy implementation.  *)
  let loc = {Location.line; col= -1; file= env.source} in
  let procdesc = Option.value_exn env.procdesc in
  let create_node kind = Procdesc.create_node procdesc loc kind [] in
  let create_stmt_node () = create_node (Stmt_node CaseStmt) in
  let create_prune_node branch =
    let kind : Procdesc.Node.prune_node_kind =
      if branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch
    in
    create_node (Prune_node (branch, Ik_switch, kind))
  in
  let start = create_stmt_node () in
  let prune_true = create_prune_node true in
  let prune_false = create_prune_node false in
  let exit_success = create_stmt_node () in
  let exit_failure = create_stmt_node () in
  let value = Ident.create_fresh Ident.knormal in
  start |~~> [prune_true; prune_false] ;
  prune_true |~~> [exit_success] ;
  prune_false |~~> [exit_failure] ;
  {start; exit_success; exit_failure; value}


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
  let env = {env with procdesc= Some procdesc} in
  let idents, loads =
    let load (formal, typ) =
      let id = Ident.create_fresh Ident.knormal in
      let pvar = Pvar.mk formal name in
      let load = Sil.Load {id; e= Exp.Lvar pvar; root_typ= typ; typ; loc= attributes.loc} in
      (id, load)
    in
    List.unzip (List.map ~f:load attributes.formals)
  in
  let blocks = List.map ~f:(translate_case_clause env idents) clauses in
  let fail_node =
    (* Add a node that loads all values on which we pattern-match into idents. *)
    let loads_node = Procdesc.create_node procdesc attributes.loc (Stmt_node CaseStmt) loads in
    Procdesc.get_start_node procdesc |~~> [loads_node] ;
    (* Connect exit_failure of one case_clause to the start of the next case_clause. *)
    let f previous {start; exit_failure; _} =
      previous |~~> [start] ;
      exit_failure
    in
    List.fold ~init:loads_node ~f blocks
  in
  let () =
    (* If all patterns fail, call BuiltinDecl.__erlang_pattern_fail *)
    let crash_instruction =
      let ret_var = Ident.create_fresh Ident.knormal (* not used: nothing returned *) in
      let pattern_fail_fun = Exp.Const (Cfun BuiltinDecl.__erlang_pattern_fail) in
      Sil.Call ((ret_var, any), pattern_fail_fun, [], attributes.loc, CallFlags.default)
    in
    let crash_node =
      Procdesc.create_node procdesc attributes.loc Procdesc.Node.throw_kind [crash_instruction]
    in
    fail_node |~~> [crash_node] ;
    crash_node |~~> [Procdesc.get_exit_node procdesc]
  in
  let () =
    (* Copy result of each case into return value, and go to exit node. *)
    let ret_exp = Exp.Lvar (Pvar.get_ret_pvar name) in
    let f {exit_success; value; _} =
      let copy_instruction =
        Sil.Store {e1= ret_exp; root_typ= any; typ= any; e2= Exp.Var value; loc= attributes.loc}
      in
      let copy_node =
        Procdesc.create_node procdesc attributes.loc (Stmt_node ReturnStmt) [copy_instruction]
      in
      exit_success |~~> [copy_node] ;
      copy_node |~~> [Procdesc.get_exit_node procdesc]
    in
    List.iter ~f blocks
  in
  ()


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
