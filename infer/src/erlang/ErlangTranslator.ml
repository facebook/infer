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

(** [exports] are used to determine which functions are public; [imports] and [current_module] are
    used to turn unqualified function references into qualified ones *)
type names_env =
  { exports: UnqualifiedFunction.Set.t
  ; imports: module_name UnqualifiedFunction.Map.t
  ; current_module: module_name }
[@@deriving sexp_of]

let get_environment module_ : names_env =
  let init =
    { exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; current_module= Printf.sprintf "%s:unknown_module" __FILE__ }
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
    | _ ->
        env
  in
  List.fold ~init ~f module_


let translate_one_function source names_env cfg line function_ clauses =
  let uf_name = UnqualifiedFunction.of_ast function_ in
  let {UnqualifiedFunction.name= function_name; arity} = uf_name in
  let name =
    let module_name = names_env.current_module in
    Procname.make_erlang ~module_name ~function_name ~arity
  in
  let attributes =
    let default = ProcAttributes.default source name in
    let access : ProcAttributes.access =
      if Set.mem names_env.exports uf_name then Public else Private
    in
    let formals = List.init ~f:(fun i -> (mangled_arg i, typ_of_name Any)) arity in
    let loc = {Location.line; col= -1; file= source} in
    let ret_type = typ_of_name Any in
    {default with access; formals; loc; ret_type}
  in
  let _proc = Cfg.create_proc_desc cfg attributes in
  (* TODO: add nodes to proc *)
  if List.is_empty clauses then
    L.die InternalError "%s:%a has no clauses" names_env.current_module Procname.pp name


let translate_functions source names_env cfg module_ =
  let f (form : Ast.form) =
    match form.simple_form with
    | Function {function_; clauses} ->
        translate_one_function source names_env cfg form.line function_ clauses
    | _ ->
        ()
  in
  List.iter module_ ~f ;
  DB.Results_dir.init source ;
  Cfg.store source cfg ;
  SourceFiles.add source cfg Tenv.Global None


let to_source_and_cfg module_ =
  let source =
    let extract_path = function
      | {Ast.line= _; simple_form= File {path}} ->
          Some path
      | _ ->
          None
    in
    match List.find_map ~f:extract_path module_ with
    | None ->
        SourceFile.invalid __FILE__
    | Some path ->
        SourceFile.create path
  in
  let cfg =
    let cfg = Cfg.create () in
    let names_env = get_environment module_ in
    translate_functions source names_env cfg module_ ;
    cfg
  in
  (source, cfg)
