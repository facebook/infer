(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module L = Logging

module UnqualifiedFunction = struct
  module T = struct
    type t = {name: string; arity: int} [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
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
  let unqualified (f : Ast.function_) : UnqualifiedFunction.t =
    match f with
    | {module_= ModuleMissing; function_= FunctionName name; arity} ->
        {name; arity}
    | _ ->
        L.die InternalError "expected unqualified function"
  in
  let init =
    { exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty (* TODO: auto-import from module "erlang" *)
    ; current_module= Printf.sprintf "%s:unknown_module" __FILE__ }
  in
  let f env (form : Ast.form) =
    match form.simple_form with
    | Export functions ->
        let f exports function_ = Set.add exports (unqualified function_) in
        let exports = List.fold ~init:env.exports ~f functions in
        {env with exports}
    | Import {module_name; functions} ->
        let f imports function_ =
          let key = unqualified function_ in
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


let translate_functions _names_env _cfg _module = (* TODO *) ()

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
    translate_functions names_env cfg module_ ;
    cfg
  in
  (source, cfg)
