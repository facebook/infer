(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module L = Logging

type module_name = string [@@deriving sexp_of]

type absent = Absent

type 'a present = Present of 'a

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

type record_field_info = {index: int; initializer_: Ast.expression option} [@@deriving sexp_of]

type record_info = {field_names: string list; field_info: record_field_info String.Map.t}
[@@deriving sexp_of]

type ('procdesc, 'result) t =
  { cfg: (Cfg.t[@sexp.opaque])
  ; current_module: module_name  (** used to qualify function names *)
  ; functions: UnqualifiedFunction.Set.t  (** used to resolve function names *)
  ; exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; records: record_info String.Map.t  (** used to get fields, indexes and initializers *)
  ; location: Location.t  (** used to tag nodes and instructions being created *)
  ; procdesc: ('procdesc[@sexp.opaque])
  ; result: ('result[@sexp.opaque]) }
[@@deriving sexp_of]

let get_environment module_ =
  let init =
    { cfg= Cfg.create ()
    ; current_module= Printf.sprintf "%s:unknown_module" __FILE__
    ; functions= UnqualifiedFunction.Set.empty
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
    | Function {function_; _} ->
        let key = UnqualifiedFunction.of_ast function_ in
        {env with functions= Set.add env.functions key}
  in
  List.fold ~init ~f module_


let typ_of_name (name : ErlangTypeName.t) : Typ.t = Typ.mk (Tstruct (ErlangType name))

let ptr_typ_of_name (name : ErlangTypeName.t) : Typ.t = Typ.mk (Tptr (typ_of_name name, Pk_pointer))

let func_procname env function_ =
  let uf_name = UnqualifiedFunction.of_ast function_ in
  let {UnqualifiedFunction.name= function_name; arity} = uf_name in
  let module_name = env.current_module in
  let procname = Procname.make_erlang ~module_name ~function_name ~arity in
  (uf_name, procname)
