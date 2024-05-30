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
  ; module_info: (Annot.t String.Map.t[@sexp.opaque])
        (** used to store data for Module:module_info *)
  ; current_module: module_name  (** used to qualify function names *)
  ; is_otp: bool  (** does this module come from the OTP library *)
  ; functions: UnqualifiedFunction.Set.t  (** used to resolve function names *)
  ; specs: Ast.spec UnqualifiedFunction.Map.t  (** map functions to their specs *)
  ; types: Ast.type_ String.Map.t  (** user defined types *)
  ; exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; records: record_info String.Map.t  (** used to get fields, indexes and initializers *)
  ; location: Location.t  (** used to tag nodes and instructions being created *)
  ; procdesc: ('procdesc[@sexp.opaque])
  ; result: ('result[@sexp.opaque]) }
[@@deriving sexp_of]

let unknown_module_name = "__INFER_UNKNOWN_MODULE"

let initialize_environment module_ otp_modules =
  let init =
    { cfg= Cfg.create ()
    ; module_info= String.Map.empty
    ; current_module= unknown_module_name
    ; is_otp= false
    ; functions= UnqualifiedFunction.Set.empty
    ; specs= UnqualifiedFunction.Map.empty
    ; types= String.Map.empty
    ; exports= UnqualifiedFunction.Set.empty
    ; imports= UnqualifiedFunction.Map.empty
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
              L.debug Capture Verbose "repeated import: %s/%d" key.name key.arity ;
              imports
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
        if String.(unknown_module_name <> env.current_module) then
          L.die InternalError "trying to set current module twice: old: %s, new: %s"
            env.current_module current_module ;
        let is_otp = String.Set.mem otp_modules current_module in
        {env with current_module; is_otp}
    | File _ ->
        env (* Handled during translation. *)
    | Function {function_; _} ->
        let key = UnqualifiedFunction.of_ast function_ in
        {env with functions= Set.add env.functions key}
    | Spec {function_; spec} -> (
        let key = UnqualifiedFunction.of_ast function_ in
        match Map.add ~key ~data:spec env.specs with
        | `Ok specs ->
            {env with specs}
        | `Duplicate ->
            L.die InternalError "repeated spec for %s/%d" key.name key.arity )
    | Type {name; type_} -> (
      match Map.add ~key:name ~data:type_ env.types with
      | `Ok types ->
          {env with types}
      | `Duplicate ->
          L.die InternalError "repeated type '%s'" name )
    | Attribute (StringAttribute {tag; value}) ->
        let module_info =
          let parameter = {Annot.name= Some tag; value= Str value} in
          let class_name = ErlangTypeName.module_info_attributes_class_name in
          Map.update env.module_info class_name ~f:(function
            | None ->
                {Annot.class_name; parameters= [parameter]}
            | Some annot ->
                let {Annot.parameters} = annot in
                {annot with Annot.parameters= parameter :: parameters} )
        in
        {env with module_info}
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


let has_type_instr (env : (_, _) t) ~result ~value (name : ErlangTypeName.t) : Sil.instr =
  let any_typ = ptr_typ_of_name Any in
  let fun_exp : Exp.t = Const (Cfun BuiltinDecl.__instanceof) in
  let args : (Exp.t * Typ.t) list =
    [ (value, any_typ)
    ; ( Sizeof
          { typ= typ_of_name name
          ; nbytes= None
          ; dynamic_length= None
          ; subtype= Subtype.subtypes_instof
          ; nullable= false }
      , any_typ ) ]
  in
  Call ((result, Typ.mk (Tint IBool)), fun_exp, args, env.location, CallFlags.default)


let procname_for_user_type module_name name =
  (* Avoid conflict with a "normal" function that has the same name as the type and arity of 1. *)
  let function_name = "__infer_assume_type_" ^ name in
  Procname.make_erlang ~module_name ~function_name ~arity:1


(** into_id=expr.field_name *)
let load_field_from_expr (env : (_, _) t) into_id expr field_name typ : Sil.instr =
  let any_typ = ptr_typ_of_name Any in
  let field = Fieldname.make (ErlangType typ) field_name in
  Load {id= into_id; e= Lfield (expr, field, typ_of_name typ); typ= any_typ; loc= env.location}
