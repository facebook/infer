(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module Env = ErlangEnvironment
module L = Logging

(** Enforce additional invariants and constraints on the AST based on
    https://erlang.org/doc/apps/erts/absform.html *)

let pp_location (loc : Ast.location) =
  match loc.col with
  | -1 ->
      Printf.sprintf "line %d" loc.line
  | _ ->
      Printf.sprintf "line %d column %d" loc.line loc.col


let validate_record_name (env : (_, _) Env.t) name =
  match String.Map.find env.records name with
  | None ->
      L.debug Capture Verbose "Record definition not found for '%s'@." name ;
      false
  | Some _ ->
      true


let validate_record_field (env : (_, _) Env.t) name field =
  match String.Map.find env.records name with
  | None ->
      L.debug Capture Verbose "Record definition not found for '%s'@." name ;
      false
  | Some record_info -> (
    match String.Map.find record_info.field_info field with
    | None ->
        L.debug Capture Verbose "Record field '%s' not found in definition of '%s'@." field name ;
        false
    | Some _ ->
        true )


(** {2 Patterns} *)

(** Patterns are expressions in general, but with restrictions and constraints. *)
let rec validate_pattern env (p : Ast.expression) =
  match p.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_pattern env e1 && validate_pattern env e2
      (* Not checked: operator is ++ over literal str or char list, or whole expr is number *)
  | BitstringConstructor elems ->
      let validate_elem env (e : Ast.bin_element) = validate_pattern env e.expression in
      List.for_all ~f:(validate_elem env) elems
  | Cons {head; tail} ->
      validate_pattern env head && validate_pattern env tail
  | Literal _ ->
      true
  | Map {map; updates} ->
      let validate_assoc (a : Ast.association) =
        match a.kind with
        | Exact ->
            validate_guard_test env a.key && validate_pattern env a.value
        | _ ->
            L.debug Capture Verbose "Invalid map association kind (not :=) in pattern@." ;
            false
      in
      is_none map && List.for_all ~f:validate_assoc updates
  | Match {pattern; body} ->
      validate_pattern env pattern && validate_pattern env body
  | Nil ->
      true
  | RecordIndex {name; field} ->
      validate_record_field env name field
  | RecordUpdate {record; updates; name} ->
      let validate_update env (ru : Ast.record_update) =
        (match ru.field with None -> true | Some field -> validate_record_field env name field)
        && validate_pattern env ru.expression
      in
      is_none record && validate_record_name env name
      && List.for_all ~f:(validate_update env) updates
  | Tuple exprs ->
      List.for_all ~f:(validate_pattern env) exprs
  | UnaryOperator (_, e) ->
      validate_pattern env e (* Not checked: evaluates to number *)
  | Variable _ ->
      true
  | _ ->
      (* Everything else is invalid in a pattern *)
      L.debug Capture Verbose "Invalid pattern at %s@." (pp_location p.location) ;
      false


and validate_patterns env = List.for_all ~f:(validate_pattern env)

(** {2 Guards} *)
(** Guards are expressions in general, but with restrictions and constraints. *)

and validate_guard_call_func (e : Ast.expression) =
  match e.simple_expression with
  | Literal lit -> (
    match lit with Atom _ -> true | _ -> false )
  | _ ->
      L.debug Capture Verbose "Function in guard is not an atom@." ;
      false


and validate_guard_call_module (eo : Ast.expression option) =
  match eo with
  | None ->
      true
  | Some e -> (
    match e.simple_expression with
    | Literal lit -> (
      match lit with
      | Atom "erlang" ->
          true
      | _ ->
          L.debug Capture Verbose "Non-erlang module in call in a guard@." ;
          false )
    | _ ->
        L.debug Capture Verbose "Non-atom module in call in a guard@." ;
        false )


and validate_guard_test env (gt : Ast.expression) =
  match gt.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_guard_test env e1 && validate_guard_test env e2
  | BitstringConstructor elems ->
      let validate_elem env (e : Ast.bin_element) = validate_guard_test env e.expression in
      List.for_all ~f:(validate_elem env) elems
  | Call {module_; function_; args} ->
      validate_guard_call_module module_
      && validate_guard_call_func function_
      && List.for_all ~f:(validate_guard_test env) args
  | Cons {head; tail} ->
      validate_guard_test env head && validate_guard_test env tail
  | Literal _ ->
      true
  | Map {map; updates} -> (
      (* Map create only accepts '=>' *)
      let validate_create (a : Ast.association) =
        match a.kind with
        | Arrow ->
            validate_guard_test env a.key && validate_guard_test env a.value
        | _ ->
            L.debug Capture Verbose "Invalid map association kind (not =>) in map create@." ;
            false
      in
      (* Map update accepts '=>' and ':=' *)
      let validate_update (a : Ast.association) =
        validate_guard_test env a.key && validate_guard_test env a.value
      in
      match map with
      | None ->
          List.for_all ~f:validate_create updates
      | Some expr ->
          validate_guard_test env expr && List.for_all ~f:validate_update updates )
  | Nil ->
      true
  | RecordAccess {record; name; field} ->
      validate_record_field env name field && validate_guard_test env record
  | RecordIndex {name; field} ->
      validate_record_field env name field
  | RecordUpdate {record; updates; name} ->
      let validate_update env (ru : Ast.record_update) =
        (match ru.field with None -> true | Some field -> validate_record_field env name field)
        && validate_guard_test env ru.expression
      in
      is_none record && validate_record_name env name
      && List.for_all ~f:(validate_update env) updates
  | Tuple exprs ->
      List.for_all ~f:(validate_guard_test env) exprs
  | UnaryOperator (_, e) ->
      validate_guard_test env e
  | Variable _ ->
      true
  | _ ->
      (* Everything else is invalid in a guard test *)
      L.debug Capture Verbose "Invalid guard test at %s@." (pp_location gt.location) ;
      false


let validate_guard env = List.for_all ~f:(validate_guard_test env)

let validate_guard_seq env = List.for_all ~f:(validate_guard env)

(** {2 Expressions} *)

let rec validate_expr env (expr : Ast.expression) =
  match expr.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_expr env e1 && validate_expr env e2
  | BitstringComprehension {expression= e; qualifiers= qs} ->
      validate_expr env e && List.for_all ~f:(validate_qualifier env) qs
  | BitstringConstructor elems ->
      let validate_elem (e : Ast.bin_element) = validate_expr env e.expression in
      List.for_all ~f:validate_elem elems
  | Block body ->
      validate_body env body
  | Call {module_; function_; args} ->
      validate_expr_opt env module_ && validate_expr env function_
      && List.for_all ~f:(validate_expr env) args
  | Case {expression= e; cases= cs} ->
      validate_expr env e && List.for_all ~f:(validate_case_clause env) cs
  | Catch e ->
      validate_expr env e
  | Cons {head; tail} ->
      validate_expr env head && validate_expr env tail
  | Fun _ ->
      true
  | If clauses ->
      List.for_all ~f:(validate_if_clause env) clauses
  | Lambda {cases; _} ->
      List.for_all ~f:(validate_function_clause env) cases
  | ListComprehension {expression= e; qualifiers= qs} ->
      validate_expr env e && List.for_all ~f:(validate_qualifier env) qs
  | Literal _ ->
      true
  | Map {map; updates} -> (
      (* Map create only accepts '=>' *)
      let validate_create (a : Ast.association) =
        match a.kind with
        | Arrow ->
            validate_expr env a.key && validate_expr env a.value
        | _ ->
            L.debug Capture Verbose "Invalid map association kind (not =>) in map create@." ;
            false
      in
      (* Map update accepts '=>' and ':=' *)
      let validate_update (a : Ast.association) =
        validate_expr env a.key && validate_expr env a.value
      in
      match map with
      | None ->
          List.for_all ~f:validate_create updates
      | Some expr ->
          validate_expr env expr && List.for_all ~f:validate_update updates )
  | MapComprehension {expression= e; qualifiers= qs} -> (
    match e.kind with
    | Arrow ->
        validate_expr env e.key && validate_expr env e.value
        && List.for_all ~f:(validate_qualifier env) qs
    | _ ->
        L.debug Capture Verbose "Invalid map association kind (not =>) in map comprehension@." ;
        false )
  | Match {pattern; body} | MaybeMatch {pattern; body} ->
      (* TOOD: MaybeMatch must have Maybe as parent *)
      validate_pattern env pattern && validate_expr env body
  | Maybe {body; else_cases} ->
      validate_body env body && List.for_all ~f:(validate_case_clause env) else_cases
  | Nil ->
      true
  | Receive {cases; timeout} -> (
      List.for_all ~f:(validate_case_clause env) cases
      &&
      match timeout with
      | None ->
          true
      | Some {time; handler} ->
          validate_expr env time && validate_body env handler )
  | RecordAccess {record; name; field} ->
      validate_record_field env name field && validate_expr env record
  | RecordIndex {name; field} ->
      validate_record_field env name field
  | RecordUpdate {record; updates; name} ->
      let validate_update env (ru : Ast.record_update) =
        (match ru.field with None -> true | Some field -> validate_record_field env name field)
        && validate_expr env ru.expression
      in
      validate_expr_opt env record && validate_record_name env name
      && List.for_all ~f:(validate_update env) updates
  | TryCatch {body; ok_cases; catch_cases; after} ->
      validate_body env body
      && List.for_all ~f:(validate_case_clause env) ok_cases
      && List.for_all ~f:(validate_catch_clause env) catch_cases
      && validate_body env after
  | Tuple exprs ->
      List.for_all ~f:(validate_expr env) exprs
  | UnaryOperator (_, e) ->
      validate_expr env e
  | Variable _ ->
      true


and validate_expr_opt env (expr : Ast.expression option) =
  match expr with Some e -> validate_expr env e | None -> true


and validate_qualifier env (q : Ast.qualifier) =
  match q with
  | BitsGenerator {pattern= p; expression= e} ->
      validate_pattern env p && validate_expr env e
  | Filter e ->
      validate_expr env e
  | Generator {pattern= p; expression= e} ->
      validate_pattern env p && validate_expr env e
  | MapGenerator {pattern= p; expression= e} -> (
    match p.kind with
    | Exact ->
        validate_pattern env p.key && validate_pattern env p.value && validate_expr env e
    | _ ->
        L.debug Capture Verbose "Invalid map association kind (not :=) in map generator@." ;
        false )


and validate_body env = List.for_all ~f:(validate_expr env)

(** {2 Clauses} *)

and validate_case_clause_components env patterns guards body =
  validate_patterns env patterns && validate_guard_seq env guards && validate_body env body


and validate_case_clause env ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  (match ps with [_] -> true | _ -> false) && validate_case_clause_components env ps gs b


and validate_if_clause env ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  List.is_empty ps && validate_case_clause_components env ps gs b


and validate_catch_clause env ({patterns= ps; guards= gs; body= b; _} : Ast.catch_clause) =
  (match ps with [{pattern; _}] -> validate_pattern env pattern | _ -> false)
  && validate_guard_seq env gs && validate_body env b


and validate_function_clause env ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  validate_case_clause_components env ps gs b


(** {2 Module declarations and forms} *)

let validate_function env _ = List.for_all ~f:(validate_function_clause env)

let rec validate_type (type_ : Ast.type_) =
  match type_ with
  | List (Proper t) ->
      validate_type t
  | Tuple (FixedSize ts) ->
      List.for_all ~f:validate_type ts
  | Union [] ->
      (* A union is a list of types. This list should not be empty. *)
      L.debug Capture Verbose "Invalid type: empty union@." ;
      false
  | Union ts ->
      List.for_all ~f:validate_type ts
  | _ ->
      (* TODO: validate other kinds of types. *)
      true


let validate_spec_disjunct ({arguments; return; constraints} : Ast.spec_disjunct) =
  List.for_all ~f:validate_type arguments
  && validate_type return
  && String.Map.for_all ~f:validate_type constraints


let validate_spec_arities (func_arity : int) (spec : Ast.spec) =
  (* We check that all the overloads in the function spec have the same arity,
     and that this agrees with the arity of the function (according to the spec). *)
  List.for_all
    ~f:(fun ({arguments: _} : Ast.spec_disjunct) -> Int.equal (List.length arguments) func_arity)
    spec
  ||
  ( L.debug Capture Verbose "Invalid spec: inconsistent arities in function spec@." ;
    false )


let validate_form env (form : Ast.form) =
  match form.simple_form with
  | Function {function_; clauses} ->
      validate_function env function_ clauses
  | Spec {spec= []; _} ->
      (* A function spec is a list of overloads. This list should not be empty *)
      L.debug Capture Verbose "Invalid spec: empty list of overloads@." ;
      false
  | Spec {function_= {arity; _}; spec} ->
      (validate_spec_arities arity spec && List.for_all ~f:validate_spec_disjunct spec)
      ||
      ( L.debug Capture Verbose "Invalid spec@." ;
        false )
  | Type {name; type_} ->
      validate_type type_
      ||
      ( L.debug Capture Verbose "Invalid type: %s@." name ;
        false )
  (* TODO: validate other forms. *)
  | _ ->
      true


let validate (env : (_, _) Env.t) module_ =
  L.debug Capture Verbose "Validating AST of module %s@." env.current_module ;
  List.for_all ~f:(validate_form env) module_
