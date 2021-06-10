(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst

(** Enforce additional invariants and constraints on the AST based on
    https://erlang.org/doc/apps/erts/absform.html *)

(** {2 Patterns} *)

(** Patterns are expressions in general, but with restrictions and constraints. *)

let rec validate_pattern (p : Ast.expression) =
  match p.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_pattern e1 && validate_pattern e2
      (* Not checked: operator is ++ over literal str or char list, or whole expr is number *)
  | BitstringConstructor elems ->
      let validate_elem (e : Ast.bin_element) = validate_pattern e.expression in
      List.for_all ~f:validate_elem elems
  | Cons {head; tail} ->
      validate_pattern head && validate_pattern tail
  | Literal _ ->
      true
  | Map {map; updates} ->
      let validate_assoc (a : Ast.association) =
        validate_pattern a.key && validate_pattern a.value
        (* TODO: a.kind must be Exact, except inside ets:fun2ms or dbg:fun2ms *)
      in
      is_none map && List.for_all ~f:validate_assoc updates
  | Match {pattern; body} ->
      validate_pattern pattern && validate_pattern body
  | Nil ->
      true
  | RecordIndex _ ->
      true
  | RecordUpdate {record; updates; _} ->
      let validate_update (ru : Ast.record_update) = validate_pattern ru.expression in
      is_none record && List.for_all ~f:validate_update updates
  | Tuple exprs ->
      List.for_all ~f:validate_pattern exprs
  | UnaryOperator (_, e) ->
      validate_pattern e (* Not checked: evaluates to number *)
  | Variable _ ->
      true
  | _ ->
      (* Everything else is invalid in a pattern *)
      Logging.debug Capture Verbose "Invalid pattern at line %d@." p.line ;
      false


let validate_patterns = List.for_all ~f:validate_pattern

(** {2 Guards} *)

(** Guards are expressions in general, but with restrictions and constraints. *)

let is_atom (e : Ast.expression) =
  match e.simple_expression with
  | Literal lit -> (
    match lit with Atom _ -> true | _ -> false )
  | _ ->
      false


let validate_guard_call_module (eo : Ast.expression option) =
  match eo with
  | None ->
      true
  | Some e -> (
    match e.simple_expression with
    | Literal lit -> (
      match lit with Atom "erlang" -> true | _ -> false )
    | _ ->
        false )


let rec validate_guard_test (gt : Ast.expression) =
  match gt.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_guard_test e1 && validate_guard_test e2
  | BitstringConstructor elems ->
      let validate_elem (e : Ast.bin_element) = validate_guard_test e.expression in
      List.for_all ~f:validate_elem elems
  | Call {module_; function_; args} ->
      validate_guard_call_module module_
      && is_atom function_
      && List.for_all ~f:validate_guard_test args
  | Cons {head; tail} ->
      validate_guard_test head && validate_guard_test tail
  | Literal _ ->
      true
  | Map {map; updates} -> (
      (* Map create only accepts '=>' *)
      let validate_create (a : Ast.association) =
        match a.kind with
        | Arrow ->
            validate_guard_test a.key && validate_guard_test a.value
        | _ ->
            false
      in
      (* Map update accepts '=>' and ':=' *)
      let validate_update (a : Ast.association) =
        validate_guard_test a.key && validate_guard_test a.value
      in
      match map with
      | None ->
          List.for_all ~f:validate_create updates
      | Some expr ->
          validate_guard_test expr && List.for_all ~f:validate_update updates )
  | Nil ->
      true
  | RecordAccess {record; _} ->
      validate_guard_test record
  | RecordIndex _ ->
      true
  | RecordUpdate {record; updates; _} ->
      let validate_update (ru : Ast.record_update) = validate_guard_test ru.expression in
      is_none record && List.for_all ~f:validate_update updates
  | Tuple exprs ->
      List.for_all ~f:validate_guard_test exprs
  | UnaryOperator (_, e) ->
      validate_guard_test e
  | Variable _ ->
      true
  | _ ->
      (* Everything else is invalid in a guard test *)
      Logging.debug Capture Verbose "Invalid guard test at line %d@." gt.line ;
      false


let validate_guard = List.for_all ~f:validate_guard_test

let validate_guard_seq = List.for_all ~f:validate_guard

(** {2 Expressions} *)

let rec validate_expr (expr : Ast.expression) =
  match expr.simple_expression with
  | BinaryOperator (e1, _, e2) ->
      validate_expr e1 && validate_expr e2
  | BitstringComprehension {expression= e; qualifiers= qs} ->
      validate_expr e && List.for_all ~f:validate_qualifier qs
  | BitstringConstructor elems ->
      let validate_elem (e : Ast.bin_element) = validate_expr e.expression in
      List.for_all ~f:validate_elem elems
  | Block body ->
      validate_body body
  | Call {module_; function_; args} ->
      validate_expr_opt module_ && validate_expr function_ && List.for_all ~f:validate_expr args
  | Case {expression= e; cases= cs} ->
      validate_expr e && List.for_all ~f:validate_case_clause cs
  | Catch e ->
      validate_expr e
  | Cons {head; tail} ->
      validate_expr head && validate_expr tail
  | Fun _ ->
      true
  | If clauses ->
      List.for_all ~f:validate_if_clause clauses
  | Lambda {cases; _} ->
      List.for_all ~f:validate_function_clause cases
  | ListComprehension {expression= e; qualifiers= qs} ->
      validate_expr e && List.for_all ~f:validate_qualifier qs
  | Literal _ ->
      true
  | Map {map; updates} ->
      let validate_assoc (a : Ast.association) = validate_expr a.key && validate_expr a.value in
      validate_expr_opt map && List.for_all ~f:validate_assoc updates
  | Match {pattern; body} ->
      validate_pattern pattern && validate_expr body
  | Nil ->
      true
  | Receive {cases; timeout} -> (
      List.for_all ~f:validate_case_clause cases
      &&
      match timeout with
      | None ->
          true
      | Some {time; handler} ->
          validate_expr time && validate_body handler )
  | RecordAccess {record; _} ->
      validate_expr record
  | RecordIndex _ ->
      true
  | RecordUpdate {record; updates; _} ->
      let validate_update (ru : Ast.record_update) = validate_expr ru.expression in
      validate_expr_opt record && List.for_all ~f:validate_update updates
  | TryCatch {body; ok_cases; catch_cases; after} ->
      validate_body body
      && List.for_all ~f:validate_case_clause ok_cases
      && List.for_all ~f:validate_catch_clause catch_cases
      && validate_body after
  | Tuple exprs ->
      List.for_all ~f:validate_expr exprs
  | UnaryOperator (_, e) ->
      validate_expr e
  | Variable _ ->
      true


and validate_expr_opt (expr : Ast.expression option) =
  match expr with Some e -> validate_expr e | None -> true


and validate_qualifier (q : Ast.qualifier) =
  match q with
  | BitsGenerator {pattern= p; expression= e} ->
      validate_pattern p && validate_expr e
  | Filter e ->
      validate_expr e
  | Generator {pattern= p; expression= e} ->
      validate_pattern p && validate_expr e


and validate_body = List.for_all ~f:validate_expr

(** {2 Clauses} *)

and validate_case_clause_components patterns guards body =
  validate_patterns patterns && validate_guard_seq guards && validate_body body


and validate_case_clause ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  Poly.(List.length ps = 1) && validate_case_clause_components ps gs b


and validate_if_clause ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  Poly.(List.length ps = 0) && validate_case_clause_components ps gs b


and validate_catch_clause ({patterns= ps; guards= gs; body= b; _} : Ast.catch_clause) =
  (match ps with [{pattern; _}] -> validate_pattern pattern | _ -> false)
  && validate_guard_seq gs && validate_body b


and validate_function_clause ({patterns= ps; guards= gs; body= b; _} : Ast.case_clause) =
  validate_case_clause_components ps gs b


(** {2 Module declarations and forms} *)

let validate_function _ = List.for_all ~f:validate_function_clause

let validate_form (form : Ast.form) =
  match form.simple_form with
  | Function {function_; clauses} ->
      validate_function function_ clauses
  | _ ->
      true


let validate module_ =
  Logging.debug Capture Verbose "Validating AST@." ;
  List.for_all ~f:validate_form module_
