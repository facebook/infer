(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(** Erlang abstract forms, following https://erlang.org/doc/apps/erts/absform.html *)

open! IStd

(* TODO: validation, including basic type-checking *)

(** {2 Basics} *)

type module_reference = ModuleName of string | ModuleMissing | ModuleVariable of string

type function_reference = FunctionName of string | FunctionVariable of string

(* NOTE: Arity could be an expression but we don't handle that case, yet. *)
type function_ = {module_: module_reference; function_: function_reference; arity: int}

type line = int

type record_name = string

type binary_operator =
  | Add
  | And
  | AndAlso
  | AtLeast
  | AtMost
  | BAnd
  | BOr
  | Bsl
  | Bsr
  | BXor
  | Equal
  | ExactlyEqual
  | ExactlyNotEqual
  | FDiv
  | Greater
  | IDiv
  | Less
  | ListAdd
  | ListSub
  | Mul
  | NotEqual
  | Or
  | OrElse
  | Rem
  | Send
  | Sub
  | Xor

type unary_operator = UBNot | UMinus | UNot

type association_kind = Arrow | Exact

type exception_ = Atom of string | Pattern of string

type type_specifier = (* TODO *) unit

(** {2 S8.2: Atomic literals} *)

type literal = Atom of string | Char of string | Float of float | Int of string | String of string

(** {2 S8.4: Expressions} *)

type body = expression list

and simple_expression =
  | BinaryOperator of expression * binary_operator * expression
  | BitstringComprehension of {expression: expression; qualifiers: qualifier list}
  | BitstringConstructor of bin_element list
  | Block of body
  | Call of {module_: expression option; function_: expression; args: expression list}
  | Case of {expression: expression; cases: case_clause list}
  | Catch of expression
  | Cons of {head: expression; tail: expression}
  | Fun of function_
  | If of case_clause list
  | Lambda of {name: string option; cases: case_clause list}
  | ListComprehension of {expression: expression; qualifiers: qualifier list}
  | Literal of literal
  | Map of {map: expression option; updates: association list}
  | Match of {pattern: pattern; body: (* body is a pattern within patterns *) expression}
  | Nil
  | Receive of {cases: case_clause list; timeout: timeout option}
  | RecordAccess of {record: expression; name: record_name; field: string}
  | RecordIndex of {name: record_name; field: string} (* factor from above? *)
  | RecordUpdate of {record: expression option; name: record_name; updates: record_update list}
  | TryCatch of {body: body; ok_cases: case_clause list; catch_cases: catch_clause list; after: body}
  | Tuple of expression list
  | UnaryOperator of unary_operator * expression
  | Variable of string

and expression = {line: line; simple_expression: simple_expression}

and qualifier =
  | BitsGenerator of {pattern: pattern; expression: expression}
  | Filter of expression
  | Generator of {pattern: pattern; expression: expression}

and timeout = {time: expression; handler: body}

and bin_element =
  {expression: expression; size: expression option; types: type_specifier list option}

(* A [None] field stands for _, which means "all other fields". *)
and record_update = {field: string option; expression: expression}

and association = {kind: association_kind; key: expression; value: expression}

and pattern = expression

and guard_test = expression

(** {2 S8.5 Clauses} *)

and 'pat clause = {line: line; patterns: 'pat list; guards: guard_test list list; body: body}

and case_clause = pattern clause

and catch_clause = catch_pattern clause

and catch_pattern = {exception_: exception_; pattern: pattern; variable: string}

(** {2 S8.1: Module declarations and forms} *)

(* TODO: Add records, types, and specs. *)
type simple_form =
  | Export of function_ list
  | Import of {module_name: string; functions: function_ list}
  | Module of string
  | File of {path: string}
  | Function of {function_: function_; clauses: case_clause list}

type form = {line: line; simple_form: simple_form}

type module_ = form list
