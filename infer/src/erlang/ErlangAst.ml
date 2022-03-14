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
[@@deriving sexp_of]

type function_reference = FunctionName of string | FunctionVariable of string [@@deriving sexp_of]

(* NOTE: Arity could be an expression but we don't handle that case, yet. *)
type function_ = {module_: module_reference; function_: function_reference; arity: int}
[@@deriving sexp_of]

(* Location info. For compatibility with [Location.t] -1 means unknown. *)
type location = {line: int; col: int} [@@deriving sexp_of]

type record_name = string [@@deriving sexp_of]

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
[@@deriving sexp_of]

type unary_operator = UBNot | UMinus | UNot [@@deriving sexp_of]

type association_kind = Arrow | Exact [@@deriving sexp_of]

type exception_ = Atom of string | Pattern of string [@@deriving sexp_of]

type type_specifier = (* TODO *) unit [@@deriving sexp_of]

(** {2 S8.2: Atomic literals} *)

type literal = Atom of string | Char of string | Float of float | Int of string | String of string
[@@deriving sexp_of]

(** {2 S8.4: Expressions} *)

type body = expression list [@@deriving sexp_of]

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
  | Lambda of
      { name: string option
      ; cases: case_clause list
      ; mutable procname: (Procname.t option[@sexp.opaque])
      ; mutable captured: (Pvar.Set.t option[@sexp.opaque]) }
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
  | Variable of {vname: string; mutable scope: (Procname.t option[@sexp.opaque])}
[@@deriving sexp_of]

and expression = {location: location; simple_expression: simple_expression} [@@deriving sexp_of]

and qualifier =
  | BitsGenerator of {pattern: pattern; expression: expression}
  | Filter of expression
  | Generator of {pattern: pattern; expression: expression}
[@@deriving sexp_of]

and timeout = {time: expression; handler: body} [@@deriving sexp_of]

and bin_element =
  {expression: expression; size: expression option; types: type_specifier list option}
[@@deriving sexp_of]

(* A [None] field stands for _, which means "all other fields". *)
and record_update = {field: string option; expression: expression} [@@deriving sexp_of]

and association = {kind: association_kind; key: expression; value: expression} [@@deriving sexp_of]

and pattern = expression [@@deriving sexp_of]

and guard_test = expression [@@deriving sexp_of]

(** {2 S8.5 Clauses} *)

and 'pat clause = {location: location; patterns: 'pat list; guards: guard_test list list; body: body}
[@@deriving sexp_of]

and case_clause = pattern clause [@@deriving sexp_of]

and catch_clause = catch_pattern clause [@@deriving sexp_of]

and catch_pattern = {exception_: exception_; pattern: pattern; variable: string}
[@@deriving sexp_of]

(** {2 S8.7 Types} *)

(** See also https://www.erlang.org/doc/reference_manual/typespec.html *)

type atom_type = Any | Literal of string [@@deriving sexp_of]

type integer_type = Any | Neg | NonNeg | Pos [@@deriving sexp_of]

type type_ =
  | Any
  | Atom of atom_type
  | BitString of {start_size: int; segment_size: int}
  | Integer of integer_type
  | List of list_type
  | Map (* TODO: associations *)
  | Nil
  | None
  | Pid
  | Port
  | Record of string (* TODO: fields*)
  | Reference
  | Remote of {module_: string; type_: string} (* TODO: arguments *)
  | String (* TODO: replace this with [char()] when we model strings as lists. *)
  | Tuple of tuple_type
  | Union of type_ list
  | UserDefined of string (* TODO: arguments *)
[@@deriving sexp_of]

and list_type = Proper of type_

and tuple_type = AnySize | FixedSize of type_ list [@@deriving sexp_of]

type spec = {function_: function_; arguments: type_ list; return: type_} [@@deriving sexp_of]

(** {2 S8.1: Module declarations and forms} *)

(* TODO: Add types, and specs. *)
type record_field = {field_name: string; initializer_: expression option} [@@deriving sexp_of]

type simple_form =
  | Export of function_ list
  | Import of {module_name: string; functions: function_ list}
  | Module of string
  | File of {path: string}
  | Function of {function_: function_; clauses: case_clause list}
  | Record of {name: string; fields: record_field list}
  | Spec of spec
[@@deriving sexp_of]

type form = {location: location; simple_form: simple_form} [@@deriving sexp_of]

type module_ = form list [@@deriving sexp_of]
