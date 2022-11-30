(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This partial grammar specification defines the set of tokens for Doli.
 * In a next Diff, these will also become the tokens for Textual     *)


(* Doli-specific keywords *)
%token UNDER
%token IN
%token MATCH
%token Java
%token OBJC
(* various brackets and parentheses *)
%token LPAREN
%token RPAREN
%token LABRACKET
%token RABRACKET
%token LBRACKET
%token RBRACKET
%token LSBRACKET
%token RSBRACKET
(* mofifiers and throws *)
%token PUBLIC
%token PROTECTED
%token PRIVATE
%token STATIC
%token ABSTRACT
%token FINAL
%token NATIVE
%token THROWS
(* basic types *)
%token BYTE
%token SHORT
%token CHAR
%token INT
%token LONG
%token FLOAT
%token DOUBLE
%token BOOLEAN
%token VOID
(* separators *)
%token DOT
%token COMMA
%token SEMICOLON
(* generics *)
%token EXTENDS
%token SUPER
%token QUESTION
(* identifier *)
%token <string> ID
(* end-of-file *)
%token EOF
%%
