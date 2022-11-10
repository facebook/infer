(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

%{
open DoliAst
%}


(* Doli-specific keywords *)
%token UNDER
%token IN
%token MATCH
%token Java
%token ObjC
(* various brackets and parentheses *)
%token LPAREN
%token RPAREN
%token LANGLE
%token RANGLE
%token LBRACE
%token RBRACE
%token LSQUAREBRACKET
%token RSQUAREBRACKET
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
%token SEMI
(* generics *)
%token EXTENDS
%token SUPER
%token QUESTION
(* identifier *)
%token <string> ID
(* end-of-file *)
%token EOF
(* types *)
%type <DoliAst.matching> _javaMatch
%type <DoliAst.matching> _objCMatch
%type <DoliAst.body> _body
%type <DoliAst.doliProgram>  doliProgram
(* start *)
%start doliProgram _javaMatch _objCMatch _body
%%

doliProgram:
 | dis = doliInstruction* EOF { DoliProgram dis }
 ;

 doliInstruction:
	| IN; Java jm =_javaMatch bd=_body
	{  { match_ = jm; body = bd }  }
	| IN; ObjC ocm = _objCMatch bd=_body
	{   { match_ = ocm; body = bd }  }
	;
