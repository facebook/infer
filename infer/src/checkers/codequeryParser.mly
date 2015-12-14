/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{

%}

%token DOT SEMICOLON COLON COMMA SINGLE_QUOTE DOUBLE_QUOTE REV_QUOTE
%token PERCENT AMPERSAND EXCLAMATION EQUAL MINUS PLUS EQUALEQUAL EXCLAMATIONEQUAL
%token LEFT_CHEVRON RIGHT_CHEVRON LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_SQBRACKET RIGHT_SQBRACKET
%token STAR PIPE SLASH BACKSLASH
%token IF DOTDOTDOT SOURCE ERROR

%token <string> HEXA
%token <string> NUMBER
%token <string> IDENT
%token <string> CONST_STRING
%token <string> ATTRIBUTE

%token EOF

%start query
%type <CodeQueryAst.query option> query

%%

ident:
  | IDENT { $1 }
;

expr:
  | ident { if $1 = "null" then CodeQueryAst.Null else CodeQueryAst.Ident $1 }
  | CONST_STRING { CodeQueryAst.ConstString $1 }
;

condition:
  | expr EQUALEQUAL expr { ($1, "==", $3) }
  | expr EXCLAMATIONEQUAL expr { ($1, "!=", $3) }
;

param_element:
  | expr { $1 }
;

param_element_list_nonempty:
  | param_element { [$1] }
  | param_element COMMA param_element_list_nonempty { $1 :: $3 }
;
param_element_list:
  | { [] }
  | param_element_list_nonempty { $1 }
;

call_params:
  | STAR { None }
  | param_element_list { Some $1 }
;

rule:
  | expr LEFT_PARENTHESIS expr RIGHT_PARENTHESIS { CodeQueryAst.Call ($1, $3) }
  | expr DOT expr LEFT_PARENTHESIS call_params RIGHT_PARENTHESIS { CodeQueryAst.MethodCall($1, $3, $5) }
  | IF LEFT_PARENTHESIS condition RIGHT_PARENTHESIS DOTDOTDOT rule { let x, y, z = $3 in CodeQueryAst.If (x, y, z, $6) }
;

action:
  | { CodeQueryAst.Noaction }
  | SOURCE SEMICOLON { CodeQueryAst.Source None }
  | SOURCE LEFT_PARENTHESIS NUMBER COMMA NUMBER RIGHT_PARENTHESIS SEMICOLON { CodeQueryAst.Source (Some (int_of_string $3, int_of_string $5)) }
  | ERROR SEMICOLON { CodeQueryAst.Error None }
  | ERROR LEFT_PARENTHESIS ident RIGHT_PARENTHESIS SEMICOLON { CodeQueryAst.Error (Some $3) }

query:
  | rule SEMICOLON action { Some ($1,$3) }
  | EOF { None }
;
