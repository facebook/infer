/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{
  open Ctl_parser_types
%}

%token EU
%token AU
%token EF
%token AF
%token EX
%token AX
%token EG
%token AG
%token EH
%token ET
%token DEFINE_CHECKER
%token SET
%token LET
%token TRUE
%token FALSE
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token ASSIGNMENT
%token SEMICOLON
%token COMMA
%token AND
%token OR
%token NOT
%token QUOTES
%token COMMENT
%token COMMENT_LINE
%token <string> IDENTIFIER
%token <string> STRING
%token EOF

%start <Ctl_parser_types.ctl_checker list> checkers_list

%%
checkers_list:
  | EOF { [] }
  | checker SEMICOLON checkers_list { $1::$3 }
  ;

checker:
 DEFINE_CHECKER IDENTIFIER ASSIGNMENT LEFT_BRACE clause_list RIGHT_BRACE
  { Logging.out "Parsed checker \n"; { name = $2; definitions = $5 } }
;

clause_list:
 | clause SEMICOLON { [$1] }
 | clause SEMICOLON clause_list { $1 :: $3 }
;

clause:
  | SET IDENTIFIER ASSIGNMENT formula
    { Logging.out "Parsed set clause\n"; CSet ($2, $4) }
  | SET IDENTIFIER ASSIGNMENT STRING
    { Logging.out "Parsed desc clause\n"; CDesc ($2, $4) }
  | LET IDENTIFIER ASSIGNMENT formula
    { Logging.out "Parsed let clause\n"; CLet ($2, $4) }
;

atomic_formula:
  | TRUE { Logging.out "Parsed True\n"; CTL.True }
  | FALSE { Logging.out "Parsed False\n"; CTL.False }
  | IDENTIFIER LEFT_PAREN params RIGHT_PAREN
    { Logging.out "Parsed predicate\n"; CTL.Atomic($1, $3) }
  ;

 formula_id:
  | IDENTIFIER { CTL.Atomic($1,[]) }
  ;

params:
  | {[]}
  | IDENTIFIER { [$1] }
  | IDENTIFIER COMMA params { $1 :: $3 }
  ;

formula:
  | LEFT_PAREN formula RIGHT_PAREN { $2 }
  | formula_id { Logging.out "Parsed formula identifier\n"; $1 }
  | atomic_formula { Logging.out "Parsed atomic formula\n"; $1 }
  | formula EU formula { Logging.out "Parsed EU\n"; CTL.EU($1, $3) }
  | formula AU formula { Logging.out "Parsed AU\n"; CTL.AU($1, $3) }
  | formula EF { Logging.out "Parsed EF\n"; CTL.EF($1) }
  | formula AF{ Logging.out "Parsed AF\n"; CTL.AF($1) }
  | formula EX  { Logging.out "Parsed EX\n"; CTL.EX($1) }
  | formula AX  { Logging.out "Parsed AX\n"; CTL.AX($1) }
  | formula EG  { Logging.out "Parsed EG\n"; CTL.EG($1) }
  | formula AG  { Logging.out "Parsed AG\n"; CTL.AG($1) }
  | formula EH params  { Logging.out "Parsed EH\n"; CTL.EH($3, $1) }
  | formula AND formula { Logging.out "Parsed AND\n"; CTL.And($1, $3) }
  | formula OR formula { Logging.out "Parsed OR\n"; CTL.Or($1, $3) }
  | NOT formula { Logging.out "Parsed NOT\n"; CTL.Not($2)}
;

%%
