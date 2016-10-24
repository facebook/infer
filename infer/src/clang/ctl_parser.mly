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
%token DEFINE_CHECKER
%token ET
%token WITH_TRANSITION
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
%token IMPLIES
%token <string> IDENTIFIER
%token <string> STRING
%token EOF

/* associativity and priority (lower to higher) of operators */
%nonassoc IMPLIES
%left OR
%left AND
%left AU, EU
%right NOT, AX, EX, AF, EF, EG, AG, EH

%start <Ctl_parser_types.ctl_checker list> checkers_list

%%
checkers_list:
  | EOF { [] }
  | checker SEMICOLON checkers_list { $1::$3 }
  ;

checker:
 DEFINE_CHECKER IDENTIFIER ASSIGNMENT LEFT_BRACE clause_list RIGHT_BRACE
  { Logging.out "Parsed checker \n\n"; { name = $2; definitions = $5 } }
;

clause_list:
 | clause SEMICOLON { [$1] }
 | clause SEMICOLON clause_list { $1 :: $3 }
;

clause:
  | SET IDENTIFIER ASSIGNMENT formula
    { Logging.out "\tParsed set clause\n"; CSet ($2, $4) }
  | SET IDENTIFIER ASSIGNMENT STRING
    { Logging.out "\tParsed desc clause\n"; CDesc ($2, $4) }
  | LET IDENTIFIER ASSIGNMENT formula
    { Logging.out "\tParsed let clause\n"; CLet ($2, $4) }
;

atomic_formula:
  | TRUE { Logging.out "\tParsed True\n"; CTL.True }
  | FALSE { Logging.out "\tParsed False\n"; CTL.False }
  | IDENTIFIER LEFT_PAREN params RIGHT_PAREN
    { Logging.out "\tParsed predicate\n"; CTL.Atomic($1, $3) }
  ;

 formula_id:
  | IDENTIFIER { Logging.out "\tParsed formula identifier '%s' \n" $1; CTL.Atomic($1,[]) }
  ;

params:
  | {[]}
  | IDENTIFIER { [$1] }
  | IDENTIFIER COMMA params { $1 :: $3 }
  ;

transition_label:
  | IDENTIFIER { match $1 with
                  | "Body" | "body" -> Some CTL.Body
                  | "InitExpr" | "initexpr" -> Some CTL.InitExpr
                  | _  -> None }
  ;

formula_EF:
 | LEFT_PAREN formula RIGHT_PAREN EF { $2 }
;

formula:
  | LEFT_PAREN formula RIGHT_PAREN { $2 }
  | formula_id { $1 }
  | atomic_formula { Logging.out "\tParsed atomic formula\n"; $1 }
  | formula EU formula { Logging.out "\tParsed EU\n"; CTL.EU ($1, $3) }
  | formula AU formula { Logging.out "\tParsed AU\n"; CTL.AU ($1, $3) }
  | formula AF { Logging.out "\tParsed AF\n"; CTL.AF ($1) }
  | formula EX { Logging.out "\tParsed EX\n"; CTL.EX ($1) }
  | formula AX { Logging.out "\tParsed AX\n"; CTL.AX ($1) }
  | formula EG { Logging.out "\tParsed EG\n"; CTL.EG ($1) }
  | formula AG { Logging.out "\tParsed AG\n"; CTL.AG ($1) }
  | formula EH params { Logging.out "\tParsed EH\n"; CTL.EH ($3, $1) }
  | formula EF { Logging.out "\tParsed EF\n"; CTL.EF ($1) }
  | ET params WITH_TRANSITION transition_label formula_EF
     { Logging.out "\tParsed ET\n"; CTL.ET ($2, $4, $5)}
  | formula AND formula { Logging.out "\tParsed AND\n"; CTL.And ($1, $3) }
  | formula OR formula { Logging.out "\tParsed OR\n"; CTL.Or ($1, $3) }
  | formula IMPLIES formula { Logging.out "\tParsed IMPLIES\n"; CTL.Implies ($1, $3) }
  | NOT formula { Logging.out "\tParsed NOT\n"; CTL.Not ($2) }
;

%%
