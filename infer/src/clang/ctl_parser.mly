/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{

  let formal_params : (ALVar.t list) ref = ref []

  let is_not_infer_reserved_id id =
    if Str.string_match (Str.regexp_string Ctl_parser_types.infer_prefix) id 0 then
      raise (Ctl_parser_types.ALParsingException
               ("ERROR: " ^ id ^ " contains __infer_ctl_ that is a reserved keyword "
            ^ "which cannot be used in identifiers:"))
     else ()

  let is_defined_identifier id =
     if (List.mem (ALVar.Var id) !formal_params) then
              Logging.out "\tParsed exp '%s' as variable" id
     else
      raise (Ctl_parser_types.ALParsingException
        ("ERROR: Variable '" ^ id ^ "' is undefined"))

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
%token GLOBAL_MACROS
%token HASHIMPORT
%token LESS_THAN
%token GREATER_THAN
%token ET
%token ETX
%token WITH_TRANSITION
%token WHEN
%token HOLDS_IN_NODE
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
%token REGEXP
%token <string> IDENTIFIER
%token <string> FILE_IDENTIFIER
%token <string> STRING
%token EOF

/* associativity and priority (lower to higher) of operators */
%nonassoc IMPLIES
%left OR
%left AND
%left AU, EU
%right NOT, AX, EX, AF, EF, EG, AG, EH

%start <CTL.al_file> al_file
%%

var_list:
  | identifier { [ALVar.Var $1] }
  | identifier COMMA var_list { ALVar.Var($1) :: $3 }
;

node_list:
  | identifier { [ALVar.Const $1] }
  | identifier COMMA node_list { ALVar.Const($1) :: $3 }
;

formal_params:
  | var_list { formal_params := $1; $1}

al_file:
  | import_files global_macros checkers_list {
      { CTL.import_files = $1; CTL.global_macros = $2; CTL.checkers = $3 }
    }
  ;

import_files:
  | { [] }
  | HASHIMPORT LESS_THAN file_identifier GREATER_THAN import_files
    { Logging.out "Parsed import clauses...\n\n"; $3 :: $5 }
  ;

global_macros:
  | { [] }
  | GLOBAL_MACROS LEFT_BRACE let_clause_list RIGHT_BRACE SEMICOLON
    { Logging.out "Parsed global macro definitions...\n\n"; $3 }
  ;

checkers_list:
  | EOF { [] }
  | checker SEMICOLON checkers_list { $1::$3 }
  ;

checker:
 DEFINE_CHECKER identifier ASSIGNMENT LEFT_BRACE clause_list RIGHT_BRACE
  {
    Logging.out "\nParsed checker definition\n";
    let c = { CTL.name = $2; CTL.definitions = $5 } in
    CTL.print_checker c;
    c
  }
;

clause_list:
 | clause SEMICOLON { [$1] }
 | clause SEMICOLON clause_list { $1 :: $3 }
;

let_clause_list:
 | let_clause SEMICOLON { [$1] }
 | let_clause SEMICOLON let_clause_list { $1 :: $3 }
;

clause:
  | SET identifier ASSIGNMENT formula
    { Logging.out "\tParsed SET clause\n";
    let alvar = match $2 with
      | "report_when" -> ALVar.Report_when
      | _ -> failwith ("[ERROR] string '%s' cannot be set to a variable. " ^
                      "Use the reserverd variable 'report_when'\n") in
      CTL.CSet (alvar, $4) }
  | SET identifier ASSIGNMENT STRING
    { Logging.out "\tParsed SET clause\n";
      let alvar = match $2 with
      | "message" -> ALVar.Message
      | "suggestion" -> ALVar.Suggestion
      | "severity" -> ALVar.Severity
      | "mode" -> ALVar.Mode
      | _ -> failwith ("[ERROR] string '%s' cannot be set in a SET clause. " ^
                        "Use either of: 'message', 'suggestion', 'severity', or 'mode'\n") in
      CTL.CDesc (alvar, $4) }
    | let_clause { $1 }
    ;

let_clause:
  | LET formula_id_def ASSIGNMENT formula
    { Logging.out "\tParsed LET clause\n"; CTL.CLet ($2, [], $4) }
  | LET formula_id_def LEFT_PAREN formal_params RIGHT_PAREN ASSIGNMENT formula
               { Logging.out "\tParsed let clause with formula identifier '%s(....)' \n"
                  (ALVar.formula_id_to_string $2);
                 CTL.CLet ($2, $4, $7) }
  ;

atomic_formula:
  | TRUE { Logging.out "\tParsed True\n"; CTL.True }
  | FALSE { Logging.out "\tParsed False\n"; CTL.False }
  | identifier LEFT_PAREN actual_params RIGHT_PAREN
    { Logging.out "\tParsed predicate\n"; CTL.Atomic(ALVar.Formula_id $1, $3) }
  ;

  formula_id_def:
  | identifier { Logging.out "\tParsed formula identifier '%s' \n" $1;
                  formal_params := [];
                  ALVar.Formula_id $1 }
   ;

 formula_id:
 | identifier { Logging.out "\tParsed formula identifier '%s' \n" $1;
                 ALVar.Formula_id $1 }
  ;

actual_params:
  | {[]}
  | alexp { [$1] }
  | alexp COMMA actual_params { $1 :: $3 }
  ;

transition_label:
  | identifier { match $1 with
                  | "Body" | "body" -> Some CTL.Body
                  | "InitExpr" | "initexpr" -> Some CTL.InitExpr
                  | "Cond" | "cond" -> Some CTL.Cond
                  | "PointerToDecl" | "pointertodecl" -> Some CTL.PointerToDecl
                  | _  -> None }
  ;

formula_EF:
 | LEFT_PAREN formula RIGHT_PAREN EF { $2 }
;

formula_with_paren:
 | LEFT_PAREN formula RIGHT_PAREN { $2 }
;

formula:
  | formula_with_paren { $1 }
  | formula_id { CTL.Atomic($1, []) }
  | atomic_formula { Logging.out "\tParsed atomic formula\n"; $1 }
  | formula EU formula { Logging.out "\tParsed EU\n"; CTL.EU (None, $1, $3) }
  | formula AU formula { Logging.out "\tParsed AU\n"; CTL.AU ($1, $3) }
  | formula AF { Logging.out "\tParsed AF\n"; CTL.AF ($1) }
  | formula EX { Logging.out "\tParsed EX\n"; CTL.EX (None, $1) }
  | formula AX { Logging.out "\tParsed AX\n"; CTL.AX ($1) }
  | formula EG { Logging.out "\tParsed EG\n"; CTL.EG (None, $1) }
  | formula AG { Logging.out "\tParsed AG\n"; CTL.AG ($1) }
  | formula EH node_list { Logging.out "\tParsed EH\n"; CTL.EH ($3, $1) }
  | formula EF { Logging.out "\tParsed EF\n"; CTL.EF (None, $1) }
  | WHEN formula HOLDS_IN_NODE node_list
     { Logging.out "\tParsed InNode\n"; CTL.InNode ($4, $2)}
  | ET node_list WITH_TRANSITION transition_label formula_EF
     { Logging.out "\tParsed ET\n"; CTL.ET ($2, $4, $5)}
  | ETX node_list WITH_TRANSITION transition_label formula_EF
        { Logging.out "\tParsed ETX\n"; CTL.ETX ($2, $4, $5)}
  | EX WITH_TRANSITION transition_label formula_with_paren
    { Logging.out "\tParsed EX\n"; CTL.EX ($3, $4)}
  | formula AND formula { Logging.out "\tParsed AND\n"; CTL.And ($1, $3) }
  | formula OR formula { Logging.out "\tParsed OR\n"; CTL.Or ($1, $3) }
  | formula IMPLIES formula { Logging.out "\tParsed IMPLIES\n"; CTL.Implies ($1, $3) }
  | NOT formula { Logging.out "\tParsed NOT\n"; CTL.Not ($2) }
;


alexp:
 | STRING { is_not_infer_reserved_id $1;
            Logging.out "\tParsed string constant '%s' \n" $1;
            ALVar.Const $1 }
 | REGEXP LEFT_PAREN STRING RIGHT_PAREN
          { Logging.out "\tParsed regular expression '%s' \n" $3;
            ALVar.Regexp $3 }
 | identifier { is_defined_identifier $1; ALVar.Var $1 }
 ;

 identifier:
  | IDENTIFIER { is_not_infer_reserved_id $1;
                 Logging.out "\tParsed identifier '%s' \n" $1; $1 }
  ;

file_identifier:
  | FILE_IDENTIFIER { is_not_infer_reserved_id $1;
                      Logging.out "\tParsed file identifier '%s' \n" $1; $1 }
  ;
%%
