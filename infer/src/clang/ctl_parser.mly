/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

%{
  module L = Logging

  open! IStd

  let formal_params : (ALVar.t list) ref = ref []

  let is_not_infer_reserved_id id =
    if Str.string_match (Str.regexp_string Ctl_parser_types.infer_prefix) id 0 then
      raise (Ctl_parser_types.ALParsingException
               ("ERROR: " ^ id ^ " contains __infer_ctl_ that is a reserved keyword "
            ^ "which cannot be used in identifiers:"))
     else ()

   let is_defined_identifier id =
       if (List.mem ~equal:ALVar.equal !formal_params (ALVar.Var id)) then
         L.(debug Linters Verbose) "\tParsed exp '%s' as variable" id
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
%token GLOBAL_PATHS
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
%token WHITELIST_PATH
%token BLACKLIST_PATH
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
  | import_files global_macros global_paths checkers_list {
      { CTL.import_files = $1; CTL.global_macros = $2; CTL.global_paths = $3; CTL.checkers = $4 }
    }
  ;

import_files:
  | { [] }
  | HASHIMPORT LESS_THAN file_identifier GREATER_THAN import_files
    { L.(debug Linters Verbose) "Parsed import clauses...@\n@\n"; $3 :: $5 }
  ;

global_macros:
  | { [] }
  | GLOBAL_MACROS LEFT_BRACE let_clause_list RIGHT_BRACE SEMICOLON
    { L.(debug Linters Verbose) "Parsed global macro definitions...@\n@\n"; $3 }
  ;

global_path_declaration:
  | LET identifier ASSIGNMENT LEFT_BRACE path_list RIGHT_BRACE SEMICOLON { ($2, $5) }
;

global_paths_list:
  | global_path_declaration { [$1] }
  | global_path_declaration SEMICOLON global_paths_list { $1 :: $3 }
;

global_paths:
  | { [] }
  | GLOBAL_PATHS LEFT_BRACE global_paths_list RIGHT_BRACE SEMICOLON
    { L.(debug Linters Verbose) "Parsed global path definitions...@\n"; $3 }
  ;

checkers_list:
  | EOF { [] }
  | checker SEMICOLON checkers_list { $1::$3 }
  ;

checker:
 DEFINE_CHECKER identifier ASSIGNMENT LEFT_BRACE clause_list RIGHT_BRACE
  {
    L.(debug Linters Verbose) "@\nParsed checker definition@\n";
    let c = { CTL.id = $2; CTL.definitions = $5 } in
    CTL.print_checker c;
    c
  }
;

path_list:
 | alexp_path { [$1] }
 | alexp_path COMMA path_list { $1 :: $3 }
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
    { L.(debug Linters Verbose) "\tParsed SET clause@\n";
    let alvar = match $2 with
      | "report_when" -> ALVar.Report_when
      | _ -> failwith "string '%s' cannot be set to a variable. \
                       Use the reserverd variable 'report_when'" in
      CTL.CSet (alvar, $4) }
  | SET WHITELIST_PATH ASSIGNMENT LEFT_BRACE path_list RIGHT_BRACE
    { CTL.CPath (`WhitelistPath, $5) }
  | SET BLACKLIST_PATH ASSIGNMENT LEFT_BRACE path_list RIGHT_BRACE
    { CTL.CPath (`BlacklistPath, $5) }
  | SET identifier ASSIGNMENT STRING
    { L.(debug Linters Verbose) "\tParsed SET clause@\n";
      let alvar = match $2 with
      | "message" -> ALVar.Message
      | "suggestion" -> ALVar.Suggestion
      | "severity" -> ALVar.Severity
      | "mode" -> ALVar.Mode
      | "doc_url" -> ALVar.Doc_url
      | "name" -> ALVar.Name
      | _ -> failwithf "string '%s' cannot be set in a SET clause. \
                        Use either of: \
                        'doc_url', 'message', 'mode', 'name', 'severity' or 'suggestion'" $2 in
      CTL.CDesc (alvar, $4) }
    | let_clause { $1 }
    ;

let_clause:
  | LET formula_id_def ASSIGNMENT formula
    { L.(debug Linters Verbose) "\tParsed LET clause@\n"; CTL.CLet ($2, [], $4) }
  | LET formula_id_def LEFT_PAREN formal_params RIGHT_PAREN ASSIGNMENT formula
    { L.(debug Linters Verbose) "\tParsed let clause with formula identifier '%s(....)'@\n"
        (ALVar.formula_id_to_string $2);
      CTL.CLet ($2, $4, $7) }
  ;

atomic_formula:
  | TRUE { L.(debug Linters Verbose) "\tParsed True@\n"; CTL.True }
  | FALSE { L.(debug Linters Verbose) "\tParsed False@\n"; CTL.False }
  | identifier LEFT_PAREN actual_params RIGHT_PAREN
    { L.(debug Linters Verbose) "\tParsed predicate@\n"; CTL.Atomic(ALVar.Formula_id $1, $3) }
  ;

  formula_id_def:
  | identifier { L.(debug Linters Verbose) "\tParsed formula identifier '%s'@\n" $1;
                  formal_params := [];
                  ALVar.Formula_id $1 }
   ;

 formula_id:
 | identifier { L.(debug Linters Verbose) "\tParsed formula identifier '%s'@\n" $1;
                 ALVar.Formula_id $1 }
  ;

actual_params:
  | {[]}
  | alexp { [$1] }
  | alexp COMMA actual_params { $1 :: $3 }
  ;

transition_label:
  | identifier { match $1 with
                  | "Body" | "body" -> "Body", Some CTL.Body
                  | "Protocol" | "protocol" -> "Protocol", Some CTL.Protocol
                  | "InitExpr" | "initexpr" -> "InitExpr", Some CTL.InitExpr
                  | "Cond" | "cond" -> "Cond", Some CTL.Cond
                  | "Parameters" | "parameters" -> "Parameters", Some CTL.Parameters
                  | "PointerToDecl" | "pointertodecl" -> "PointerToDecl", Some CTL.PointerToDecl
                  | _  -> "None", None }
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
  | atomic_formula { L.(debug Linters Verbose) "\tParsed atomic formula@\n"; $1 }
  | formula EU formula { L.(debug Linters Verbose) "\tParsed EU@\n"; CTL.EU (None, $1, $3) }
  | formula AU formula { L.(debug Linters Verbose) "\tParsed AU@\n"; CTL.AU (None,$1, $3) }
  | formula AF { L.(debug Linters Verbose) "\tParsed AF@\n"; CTL.AF (None,$1) }
  | formula EX { L.(debug Linters Verbose) "\tParsed EX@\n"; CTL.EX (None, $1) }
  | formula EX WITH_TRANSITION transition_label
     { L.(debug Linters Verbose) "\tParsed EX WITH-TRANSITION '%s'@\n" (fst $4);
       CTL.EX (snd $4, $1) }
  | formula AX { L.(debug Linters Verbose) "\tParsed AX@\n"; CTL.AX (None, $1) }
  | formula AX WITH_TRANSITION transition_label
     { L.(debug Linters Verbose) "\tParsed AX WITH-TRANSITION '%s'@\n" (fst $4);
       CTL.AX (snd $4, $1) }
  | formula EG { L.(debug Linters Verbose) "\tParsed EG@\n"; CTL.EG (None, $1) }
  | formula AG { L.(debug Linters Verbose) "\tParsed AG@\n"; CTL.AG (None, $1) }
  | formula EH node_list { L.(debug Linters Verbose) "\tParsed EH@\n"; CTL.EH ($3, $1) }
  | formula EF { L.(debug Linters Verbose) "\tParsed EF@\n"; CTL.EF (None, $1) }
  | formula EF WITH_TRANSITION transition_label
     { L.(debug Linters Verbose) "\tParsed EF WITH-TRANSITION '%s'@\n" (fst $4);
       CTL.EF(snd $4, $1) }
  | WHEN formula HOLDS_IN_NODE node_list
     { L.(debug Linters Verbose) "\tParsed InNode@\n"; CTL.InNode ($4, $2)}
  | ET node_list WITH_TRANSITION transition_label formula_EF
     { L.(debug Linters Verbose) "\tParsed ET with transition '%s'@\n" (fst $4);
       CTL.ET ($2, snd $4, $5)}
  | ETX node_list WITH_TRANSITION transition_label formula_EF
     { L.(debug Linters Verbose) "\tParsed ETX ith transition '%s'@\n" (fst $4);
       CTL.ETX ($2, snd $4, $5)}
  | EX WITH_TRANSITION transition_label formula_with_paren
    { L.(debug Linters Verbose) "\tParsed EX with transition '%s'@\n" (fst $3);
      CTL.EX (snd $3, $4)}
  | AX WITH_TRANSITION transition_label formula_with_paren
      { L.(debug Linters Verbose) "\tParsed AX with transition '%s'@\n" (fst $3);
        CTL.AX (snd $3, $4)}
  | formula AND formula { L.(debug Linters Verbose) "\tParsed AND@\n"; CTL.And ($1, $3) }
  | formula OR formula { L.(debug Linters Verbose) "\tParsed OR@\n"; CTL.Or ($1, $3) }
  | formula IMPLIES formula
      { L.(debug Linters Verbose) "\tParsed IMPLIES@\n"; CTL.Implies ($1, $3) }
  | NOT formula { L.(debug Linters Verbose) "\tParsed NOT@\n"; CTL.Not ($2) }
;


alexp_const: STRING
          { is_not_infer_reserved_id $1;
           L.(debug Linters Verbose) "\tParsed string constant '%s'@\n" $1;
           ALVar.Const $1 }

alexp_regex:  REGEXP LEFT_PAREN STRING RIGHT_PAREN
          { L.(debug Linters Verbose) "\tParsed regular expression '%s'@\n" $3;
            ALVar.Regexp $3 }

alexp_var: identifier { is_defined_identifier $1; ALVar.Var $1 }

alexp:
 | alexp_const {$1}
 | alexp_regex {$1}
 | alexp_var { $1}
 ;

 alexp_path:
  | alexp_const {$1}
  | alexp_regex {$1}
  | identifier { ALVar.Var $1 }
  ;

 identifier:
  | IDENTIFIER { is_not_infer_reserved_id $1;
                 L.(debug Linters Verbose) "\tParsed identifier '%s'@\n" $1; $1 }
  ;

file_identifier:
  | FILE_IDENTIFIER { is_not_infer_reserved_id $1;
                      L.(debug Linters Verbose) "\tParsed file identifier '%s'@\n" $1; $1 }
  ;
%%
