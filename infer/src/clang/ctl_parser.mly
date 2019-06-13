/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

%{
  module L = Logging

  open! IStd

  let formal_params : (ALVar.t list) ref = ref []

  let is_not_infer_reserved_id id =
    if Str.string_match (Str.regexp_string Ctl_parser_types.infer_prefix) id 0 then
      raise (CTLExceptions.ALParserInvariantViolationException
               ("ERROR: " ^ id ^ " contains __infer_ctl_ that is a reserved keyword "
            ^ "which cannot be used in identifiers:"))
     else ()

   let is_defined_identifier id =
       if (List.mem ~equal:ALVar.equal !formal_params (ALVar.Var id)) then
         L.(debug Linters Verbose) "\tParsed exp '%s' as variable" id
       else
         raise (CTLExceptions.ALParserInvariantViolationException
                  ("ERROR: Variable '" ^ id ^ "' is undefined"))

   let is_valid_astnode node_name =
      if Clang_ast_proj.is_valid_astnode_kind node_name then ()
      else
        raise (CTLExceptions.ALParserInvariantViolationException ("Invalid AST node " ^ node_name))

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
%token ET
%token WITH_TRANSITION
%token WHEN
%token HOLDS_IN_NODE
%token HOLDS_IN_OBJCCLASS
%token INTERFACE
%token IMPLEMENTATION
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
%token LEFT_SQBRACE
%token RIGHT_SQBRACE
%token AND
%token OR
%token NOT
%token IMPLIES
%token REGEXP
%token <string> IDENTIFIER
%token <string> STRING
%token WHITELIST_PATH
%token BLACKLIST_PATH
%token ACCESSOR_FOR_PROPERTY
%token ANY
%token BODY
%token COND
%token INIT_EXPR
%token FIELDS
%token FIELD_NAME
%token PARAMETERS
%token PARAMETER_NAME
%token PARAMETER_POS
%token POINTER_TO_DECL
%token SOURCE_EXPR
%token PROTOCOL
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
  | identifier { is_valid_astnode $1; [ALVar.Const $1] }
  | identifier COMMA node_list { is_valid_astnode $1; ALVar.Const($1) :: $3 }
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
  | HASHIMPORT STRING import_files
    { L.(debug Linters Verbose) "Parsed import clauses...@\n@\n"; $2 :: $3 }
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
      | _ -> L.(die ExternalError) "string '%s' cannot be set to a variable. \
                       Use the reserved variable 'report_when'" $2 in
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
      | _ -> L.(die ExternalError)
              "string '%s' cannot be set in a SET clause. \
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
  | ACCESSOR_FOR_PROPERTY alexp { Some (CTL.AccessorForProperty $2) }
  | ANY { None }
  | BODY { Some CTL.Body }
  | COND { Some CTL.Cond }
  | FIELDS { Some CTL.Fields }
  | FIELD_NAME alexp { Some (CTL.FieldName $2) }
  | INIT_EXPR { Some CTL.InitExpr }
  | PARAMETERS { Some CTL.Parameters }
  | PARAMETER_NAME alexp { Some (CTL.ParameterName $2) }
  | PARAMETER_POS alexp { Some (CTL.ParameterPos $2) }
  | POINTER_TO_DECL { Some CTL.PointerToDecl }
  | SOURCE_EXPR { Some CTL.SourceExpr}
  | PROTOCOL { Some CTL.Protocol }
  ;

formula_EF:
 | LEFT_PAREN formula RIGHT_PAREN EF { $2 }
;

formula_with_paren:
 | LEFT_PAREN formula RIGHT_PAREN { $2 }
;

when_formula:
| INTERFACE  LEFT_SQBRACE formula RIGHT_SQBRACE
  IMPLEMENTATION LEFT_SQBRACE formula RIGHT_SQBRACE HOLDS_IN_OBJCCLASS
   { L.(debug Linters Verbose) "\tParsed HOLDS-IN-OBJC-CLASS @\n";
        CTL.InObjCClass ($3, $7) }
| formula HOLDS_IN_NODE node_list
     { L.(debug Linters Verbose) "\tParsed InNode@\n"; CTL.InNode ($3, $1)}
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
     { L.(debug Linters Verbose) "\tParsed EX WITH-TRANSITION '%a'@\n" CTL.Debug.pp_transition $4;
       CTL.EX ($4, $1) }
  | formula AX { L.(debug Linters Verbose) "\tParsed AX@\n"; CTL.AX (None, $1) }
  | formula AX WITH_TRANSITION transition_label
     { L.(debug Linters Verbose) "\tParsed AX WITH-TRANSITION '%a'@\n" CTL.Debug.pp_transition $4;
       CTL.AX ($4, $1) }
  | formula EG { L.(debug Linters Verbose) "\tParsed EG@\n"; CTL.EG (None, $1) }
  | formula AG { L.(debug Linters Verbose) "\tParsed AG@\n"; CTL.AG (None, $1) }
  | formula EH node_list { L.(debug Linters Verbose) "\tParsed EH@\n"; CTL.EH ($3, $1) }
  | formula EF { L.(debug Linters Verbose) "\tParsed EF@\n"; CTL.EF (None, $1) }
  | formula EF WITH_TRANSITION transition_label
     { L.(debug Linters Verbose) "\tParsed EF WITH-TRANSITION '%a'@\n" CTL.Debug.pp_transition $4;
       CTL.EF($4, $1) }
  | WHEN when_formula { $2 }
  | ET node_list WITH_TRANSITION transition_label formula_EF
     { L.(debug Linters Verbose) "\tParsed ET with transition '%a'@\n" CTL.Debug.pp_transition $4;
       CTL.ET ($2, $4, $5)}
  | EX WITH_TRANSITION transition_label formula_with_paren
    { L.(debug Linters Verbose) "\tParsed EX with transition '%a'@\n" CTL.Debug.pp_transition $3;
      CTL.EX ($3, $4)}
  | AX WITH_TRANSITION transition_label formula_with_paren
      { L.(debug Linters Verbose)
        "\tParsed AX with transition '%a'@\n" CTL.Debug.pp_transition $3;
        CTL.AX ($3, $4)}
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
            ALVar.Regexp {string=$3; regexp=lazy (Str.regexp $3)} }

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
%%
