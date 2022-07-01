/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{
    open JSourceAST

    let location_of_pos pos =
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      { line; col }

%}

%token PACKAGE
%token IMPORT
%token CLASS
%token INTERFACE
%token IMPLEMENTS
%token EXTENDS
%token ENUM
%token NEW
%token INSTANCEOF
%token VOID
%token THROWS
%token VAR
%token ASSERT
%token DO
%token WHILE
%token IF
%token ELSE
%token TRY
%token CATCH
%token FINALLY
%token FOR
%token BREAK
%token CONTINUE
%token RETURN
%token THROW
%token SYNCHRONIZED
%token YIELD
%token THIS
%token NULL
%token TRUE
%token FALSE


//non Java terminals
%token PRIMTYPE

%token LBRACKET
%token RBRACKET
%token LSBRACKET
%token RSBRACKET
%token LPAREN
%token RPAREN
%token LANGLE
%token RANGLE

%token EQ
%token ASSIGNOP
%token SEMICOLON
%token COLON
%token COMMA
%token DOT
%token QMARK
%token BANG
%token TILDE
%token PIPE

%token INCR_DECR
%token THREEDOTS

%token EOF

%token <string> IDENT
%token <string> INTEGER
%token <string> FLOATINGPOINT
%token <string> STRING
%token <string> CHAR

%token BINOP

%left BINOP LANGLE RANGLE
%nonassoc INSTANCEOF

%start <JSourceAST.file_content> main

%%

main:
  | package_declaration? import_declaration* app_list(type_declaration) EOF
    { { package = $1; classes = $3 } }

import_declaration:
  | IMPORT import_name SEMICOLON
    {}

import_name:
  | IDENT import_name_end
    {}
import_name_end:
  |
  | DOT BINOP
  | DOT IDENT import_name_end
    {}

package_declaration:
  | PACKAGE unann_class_or_interface_type SEMICOLON
    { $2 }

type_declaration:
  | class_declaration
  | interface_declaration
    { [$1] }
  | SEMICOLON
    { [] }

class_declaration:
  | normal_class_declaration
  | enum_declaration
    { $1 }

normal_class_declaration:
  | CLASS id=identifier superclass? superinterfaces? inner=class_body
    { {
        location = location_of_pos $startpos(id);
        kind = Class id;
        inner_elements = inner;
      }
    }

enum_declaration:
  | ENUM id=identifier superinterfaces? inner=enum_body
    { {
        location = location_of_pos $startpos(id);
        kind = Enum id;
        inner_elements = inner;
      }
    }

superclass:
  | EXTENDS class_type
    {}

superinterfaces:
  | IMPLEMENTS separated_nonempty_list(COMMA, class_type)
    {}

enum_body:
  | LBRACKET enum_constant_list RBRACKET // TODO add optional comma
    { [] }
  | LBRACKET enum_constant_list inner=enum_body_declarations RBRACKET
    { inner }

enum_constant_list:
  | separated_nonempty_list(COMMA, enum_constant)
    {}

enum_constant:
  | identifier class_body?
  | identifier LPAREN argument_list? RPAREN class_body?
    {}

enum_body_declarations:
  | SEMICOLON app_list(class_body_declaration)
    { $2 }

class_body:
  | LBRACKET app_list(class_body_declaration) RBRACKET
    { $2 }

class_body_declaration:
  | class_member_declaration
    { $1 }
  | constructor_declaration
    { $1 }

constructor_declaration:
  | constructor_declarator throws? inner=constructor_body
    { inner }

constructor_declarator:
  | identifier LPAREN formal_parameter_list RPAREN // TODO add receive_parameter
    {}

constructor_body:
  | LBRACKET inner=loption(block_statements) RBRACKET
    { inner }

argument_list:
  | app_separated_non_empty_list(COMMA, expression)
    { $1 }

class_member_declaration:
  | class_declaration
  | interface_declaration
    { [$1] }
  | field_declaration
  | method_declaration
    { $1 }
  | SEMICOLON
    { [] }

method_declaration:
  | method_header method_body
    { $2 }

interface_declaration:
  | normal_interface_declaration
    { $1 }

normal_interface_declaration:
  | INTERFACE id=identifier inner=interface_body
    { {
        location = location_of_pos $startpos(id);
        kind = Interface id;
        inner_elements = inner;
      }
    }

interface_body:
  | LBRACKET app_list(interface_member_declaration) RBRACKET
    { $2 }

interface_member_declaration:
  | constant_declaration
  | interface_method_declaration
    { [] }
  | class_declaration
  | interface_declaration
    { [$1] }

interface_method_declaration:
  | method_header method_body
    {}

method_header:
  | result method_declarator throws?
    {}

%inline
result:
  | unann_type
  | VOID
    {}

method_declarator:
  | identifier LPAREN formal_parameter_list RPAREN dims? //TODO add receiver_parameter
    {}

formal_parameter_list:
  | separated_list(COMMA, formal_parameter)
    {}

formal_parameter:
  | unann_type variable_declarator_id
  | variable_arity_parameter
    {}

variable_arity_parameter:
  | unann_type THREEDOTS identifier
    {}

method_body:
  | block
    { $1 }
  | SEMICOLON
    { [] }

block:
  | LBRACKET loption(block_statements) RBRACKET
    { $2 }

block_statements:
  | app_non_empty_list(block_statement)
    { $1 }

block_statement:
  | class_declaration
    { [$1] }
  | local_variable_declaration_statement
  | statement
    { $1 }

local_variable_declaration_statement:
  | local_variable_declaration SEMICOLON
    { $1 }

local_variable_declaration:
  | local_variable_type variable_declarator_list
    { $2 }

local_variable_type:
  | unann_type
  | VAR
    {}

statement:
  | statement_without_trailing_substatement
// TODO: add labeled_statement
  | if_then_statement
  | if_then_else_statement
  | while_statement
  | for_statement
    { $1 }

for_statement:
  | basic_for_statement
  | enhanced_for_statement
    { $1 }

for_statement_no_short_if:
  | basic_for_statement_no_short_if
  | enhanced_for_statement_no_short_if
    { $1 }

basic_for_statement:
  | FOR LPAREN loption(for_init) SEMICOLON loption(expression) SEMICOLON loption(for_update) RPAREN statement
    { $3 @ $5 @ $7 @ $9 }

enhanced_for_statement:
  | FOR LPAREN local_variable_type variable_declarator_id COLON expression RPAREN statement
    { $6 @ $8 }

enhanced_for_statement_no_short_if:
  | FOR LPAREN local_variable_type variable_declarator_id COLON expression RPAREN statement_no_short_if
    { $6 @ $8 }

basic_for_statement_no_short_if:
  | FOR LPAREN loption(for_init) SEMICOLON loption(expression) SEMICOLON loption(for_update) RPAREN statement_no_short_if
    { $3 @ $5 @ $7 @ $9 }

for_init:
  | statement_expression_list
  | local_variable_declaration
    { $1 }

for_update:
  | statement_expression_list
    { $1 }

statement_expression_list:
  | app_separated_non_empty_list(COMMA, statement_expression)
    { $1 }

if_then_statement:
  | IF LPAREN expression RPAREN statement
    { $3 @ $5 }

if_then_else_statement:
  | IF LPAREN expression RPAREN statement_no_short_if ELSE statement
    { $3 @ $5 @ $7 }

if_then_else_statement_no_short_if:
  | IF LPAREN expression RPAREN statement_no_short_if ELSE statement_no_short_if
    { $3 @ $5 @ $7 }

statement_no_short_if:
  | statement_without_trailing_substatement
  | if_then_else_statement_no_short_if
  | while_statement_no_short_if
  | for_statement_no_short_if
    { $1 }

while_statement:
  | WHILE LPAREN expression RPAREN statement
    { $3 @ $5 }

while_statement_no_short_if:
  | WHILE LPAREN expression RPAREN statement_no_short_if
    { $3 @ $5 }

statement_without_trailing_substatement:
  | block
  | empty_statement
  | expression_statement
  | assert_statement
  | do_statement
  | break_statement
  | continue_statement
  | return_statement
  | synchronized_statement
  | throw_statement
  | try_statement
  | yield_statement
    { $1 }

empty_statement:
  | SEMICOLON
    { [] }

expression_statement:
  | statement_expression SEMICOLON
    { $1 }

statement_expression:
  | INCR_DECR unary_expression
    { $2 }
  | assignment
  | postfix_expression INCR_DECR
  | method_invocation
  | class_instance_creation_expression
    { $1 }

assert_statement:
  | ASSERT expression SEMICOLON
    { $2 }
  | ASSERT expression COLON expression SEMICOLON
    { $2 @ $4 }

do_statement:
  | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    { $2 @ $5 }

break_statement:
  | BREAK identifier? SEMICOLON
    { [] }

continue_statement:
  | CONTINUE identifier? SEMICOLON
    { [] }

return_statement:
  | RETURN loption(expression) SEMICOLON
    { $2 }

synchronized_statement:
  | SYNCHRONIZED LPAREN expression RPAREN block
    { $3 }

try_statement:
  | TRY block catches
    { $2 @ $3 }
  | TRY block loption(catches) finally
    { $2 @ $3 @ $4 }

catches:
  | app_non_empty_list(catch_clause)
    { $1 }

catch_clause:
  | CATCH LPAREN catch_formal_parameter RPAREN block
    { $5 }

catch_formal_parameter:
  | catch_type variable_declarator_id
    {}

catch_type:
  | separated_nonempty_list(PIPE,unann_class_or_interface_type)
    {}

finally:
  | FINALLY block
    { $2 }

yield_statement:
  | YIELD expression SEMICOLON
    { $2 }

throw_statement:
  | THROW expression SEMICOLON
    { $2 }

throws:
  | THROWS exception_type_list
    { $2 }

exception_type_list:
  | separated_nonempty_list(COMMA, exception_type)
    { [] : string list }

exception_type:
  | unann_class_or_interface_type // WE DROP ANNOTS
    { [] : string list }

constant_declaration:
  | unann_type variable_declarator_list SEMICOLON
    { $2 }

field_declaration:
  | unann_type variable_declarator_list SEMICOLON
    { $2 }

variable_declarator_list:
  | app_separated_non_empty_list(COMMA,variable_declarator)
    { $1 }

variable_declarator:
  | variable_declarator_id
    { [] }
  | variable_declarator_id EQ variable_initializer
    { $3 }

variable_declarator_id:
  | identifier dims?
    {}

variable_initializer:
  | expression
  | array_initializer
    { $1 }

array_initializer:
  | LBRACKET RBRACKET
  | LBRACKET COMMA RBRACKET
    { [] }
  | LBRACKET variable_initializer array_initializer_end
    { $2 @ $3 }

array_initializer_end:
  | RBRACKET
  | COMMA RBRACKET
    { [] }
  | COMMA variable_initializer array_initializer_end
    { $2 @ $3 }

unann_type:
  | PRIMTYPE
  | unann_reference_type
    {}

unann_reference_type:
  | unann_class_or_interface_type
  | unann_array_type
    {}

dotted_name:
  | identifier
    { $1 }
  | identifier DOT dotted_name
    { $1 ^ "." ^ $3 }
//  | package_opt? separated_nonempty_list(DOT, IDENT)
// don't know how to write Java programs like that

%inline
unann_class_or_interface_type:
  | dotted_name { $1 }
%inline
class_type:
  | dotted_name { $1 }
%inline
class_or_interface_type_to_instantiate:
  | dotted_name { $1 }
%inline
expression_name:
  | dotted_name { $1 }

unann_array_type:
  | PRIMTYPE dims
  | unann_class_or_interface_type dims
    {}

%inline dim:
  | LSBRACKET RSBRACKET
    {}
dims:
  | dim+
    {}

expression:
  | assignment_expression
    { $1 }

assignment_expression:
  | conditional_expression
  | assignment
    { $1 }

assignment:
  | left_hand_side assignment_operator expression
    { $1 @ $3 }

assignment_operator:
  | EQ
  | ASSIGNOP
    {}

left_hand_side:
  | expression_name
    { [] }
  | field_access
  | array_access
    { $1 }

field_access:
  | primary DOT identifier
    { $1 }

array_access:
  | expression_name LSBRACKET expression RSBRACKET
    { $3 }
  | primary_no_new_array LSBRACKET expression RSBRACKET
    { $1 @ $3 }

primary:
  | primary_no_new_array
  | array_creation_expression
    { $1 }

array_creation_expression:
  | NEW PRIMTYPE dim_exprs
  | NEW class_type dim_exprs
    { $3 }
  | NEW PRIMTYPE dims array_initializer
  | NEW class_type dims array_initializer
    { $4 }

dim_exprs:
  | app_non_empty_list(dim_expr)
    { $1 }

dim_expr:
  | LSBRACKET expression RSBRACKET
    { $2 }

primary_no_new_array:
  | LPAREN expression RPAREN
    { $2 }
  | literal
  | class_literal
  | THIS
    { [] }
  | class_instance_creation_expression
  | field_access
  | array_access
  | method_invocation
    { $1 }

method_invocation:
  | expression_name LPAREN loption(argument_list) RPAREN
    { $3 }
  | primary DOT identifier LPAREN loption(argument_list) RPAREN
    { $1 @ $5 }

literal:
  | INTEGER
  | FLOATINGPOINT
  | boolean
  | CHAR
  | STRING
  | NULL
    {}

boolean:
  | TRUE
  | FALSE
    {}

class_literal:
  | type_name DOT CLASS
    {}

%inline
identifier:
  | id=IDENT { id }

%inline
type_name:
  | IDENT
    {}

class_instance_creation_expression:
  | unqualified_class_instance_creation_expression
    { $1 }
  | identifier DOT unqualified_class_instance_creation_expression
    { $3 }
  | primary DOT unqualified_class_instance_creation_expression
    { $1 @ $3 }

%inline
unqualified_class_instance_creation_expression:
  | NEW class_or_interface_type_to_instantiate LPAREN loption(argument_list) RPAREN
    { $4 }
  | NEW class_or_interface_type_to_instantiate LPAREN args=loption(argument_list) RPAREN inner=class_body
    {  args @
         [{
             location = location_of_pos $startpos(inner);
             kind = AnonymousClass;
             inner_elements = inner;
         }]
    }

conditional_expression:
  | conditional_or_expression
    { $1 }
  | conditional_or_expression QMARK expression COLON conditional_expression
    { $1 @ $3 @ $5 }

// we simpify official spec and merge many rules here
conditional_or_expression:
  | conditional_or_expression binop conditional_or_expression
    { $1 @ $3 }
  | conditional_or_expression INSTANCEOF unann_reference_type // WE DROP ANNOTS
  | unary_expression
    { $1 }

%inline
binop:
  | BINOP | RANGLE | LANGLE
    {}

unary_expression:
  | INCR_DECR unary_expression
  | BINOP unary_expression
    { $2 }
  | unary_expression_not_plus_minus
    { $1 }


unary_expression_not_plus_minus:
  | postfix_expression
  | cast_expression
    { $1 }
  | BANG unary_expression
  | TILDE unary_expression
    { $2 }

%inline
cast_expression:
  | LPAREN PRIMTYPE RPAREN unary_expression
    { $4 }

postfix_expression:
  | primary
    { $1 }
  | expression_name
    { [] }
  | postfix_expression INCR_DECR
    { $1 }

//speciazed version of Menhir macros using concatenation
app_list(X):
    { [] }
  | x = X; xs = app_list(X)
    { x@xs }

app_non_empty_list(X):
  | x = X; xs = app_list(X)
    { x@xs }

app_separated_list(SEP,X):
    xs = loption(app_separated_non_empty_list(SEP,X))
    { xs }

app_separated_non_empty_list(SEP,X):
    x = X
    { x }
  | x = X; SEP; xs = app_separated_non_empty_list(SEP,X)
    { x@xs }
