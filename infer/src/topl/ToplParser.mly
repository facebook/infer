/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{
  open !IStd

  let is_guard i = Char.is_lowercase i.[0]

  let normalize_id i = String.uncapitalize i

  let value_pattern_of_id i =
    assert (String.length i > 0) ;
    let j = normalize_id i in
    if is_guard i then ToplAst.EqualToRegister j else ToplAst.SaveInRegister j
%}

%token <int> INDENT (* The lexer uses this token only internally. *)
%token <string> CONSTANT
%token <string> ID
%token <string> STRING
%token ARROW
%token ASGN
%token COLON
%token COMMA
%token EOF
%token LC
%token LP
%token MESSAGE
%token PREFIX
%token PROPERTY
%token RC
%token RP
%token STAR

%start <ToplAst.t list> properties

%%

properties: ps=one_property* EOF { ps }

one_property:
    PROPERTY name=ID LC message=message? prefixes=prefix* transitions=transition* RC
    { ToplAst.{name; message; prefixes; transitions} }

message: MESSAGE s=STRING { s }

prefix: PREFIX c=CONSTANT { c }

transition:
    source=ID ARROW target=ID COLON label=label
    { ToplAst.{source; target; label} }

label:
    return=value_pattern ASGN c=call_pattern
    { let procedure_name, arguments = c in ToplAst.{return; procedure_name; arguments} }
  | c=call_pattern
    { let procedure_name, arguments = c in ToplAst.{return=Ignore; procedure_name; arguments} }

call_pattern: p=procedure_pattern a=arguments_pattern? { (p, a) }

procedure_pattern:
    i=ID { i }
  | c=CONSTANT { c }
  | STAR { ".*" }

arguments_pattern: LP a=separated_list(COMMA, value_pattern) RP { a }

value_pattern:
    i=ID { value_pattern_of_id i }
  | c=CONSTANT { ToplAst.EqualToConstant c }
  | STAR { ToplAst.Ignore }

%%
