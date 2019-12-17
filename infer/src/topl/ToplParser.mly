/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{
  open !IStd
%}

%token <int> INDENT (* The lexer uses this token only internally. *)
%token <int> INTEGER
%token <string> LID
%token <string> STRING
%token <string> UID
%token AND
%token ARROW
%token ASGN
%token COLON
%token COMMA
%token EOF
%token EQ
%token GE
%token GT
%token IF
%token LC
%token LE
%token LP
%token LT
%token MESSAGE
%token NE
%token PREFIX
%token PROPERTY
%token RC
%token RP
%token STAR

%start <ToplAst.t list> properties

%%

properties: ps=one_property* EOF { ps }

one_property:
    PROPERTY name=identifier LC message=message? prefixes=prefix* transitions=transition* RC
    { ToplAst.{name; message; prefixes; transitions} }

message: MESSAGE s=STRING { s }

prefix: PREFIX s=STRING { s }

transition:
    source=identifier ARROW target=identifier COLON label=label
    { ToplAst.{source; target; label} }

label:
    return=value_pattern ASGN cp=call_pattern condition=condition?
    { let procedure_name, arguments = cp in
      let condition = Option.value condition ~default:[] in
      ToplAst.{return; procedure_name; arguments; condition} }
  | cp=call_pattern condition=condition?
    { let procedure_name, arguments = cp in
      let condition = Option.value condition ~default:[] in
      ToplAst.{return=Ignore; procedure_name; arguments; condition} }

condition: IF ps=condition_expression { ps }

condition_expression: p=predicate ps=and_predicate* { p :: ps }

predicate:
    v1=value ov2=predop_value?
    { let f (o, v2) = ToplAst.Binop (o, v1, v2) in
      Option.value_map ~default:(ToplAst.Value v1) ~f ov2 }

value:
    id=LID { ToplAst.Register id }
  | id=UID { ToplAst.Binding (String.uncapitalize id) }
  | x=INTEGER { ToplAst.Constant (Exp.Const (Const.Cint (IntLit.of_int x))) }
  | x=STRING  { ToplAst.Constant (Exp.Const (Const.Cstr x)) }

predop_value: o=predop v=value { (o, v) }

predop:
    EQ { ToplAst.OpEq }
  | NE { ToplAst.OpNe }
  | LT { ToplAst.OpLt }
  | LE { ToplAst.OpLe }
  | GT { ToplAst.OpGt }
  | GE { ToplAst.OpGe }

and_predicate: AND p=predicate { p }

call_pattern: p=procedure_pattern a=arguments_pattern? { (p, a) }

procedure_pattern:
    i=identifier { i }
  | s=STRING { s }
  | STAR { ".*" }

arguments_pattern: LP a=separated_list(COMMA, value_pattern) RP { a }

value_pattern:
    i=LID { ToplAst.EqualToRegister i }
  | i=UID { ToplAst.SaveInRegister (String.uncapitalize i) }
  | STAR { ToplAst.Ignore }

identifier: i=LID { i } | i=UID { i }

%%
