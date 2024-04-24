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
%token ARRAYWRITE
%token ARROW
%token ARROWARROW
%token COLON
%token COLONEQ
%token COMMA
%token DOT
%token EOF
%token EQ
%token GE
%token GT
%token LC
%token LE
%token LEADSTO
%token LP
%token LT
%token MESSAGE
%token NE
%token PREFIX
%token PROPERTY
%token RC
%token RP
%token SEMI
%token STAR
%token WHEN

%start <ToplAst.t list> properties

%%

properties: ps=one_property* EOF { ps }

one_property:
    PROPERTY name=identifier LC message=message? prefixes=prefix*
      transitions=transition* RC
    { ToplAst.{name; message; prefixes; transitions} }

message: MESSAGE s=STRING { s }

prefix: PREFIX s=STRING { s }

transition:
    source=state ARROW target=state COLON label=label
    { ToplAst.{source; target; label} }

state: i=identifier { i }

label:
    STAR { None }
  | procedure_name_regex=regex arg_types=arguments_pattern?
    condition=condition? action=action?
    { let condition = Option.value ~default:[] condition in
      let action = Option.value ~default:[] action in
      let arguments = Option.map ~f:(List.map ~f:fst) arg_types in
      let type_regexes = Option.map ~f:(List.map ~f:snd) arg_types in
      let pattern = ToplAst.CallPattern {procedure_name_regex; type_regexes} in
      Some ToplAst.{ arguments; condition; action; pattern } }
  | ARRAYWRITE LP arr=UID COMMA index=UID RP
    condition=condition? action=action?
    { let arguments = Some [arr; index] in
      let condition = Option.value ~default:[] condition in
      let action = Option.value ~default:[] action in
      Some ToplAst.{ arguments; condition; action; pattern= ToplAst.ArrayWritePattern } }

condition: WHEN ps=condition_expression { ps }

condition_expression: p=predicate ps=and_predicate* { p :: ps }

predicate:
    v1=value ov2=predop_value?
    { let f (o, v2) = ToplAst.Binop (o, v1, v2) in
      Option.value_map ~default:(ToplAst.Value v1) ~f ov2 }

value:
    id=LID { ToplAst.Register id }
  | id=UID { ToplAst.Binding id }
  | x=INTEGER { ToplAst.Constant (LiteralInt x) (* (Exp.Const (Const.Cint (IntLit.of_int x)))*) }
  | v=value COLON cid=identifier DOT fid=identifier
    { ToplAst.FieldAccess {value=v; class_name=cid; field_name=fid} }
  | LP v=value RP { v }
  | x=STRING { ToplAst.Constant (LiteralStr x) }

predop_value: o=predop v=value { (o, v) }

predop:
    EQ { ToplAst.OpEq }
  | NE { ToplAst.OpNe }
  | LT { ToplAst.OpLt }
  | LE { ToplAst.OpLe }
  | LEADSTO { ToplAst.LeadsTo }
  | GT { ToplAst.OpGt }
  | GE { ToplAst.OpGe }

and_predicate: AND p=predicate { p }

arguments_pattern:
    LP a=separated_list(COMMA, argument_typeopt) RP { a }

argument_typeopt:
    a=UID t=colon_regex? { (a, t) }

colon_regex:
    COLON r=regex { r }

regex:
    i=identifier { ToplAst.mk_regex i }
  | s=STRING { ToplAst.mk_regex s }

action:
    ARROWARROW a=separated_nonempty_list(SEMI, assignment) { a }

assignment:
    r=LID COLONEQ v=UID { (r, v) }

identifier: i=LID { i } | i=UID { i }

%%
