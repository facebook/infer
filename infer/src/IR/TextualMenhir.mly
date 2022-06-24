/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{

    open !IStd
    open Textual

    let location_of_pos pos : Location.t =
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      { line; col }

%}

%token AMPERSAND
%token ASSIGN
%token COLON
%token COMMA
%token DECLARE
%token DEFINE
%token DOT
%token EOF
%token EQ
%token FALSE
%token FLOAT
%token GLOBAL
%token HANDLERS
%token INT
%token JMP
%token LBRACKET
%token LOAD
%token LPAREN
%token LSBRACKET
%token NULL
%token NOT
%token PRUNE
%token RBRACKET
%token RET
%token RPAREN
%token RSBRACKET
%token SEMICOLON
%token STAR
%token STORE
%token THROW
%token TRUE
%token TYPE
%token VOID

%token <Textual.name> IDENT
%token <int> LOCAL
%token <Z.t> INTEGER
%token <float> FLOATINGPOINT
%token <Textual.name> LABEL
%token <string> STRING

%start <Textual.Module.t> main
%type <Module.decl> declaration
%type <Typ.t> typ
%type <Typ.t> base_typ
%type <Typ.t * name> typed_param
%type <Instr.t> instruction
%type <Terminator.t> terminator
%type <Exp.t> expression
%type <Const.t> const
%type <node_label list> opt_handlers
%type <Node.t> block
%type <Terminator.node_call> node_call

%%

main:
  | l=declaration* EOF
    { l }

declaration:
  | GLOBAL name=IDENT
    { let pvar : Pvar.t = {name; kind=Global} in
      Global pvar }
  | TYPE id=IDENT EQ LBRACKET l=separated_list(SEMICOLON, typed_param) RBRACKET
    { let loc = location_of_pos $startpos(id) in
      let name = TypName.of_java_classname id in
      let fields =
        List.map l ~f:(fun (typ, name_f) ->
                        {Fieldname.name=name_f; typ; enclosing_type=name}) in
      Struct {name; fields; loc} }
  | DECLARE name=IDENT LPAREN targs = separated_list(COMMA, typ) RPAREN COLON tres=typ
    { let pkind : Procname.proc_kind = NonVirtual in
      let kind : Procname.kind = Proc { pkind } in
      let pname : Procname.t = {name; targs; tres; kind} (* FIXME *) in
      Procname pname
    }
  | DEFINE name=IDENT LPAREN params = separated_list(COMMA, typed_param) RPAREN COLON tres=typ
                         LBRACKET nodes=block+ RBRACKET
    { let targs = List.map ~f:fst params in
      let pkind : Procname.proc_kind = NonVirtual in
      let kind : Procname.kind = Proc { pkind } in
      let pname : Procname.t = {name; targs; tres; kind} (* FIXME *) in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:snd params in
      let sourcefile = SourceFile.invalid "TODO" in
      Proc { name=pname; nodes; start= start_node.Node.label; params; sourcefile}
    }

base_typ:
  | INT
    { Tint }
  | FLOAT
    { Tfloat }
  | VOID
    { Tvoid }
  | id=IDENT
    { Tstruct (TypName.of_java_classname id) }
  | typ=base_typ LSBRACKET RSBRACKET
    { Tarray typ }
  | LPAREN typ=typ RPAREN
    { typ }

typ:
  | typ=base_typ
    { typ }
  | STAR typ=typ
    { Tptr typ }

typed_param:
  | id=IDENT COLON typ=typ
    { (typ, id) }

block:
  | lab=label instrs=instruction* last=terminator exn_succs=opt_handlers
    {
      let last_loc = location_of_pos $startpos(last) in
      let label_loc = location_of_pos $startpos(lab) in
      let label, ssa_parameters = lab in
      { label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc } }

label:
  | label=LABEL COLON
    { label, [] }
  | label=LABEL LPAREN l=separated_nonempty_list(COMMA, LOCAL) RPAREN COLON
    { label, List.map ~f:Ident.of_int l }

const:
  | INTEGER
    { Cint $1 }
  | STRING
    { Cstr $1 }
  | FLOATINGPOINT
    { Cfloat $1 }
  | TRUE
    { Cint Z.one }
  | FALSE
    { Cint Z.zero }
  | NULL
    { Cnull }

instruction:
  | id=LOCAL COLON typ=typ EQ LOAD e=expression
    { Load {id= Ident.of_int id; e; typ; loc=location_of_pos $startpos } }
  | STORE e1=expression ASSIGN e2=expression COLON typ=typ
    { Store {e1; e2; typ; loc=location_of_pos $startpos } }
  | PRUNE e=expression
    { Prune {e; b= true; loc=location_of_pos $startpos} }
  | PRUNE NOT e=expression
    { Prune {e; b= false; loc=location_of_pos $startpos} }
  | r=LOCAL EQ f=IDENT LPAREN args=separated_list(COMMA, expression) RPAREN
    { Call { ret= Ident.of_int r; f; args; loc=location_of_pos $startpos } }

terminator:
  | RET e=expression
    { Ret e }
  | JMP l=separated_list(COMMA, node_call)
    { Jump l }
  | THROW e=expression
    { Throw e }

node_call:
  | label=IDENT
    { {label; ssa_args=[]} }
  | label=IDENT LPAREN l=separated_nonempty_list(COMMA, LOCAL) RPAREN
    { {label; ssa_args=List.map ~f:Ident.of_int l} }

opt_handlers:
  | { [] }
  | HANDLERS l=separated_list(COMMA, IDENT)
    { l }

expression:
  | id=LOCAL
    { Var (Ident.of_int id) }
  | AMPERSAND id=IDENT
    { Lvar id }
  | e=expression DOT f=IDENT
    { Lfield (e, f) }
  | e1=expression LSBRACKET e2=expression RSBRACKET
    { Lindex (e1, e2) }
  | c=const
    { EConst c }
  | f=IDENT LPAREN args=separated_list(COMMA, expression) RPAREN
    { ECall (f, args) }
  | LPAREN e=expression COLON t=typ RPAREN
    { ECast (t, e) }
