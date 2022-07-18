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

%token <string> IDENT
%token <int> LOCAL
%token <Z.t> INTEGER
%token <float> FLOATINGPOINT
%token <string> LABEL
%token <string> STRING

%start <SourceFile.t -> Textual.Module.t> main
%type <Module.decl> declaration
%type <ProcBaseName.t> pname
%type <FieldBaseName.t> fname
%type <NodeName.t> nname
%type <TypeName.t> tname
%type <VarName.t> vname
%type <Typ.t> typ
%type <Typ.t> base_typ
%type <Typ.t * FieldBaseName.t> typed_field
%type <Typ.t * VarName.t> typed_var
%type <Instr.t> instruction
%type <Terminator.t> terminator
%type <Exp.t> expression
%type <Const.t> const
%type <NodeName.t list> opt_handlers
%type <Node.t> block
%type <Terminator.node_call> node_call
%type <NodeName.t * Ident.t list> label
%type <Module.decl list> list(declaration)
%type <Instr.t list> list(instruction)
%type <Exp.t list> loption(separated_nonempty_list(COMMA,expression))
%type <Terminator.node_call list> loption(separated_nonempty_list(COMMA,node_call))
%type <Typ.t list> loption(separated_nonempty_list(COMMA,typ))
%type <(Typ.t * VarName.t) list> loption(separated_nonempty_list(COMMA,typed_var))
%type <(Typ.t * FieldBaseName.t) list> loption(separated_nonempty_list(SEMICOLON,typed_field))
%type <Node.t list> nonempty_list(block)
%type <int list> separated_nonempty_list(COMMA,LOCAL)
%type <Exp.t list> separated_nonempty_list(COMMA,expression)
%type <Terminator.node_call list> separated_nonempty_list(COMMA,node_call)
%type <Typ.t list> separated_nonempty_list(COMMA,typ)
%type <(Typ.t * VarName.t) list> separated_nonempty_list(COMMA,typed_var)
%type <(Typ.t * FieldBaseName.t) list> separated_nonempty_list(SEMICOLON,typed_field)

%%

main:
  | l=declaration* EOF
    { (fun sourcefile -> { decls=l; sourcefile}) }

pname:
  | id=IDENT
    { { value=id; loc=location_of_pos $startpos(id) } }

fname:
  | id=IDENT
    { { value=id; loc=location_of_pos $startpos(id) } }

nname:
  | id=IDENT
    { { value=id; loc=location_of_pos $startpos(id) } }

tname:
  | id=IDENT
    { { value=id; loc=location_of_pos $startpos(id) } }

vname:
  | id=IDENT
    { { value=id; loc=location_of_pos $startpos(id) } }

declaration:
  | GLOBAL name=vname
    { let pvar : Pvar.t = {name; kind=Global} in
      Global pvar }
  | TYPE name=tname EQ LBRACKET l=separated_list(SEMICOLON, typed_field) RBRACKET
    { let fields =
        List.map l ~f:(fun (typ, name_f) ->
                        {Fieldname.name=name_f; typ; enclosing_type=name}) in
      Struct {name; fields} }
  | DECLARE name=pname LPAREN targs = separated_list(COMMA, typ) RPAREN COLON tres=typ
    { let pkind : Procname.proc_kind = NonVirtual in
      let kind : Procname.kind = Proc { pkind } in
      let pname : Procname.t = {name; targs; tres; kind} (* FIXME: deals with virutal kind *) in
      Procname pname
    }
  | DEFINE name=pname LPAREN params = separated_list(COMMA, typed_var) RPAREN COLON tres=typ
                         LBRACKET nodes=block+ RBRACKET
    { let targs = List.map ~f:fst params in
      let pkind : Procname.proc_kind = NonVirtual in
      let kind : Procname.kind = Proc { pkind } in
      let procname : Procname.t = {name; targs; tres; kind} (* FIXME:: deals with virutal kind *) in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:snd params in
      Proc { procname; nodes; start= start_node.Node.label; params}
    }

base_typ:
  | INT
    { Tint }
  | FLOAT
    { Tfloat }
  | VOID
    { Tvoid }
  | name=tname
    { Tstruct name }
  | typ=base_typ LSBRACKET RSBRACKET
    { Tarray typ }
  | LPAREN typ=typ RPAREN
    { typ }

typ:
  | typ=base_typ
    { typ }
  | STAR typ=typ
    { Tptr typ }

typed_field:
  | name=fname COLON typ=typ
    { (typ, name) }

typed_var:
  | name=vname COLON typ=typ
    { (typ, name) }

block:
  | lab=label instrs=instruction* last=terminator exn_succs=opt_handlers
    {
      let last_loc = location_of_pos $startpos(last) in
      let label_loc = location_of_pos $startpos(lab) in
      let label, ssa_parameters = lab in
      { label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc } }

label:
  | label=LABEL COLON
    { let label : NodeName.t = { value=label; loc=location_of_pos $startpos(label)} in
      label, [] }
  | label=LABEL LPAREN l=separated_nonempty_list(COMMA, LOCAL) RPAREN COLON
    { let label : NodeName.t = { value=label; loc=location_of_pos $startpos(label)} in
      label, List.map ~f:Ident.of_int l }

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
  | id=LOCAL COLON typ=typ EQ LOAD exp=expression
    { Load {id= Ident.of_int id; exp; typ; loc=location_of_pos $startpos } }
  | STORE exp1=expression ASSIGN exp2=expression COLON typ=typ
    { Store {exp1; exp2; typ; loc=location_of_pos $startpos } }
  | PRUNE exp=expression
    { Prune {exp; b= true; loc=location_of_pos $startpos} }
  | PRUNE NOT exp=expression
    { Prune {exp; b= false; loc=location_of_pos $startpos} }
  | r=LOCAL EQ f=pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { Call { ret= Ident.of_int r; f; args; loc=location_of_pos $startpos } }

terminator:
  | RET e=expression
    { Ret e }
  | JMP l=separated_list(COMMA, node_call)
    { Jump l }
  | THROW e=expression
    { Throw e }

node_call:
  | label=nname
    { {label; ssa_args=[]} }
  | label=nname LPAREN l=separated_nonempty_list(COMMA, LOCAL) RPAREN
    { {label; ssa_args=List.map ~f:Ident.of_int l} }

opt_handlers:
  | { [] }
  | HANDLERS l=separated_list(COMMA, nname)
    { l }

expression:
  | id=LOCAL
    { Var (Ident.of_int id) }
  | AMPERSAND name=vname
    { Lvar name }
  | exp=expression DOT tname=tname DOT fname=fname
    { Lfield {exp; tname; fname} }
  | e1=expression LSBRACKET e2=expression RSBRACKET
    { Lindex (e1, e2) }
  | c=const
    { EConst c }
  | f=pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { ECall (f, args) }
  | LPAREN e=expression COLON t=typ RPAREN
    { ECast (t, e) }
