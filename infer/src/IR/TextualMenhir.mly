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
      Location.known ~line ~col

%}

%token AMPERSAND
%token ASSIGN
%token ATTRIBUTE
%token COLON
%token COMMA
%token DECLARE
%token DEFINE
%token DOT
%token EOF
%token EQ
%token EXTENDS
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
%token UNREACHABLE
%token VOID

%token <string> IDENT
%token <int> LOCAL
%token <Z.t> INTEGER
%token <float> FLOATINGPOINT
%token <string> LABEL
%token <string> STRING

%start <SourceFile.t -> Textual.Module.t> main
%type <Attr.t> attribute
%type <Module.decl> declaration
%type <Procname.qualified_name> qualified_pname
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
%type <NodeName.t * (Ident.t * Typ.t) list> label
%type <Module.decl list> list(declaration)
%type <Instr.t list> list(instruction)
%type <Exp.t list> loption(separated_nonempty_list(COMMA,expression))
%type <NodeName.t list> loption(separated_nonempty_list(COMMA,nname))
%type <Terminator.node_call list> loption(separated_nonempty_list(COMMA,node_call))
%type <Typ.t list> loption(separated_nonempty_list(COMMA,typ))
%type <(Typ.t * VarName.t) list> loption(separated_nonempty_list(COMMA,typed_var))
%type <(Typ.t * FieldBaseName.t) list> loption(separated_nonempty_list(SEMICOLON,typed_field))
%type <Node.t list> nonempty_list(block)
%type <Exp.t list> separated_nonempty_list(COMMA,expression)
%type <NodeName.t list> separated_nonempty_list(COMMA,nname)
%type <Terminator.node_call list> separated_nonempty_list(COMMA,node_call)
%type <Typ.t list> separated_nonempty_list(COMMA,typ)
%type <(Typ.t * VarName.t) list> separated_nonempty_list(COMMA,typed_var)
%type <(Typ.t * FieldBaseName.t) list> separated_nonempty_list(SEMICOLON,typed_field)
%type <TypeName.t list> extends
%type <Attr.t list> list(attribute)
%type <TypeName.t list option> option(extends)
%type <Ident.t * Typ.t> typed_ident
%type <(Ident.t * Typ.t) list> separated_nonempty_list(COMMA,typed_ident)
%type <TypeName.t list> separated_nonempty_list(COMMA,tname)

%%

main:
  | attrs=attribute* decls=declaration* EOF
    { (fun sourcefile -> { attrs; decls; sourcefile }) }

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

qualified_pname:
  | tname=tname DOT name=pname
    { {enclosing_class=Enclosing tname; name} }
  | name=pname
    { {enclosing_class=TopLevel; name} }

attribute:
  | ATTRIBUTE name=IDENT EQ value=STRING
    { {name; value; loc=location_of_pos $startpos} }

extends:
  | EXTENDS supers=separated_nonempty_list(COMMA,tname)
  { supers }

declaration:
  | GLOBAL name=vname
    { let pvar : Pvar.t = {name; kind=Global} in
      Global pvar }
  | TYPE name=tname supers=extends? ioption(EQ) LBRACKET l=separated_list(SEMICOLON, typed_field) RBRACKET
    { let fields =
        List.map l ~f:(fun (typ, name_f) ->
                        {Fieldname.name=name_f; typ; enclosing_type=name}) in
      let supers = Option.value supers ~default:[] in
      Struct {name; supers; fields; methods=[]} }
  | DECLARE qualified_name=qualified_pname LPAREN
            formals_types = separated_list(COMMA, typ) RPAREN COLON result_type=typ
    { let pname : Procname.t = {qualified_name; formals_types; result_type} in
      Procname pname
    }
  | DEFINE qualified_name=qualified_pname LPAREN
           params = separated_list(COMMA, typed_var) RPAREN COLON result_type=typ
                         LBRACKET nodes=block+ RBRACKET
    { let formals_types = List.map ~f:fst params in
      let procname : Procname.t = {qualified_name; formals_types; result_type} in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:snd params in
      let exit_loc = location_of_pos $endpos in
      Proc { procname; nodes; start= start_node.Node.label; params; exit_loc}
    }

base_typ:
  | INT
    { Int }
  | FLOAT
    { Float }
  | VOID
    { Void }
  | name=tname
    { Struct name }
  | typ=base_typ LSBRACKET RSBRACKET
    { Array typ }
  | LPAREN typ=typ RPAREN
    { typ }

typ:
  | typ=base_typ
    { typ }
  | STAR typ=typ
    { Ptr typ }

typed_field:
  | name=fname COLON typ=typ
    { (typ, name) }

typed_var:
  | name=vname COLON typ=typ
    { (typ, name) }

typed_ident:
  | id=LOCAL COLON typ=typ
    { (Ident.of_int id, typ) }

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
  | label=LABEL LPAREN l=separated_nonempty_list(COMMA, typed_ident) RPAREN COLON
    { let label : NodeName.t = { value=label; loc=location_of_pos $startpos(label)} in
      label, l }

const:
  | INTEGER
    { Int $1 }
  | STRING
    { Str $1 }
  | FLOATINGPOINT
    { Float $1 }
  | TRUE
    { Int Z.one }
  | FALSE
    { Int Z.zero }
  | NULL
    { Null }

instruction:
  | id=LOCAL COLON typ=typ EQ LOAD exp=expression
    { Load {id= Ident.of_int id; exp; typ; loc=location_of_pos $startpos } }
  | STORE exp1=expression ASSIGN exp2=expression COLON typ=typ
    { Store {exp1; exp2; typ; loc=location_of_pos $startpos } }
  | PRUNE exp=expression
    { Prune {exp; loc=location_of_pos $startpos} }
  | PRUNE NOT exp=expression
    { Prune {exp=Exp.not exp; loc=location_of_pos $startpos} }
  | id=LOCAL EQ exp=expression
    { Let { id= Ident.of_int id; exp; loc=location_of_pos $startpos } }

terminator:
  | RET e=expression
    { Ret e }
  | JMP l=separated_list(COMMA, node_call)
    { Jump l }
  | UNREACHABLE
    { Unreachable }
  | THROW e=expression
    { Throw e }

node_call:
  | label=nname
    { {label; ssa_args=[]} }
  | label=nname LPAREN ssa_args=separated_nonempty_list(COMMA, expression) RPAREN
    { {label; ssa_args} }

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
    { Field {exp; tname; fname} }
  | e1=expression LSBRACKET e2=expression RSBRACKET
    { Index (e1, e2) }
  | c=const
    { Const c }
  | proc=qualified_pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { Call {proc; args; kind= Exp.NonVirtual} }
  | recv=expression DOT proc=qualified_pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { Exp.call_virtual proc recv args }
  | LPAREN e=expression COLON t=typ RPAREN
    { Cast (t, e) }
