/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{

    open! IStd
    open Textual

    let location_of_pos pos : Location.t =
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      Location.known ~line ~col
%}

%token AMPERSAND
%token ASSIGN
%token COLON
%token COMMA
%token DECLARE
%token DEFINE
%token DOT
%token ELLIPSIS
%token EOF
%token EQ
%token EXTENDS
%token FALSE
%token FLOAT
%token GLOBAL
%token HANDLERS
%token INT
%token JMP
%token LABRACKET
%token LBRACKET
%token LOAD
%token LOCALKEYWORD
%token LPAREN
%token LSBRACKET
%token NULL
%token NOT
%token PRUNE
%token RABRACKET
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
%type <qualified_procname> qualified_pname
%type <ProcName.t> pname
%type <FieldName.t> fname
%type <NodeName.t> nname
%type <TypeName.t> tname
%type <VarName.t> vname
%type <Attr.t list> annots
%type <Attr.t> annot
%type <Typ.t> typ
%type <Typ.annotated> annotated_typ
%type <Typ.t> base_typ
%type <FieldName.t * Typ.annotated> typed_field
%type <VarName.t * Typ.annotated> typed_var
%type <Instr.t> instruction
%type <Terminator.t> terminator
%type <Exp.t> expression
%type <Const.t> const
%type <NodeName.t list> opt_handlers
%type <Node.t> block
%type <Terminator.node_call> node_call
%type <NodeName.t * (Ident.t * Typ.t) list> label
%type <TypeName.t list> extends
%type <Ident.t * Typ.t> typed_ident

%%

ident:
  | DECLARE { "declare" }
  | DEFINE { "define" }
  | EXTENDS { "extends" }
  | GLOBAL { "global" }
  | JMP { "jmp" }
  | LOAD { "load" }
  | LOCALKEYWORD { "local" }
  | PRUNE { "prune" }
  | RET { "ret" }
  | STORE { "store" }
  | THROW { "throw" }
  | TYPE { "type" }
  | UNREACHABLE { "unreachable" }
  | x=IDENT { x }

main:
  | attrs=attribute* decls=declaration* EOF
    { (fun sourcefile -> { attrs; decls; sourcefile }) }

pname:
  | id=ident
    { { value=id; loc=location_of_pos $startpos(id) } }

fname:
  | id=ident
    { { value=id; loc=location_of_pos $startpos(id) } }

nname:
  | id=ident
    { { value=id; loc=location_of_pos $startpos(id) } }

tname:
  | id=ident
    { { value=id; loc=location_of_pos $startpos(id) } }

vname:
  | id=ident
    { { value=id; loc=location_of_pos $startpos(id) } }

qualified_pname:
  | tname=tname DOT name=pname
    { {enclosing_class=Enclosing tname; name} }
  | name=pname
    { {enclosing_class=TopLevel; name} }

attribute:
  | DOT name=ident EQ value=STRING
    { {name; values=[value]; loc=location_of_pos $startpos} }

extends:
  | EXTENDS supers=separated_nonempty_list(COMMA,tname)
  { supers }

declaration:
  | GLOBAL name=vname COLON annotated_typ=annotated_typ
    { let typ = annotated_typ.Typ.typ in
      let attributes = annotated_typ.Typ.attributes in
      let global : Global.t = {name; typ; attributes} in
      Global global }
  | TYPE typ_name=tname supers=extends? ioption(EQ) attributes=annots
         LBRACKET l=separated_list(SEMICOLON, typed_field) RBRACKET
    { let fields =
        List.map l ~f:(fun (name, annotated_typ) ->
                        let enclosing_class = typ_name in
                        let qualified_name : qualified_fieldname = {name; enclosing_class} in
                        let typ = annotated_typ.Typ.typ in
                        let attributes = annotated_typ.Typ.attributes in
                        {FieldDecl.qualified_name; typ; attributes}) in
      let supers = Option.value supers ~default:[] in
      Struct {name= typ_name; supers; fields; attributes} }
  | DECLARE attributes=annots qualified_name=qualified_pname LPAREN
            formals_types = separated_list(COMMA, annotated_typ)
            RPAREN COLON result_type=annotated_typ
    { let procdecl : ProcDecl.t =
        {qualified_name; formals_types= Some formals_types; result_type; attributes} in
      Procdecl procdecl
    }
  | DECLARE attributes=annots qualified_name=qualified_pname
            LPAREN ELLIPSIS RPAREN COLON result_type=annotated_typ
/* Declarations with an ellipsis is a temporary syntax to support declarations of external functions
in Hack where formals number and types are unknown. */
    { let procdecl : ProcDecl.t =
        {qualified_name; formals_types= None; result_type; attributes} in
      Procdecl procdecl
    }
  | DEFINE attributes=annots qualified_name=qualified_pname LPAREN
           params = separated_list(COMMA, typed_var) RPAREN COLON result_type=annotated_typ
           LBRACKET locals = locals nodes=block+ RBRACKET
    { let formals_types = List.map ~f:snd params in
      let procdecl : ProcDecl.t =
        {qualified_name; formals_types= Some formals_types; result_type; attributes} in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:fst params in
      let exit_loc = location_of_pos $endpos in
      Proc { procdecl; nodes; start= start_node.Node.label; params; locals; exit_loc}
    }

locals:
  | { [] }
  | LOCALKEYWORD locals=separated_nonempty_list(COMMA, typed_var)
    { locals }

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

%inline
annots:
  | l=annot*
    { l }

annot:
  | DOT name=ident values=annot_value
    { let loc = location_of_pos $startpos(name) in
      {name; values; loc} }

%inline
annot_value:
  |
    { [] }
  | EQ l = separated_nonempty_list(COMMA, STRING)
    { l }


annotated_typ:
  | annots=annots typ=typ
    { {attributes=annots; typ} }

typed_field:
  | name=fname COLON annotated_typ=annotated_typ
    { (name, annotated_typ) }

typed_var:
  | name=vname COLON annotated_typ=annotated_typ
    { (name, annotated_typ) }

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
  | exp=expression DOT enclosing_class=tname DOT name=fname
    { let field : qualified_fieldname = {enclosing_class; name} in
      Field {exp; field} }
  | e1=expression LSBRACKET e2=expression RSBRACKET
    { Index (e1, e2) }
  | c=const
    { Const c }
  | proc=qualified_pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { Call {proc; args; kind= Exp.NonVirtual} }
  | recv=expression DOT proc=qualified_pname LPAREN args=separated_list(COMMA, expression) RPAREN
    { Exp.call_virtual proc recv args }
  | LABRACKET typ=typ RABRACKET
    { Typ typ }
