/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
%{

    open! IStd
    open Textual
    open DoliAst
    open DoliJavaAst
    open DoliObjCAst

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

(* Doli-specific keywords *)
%token RULE
%token IN
%token JAVA
%token OBJC
%token MATCH
%token BODYKW
%token UNDER
(* generics *)
%token QUESTION
%token SUPER
(* modifiers and throws *)
%token PUBLIC
%token PROTECTED
%token PRIVATE
%token STATIC
%token ABSTRACT
%token FINAL
%token NATIVE
%token THROWS
(* basic types *)
%token BYTE
%token SHORT
%token CHAR
%token LONG
%token DOUBLE
%token BOOLEAN
(* placeholders for the bodies- will be removed in future work *)
%token OBJCSIGNSTUB


%start <Textual.SourceFile.t -> Textual.Module.t> main
%start <DoliAst.doliProgram> doliProgram
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
%type <Textual.Body.t> body
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
  | PUBLIC { "public" }
  | FINAL { "final" }
  | PROTECTED { "protected" }
  | PRIVATE { "private" }
  | STATIC { "static" }
  | IN{ "in" }
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
           body = body
    { let formals_types = List.map ~f:snd params in
      let procdecl : ProcDecl.t =
        {qualified_name; formals_types= Some formals_types; result_type; attributes} in
      let {locals; nodes} : Body.t = body in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:fst params in
      let exit_loc = location_of_pos $endpos in
      Proc { procdecl; nodes; start= start_node.Node.label; params; locals; exit_loc}
    }

body:
 | LBRACKET lcls = locals nds=block+ RBRACKET
      { { locals= lcls; nodes= nds }  }

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


(*  -------------------- DOLI  ----------------------------------*)

doliProgram:
 | doliRules = doliRule* EOF { DoliProgram doliRules }
 ;

 doliRule:
  | RULE ruleId = ident IN JAVA jm = javaMatch bd= doliBody
  { { ruleName = ruleId;  match_ = jm; body = bd }  }
  | RULE ruleId = ident  IN OBJC ocm = objCMatch bd= doliBody
  { { ruleName = ruleId; match_ = ocm; body = bd }  }
  ;

doliBody:
  | BODYKW; bd = body
     { bd }
     ;

javaMatch: MATCH;
  LBRACKET; ess=list(extendedSignature); RBRACKET;
    { JavaMatching ess}
  ;

extendedSignature:
  | sigs=separated_nonempty_list(SEMICOLON,signature); UNDER; rt=referenceType
    (* the SEMICOLON separator is needed in order to avoid shift-reduce conflicts *)
    { {signs=sigs; under=rt}  }
  ;

signature:
  | mds=modifier*; rt=returnType; funcId=ident;
  LPAREN fPTs=separated_list(COMMA, formalParameterType) RPAREN
    option(throws)
   { {modifiers = mds; returns = rt; identifier = funcId; formParTypes = fPTs } }
  ;

modifier:
  | PUBLIC { Public }
  | PROTECTED { Protected }
  | PRIVATE { Private }
  | STATIC { Static }
  | FINAL { Final }
  | ABSTRACT { Abstract }
  | NATIVE { Native }
  ;

returnType:
  | VOID { VoidType }
  | nvt=nonVoidType { NonVoid nvt }
  ;

referenceType:
  | cts=separated_nonempty_list(DOT, classType) { RT cts }
   ;

classType:
  | cId=ident { CT (cId, []) }
  | cId=ident LABRACKET tArgs=separated_list(COMMA, typeArgument)  RABRACKET { CT (cId,tArgs) }
  ;


nonVoidType:
(* QUESTION: Do we want to "flatten" tree in the future?,
   ie avoid having one constructor appied to an other*)
  | BYTE { BasicType ByteType }
  | INT { BasicType IntType }
  | BOOLEAN { BasicType BoolType }
  | CHAR { BasicType CharType }
  | DOUBLE { BasicType DoubleType }
  | FLOAT { BasicType FloatType }
  | LONG { BasicType LongType }
  | SHORT { BasicType ShortType }
  | rt=referenceType { RefType rt }
  | nvt=nonVoidType; LSBRACKET RSBRACKET { Array nvt }
  ;

formalParameterType:
   | nvt=nonVoidType; ident { nvt }
   ;


typeArgument:
  | rt=referenceType { PLAIN rt }
  | QUESTION SUPER; rt=referenceType { SUPER rt }
  | QUESTION EXTENDS rt=referenceType { EXTENDS rt }
  ;


throws:
  | THROWS  separated_nonempty_list(COMMA, referenceType){ }
  ;


(* ObjC matching *)

objCMatch:
    MATCH; LBRACKET; ess=extendedSignatureList; RBRACKET;
      { ObjCMatching(ess) }
;

extendedSignatureList:
  |  OBJCSIGNSTUB;  {  [ { stub = 33 } ] }
