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

%token AND
%token AMPERSAND
%token ARROW
%token ASSIGN
%token COLON
%token COMMA
%token DECLARE
%token DEFINE
%token DOT
%token ELLIPSIS
%token ELSE
%token EOF
%token EQ
%token EXTENDS
%token EQUALS
%token FALSE
%token FLOAT
%token FUN
%token GLOBAL
%token HANDLERS
%token IF
%token IF_AND_LPAREN
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
%token OR
%token PRUNE
%token QUESTION
%token RABRACKET
%token RBRACKET
%token RET
%token RPAREN
%token RSBRACKET
%token SEMICOLON
%token STAR
%token STORE
%token THEN
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
%token <string option * string> PROC_AND_LPAREN
%token <string> STRING

%right OR
%right AND

%start <Textual.SourceFile.t -> Textual.Module.t> main
%type <Attr.t> attribute
%type <Module.decl> declaration
%type <QualifiedProcName.t> qualified_pname_and_lparen
%type <QualifiedProcName.t> opt_qualified_pname_and_lparen
%type <FieldName.t> fname
%type <NodeName.t> nname
%type <TypeName.t> tname
%type <TypeName.t> opt_tname
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
%type <BoolExp.t> bool_expression
%type <Const.t> const
%type <NodeName.t list> opt_handlers
%type <Node.t> block
%type <Terminator.node_call> node_call
%type <NodeName.t * (Ident.t * Typ.t) list> label
%type <TypeName.t list> extends
%type <Ident.t * Typ.t> typed_ident
%type <Textual.Body.t> body
%%

basic_ident:
  | DECLARE { "declare" }
  | DEFINE { "define" }
  | EXTENDS { "extends" }
  | EQUALS { "equals" }
  | GLOBAL { "global" }
  | JMP { "jmp" }
  | LOCALKEYWORD { "local" }
  | PRUNE { "prune" }
  | RET { "ret" }
  | STORE { "store" }
  | THROW { "throw" }
  | TYPE { "type" }
  | UNREACHABLE { "unreachable" }

ident_except_load:
  | x=basic_ident { x }
  | x=IDENT { x }

ident:
  | LOAD { "load" }
  | x=ident_except_load { x }

main:
  | attrs=attribute* decls=declaration* EOF
    { (fun sourcefile -> { Module.attrs; decls; sourcefile }) }

name_and_lparen:
  | proc_ident=opt_qualified_pname_and_lparen
    { match (proc_ident : QualifiedProcName.t).enclosing_class with
      | Enclosing _ ->
         let loc = location_of_pos $startpos(proc_ident) in
         let string = Format.asprintf "%a" QualifiedProcName.pp proc_ident in
         raise (SpecialSyntaxError (loc, string))
      | _ -> (proc_ident : QualifiedProcName.t).name.value }

fname:
  | id=ident
    { { FieldName.value=id; loc=location_of_pos $startpos(id) } }

nname:
  | id=ident
    { { NodeName.value=id; loc=location_of_pos $startpos(id) } }

tname:
  | id=ident
    { { TypeName.value=id; loc=location_of_pos $startpos(id) } }

tname_or_void:
  | tname=tname
    { tname }
  | VOID
    { { TypeName.value="void"; loc=location_of_pos $startpos } }

opt_tname:
  | tname=tname
    { tname }
  | QUESTION
    { { TypeName.value="?"; loc=location_of_pos $startpos } }

vname:
  | id=ident
    { { VarName.value=id; loc=location_of_pos $startpos(id) } }

vname_except_load_keyword:
  | id=ident_except_load
    { { VarName.value=id; loc=location_of_pos $startpos(id) } }

opt_qualified_pname_and_lparen:
  | proc_ident=PROC_AND_LPAREN
    { let enclosing, id = proc_ident in
      let loc = location_of_pos $startpos(proc_ident) in
      let name : ProcName.t = { value=id; loc } in
      let enclosing_class =
        Option.value_map enclosing
                         ~default:QualifiedProcName.TopLevel
                         ~f:(fun value -> QualifiedProcName.Enclosing {value; loc})
      in
      ( {enclosing_class; name} : QualifiedProcName.t)
    }

qualified_pname_and_lparen:
  | proc_ident=opt_qualified_pname_and_lparen
    { match (proc_ident : QualifiedProcName.t).enclosing_class with
      | Enclosing tname when String.equal tname.TypeName.value "?" ->
         let loc = location_of_pos $startpos(proc_ident) in
         let string = Format.asprintf "%a" QualifiedProcName.pp proc_ident in
         raise (SpecialSyntaxError (loc, string))
      | _ -> proc_ident }

attribute:
  | DOT name=ident EQ value=STRING
    { {Attr.name=name; values=[value]; loc=location_of_pos $startpos} }

extends:
  | EXTENDS supers=separated_nonempty_list(COMMA,tname)
  { supers }

typedef:
  | union_list=separated_nonempty_list(COMMA,tname_or_void)
  { union_list }

declaration:
  | GLOBAL name=vname COLON annotated_typ=annotated_typ
    { let typ = annotated_typ.Typ.typ in
      let attributes = annotated_typ.Typ.attributes in
      let global : Global.t = {name; typ; attributes} in
      Module.Global global }
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
      Module.Struct {name= typ_name; supers; fields; attributes} }
   | TYPE typ_name=tname EQUALS typ_definition=typedef alias_attributes=annots
    { let loc = location_of_pos $startpos(typ_name) in
      let kind_attribute  = {Attr.name="kind"; values=["typedef"]; loc} in
      let attributes =  kind_attribute :: alias_attributes in
    Module.Struct {name= typ_name; supers=typ_definition; fields=[]; attributes}}
  | DECLARE attributes=annots qualified_name=qualified_pname_and_lparen
            formals_types=declaration_types
            RPAREN COLON result_type=annotated_typ
    { let procdecl : ProcDecl.t =
        {qualified_name; formals_types; result_type; attributes} in
      Module.Procdecl procdecl
    }
  | DEFINE attributes=annots qualified_name=qualified_pname_and_lparen
           params = separated_list(COMMA, typed_var) RPAREN COLON result_type=annotated_typ
           body = body
    { let formals_types = List.map ~f:snd params in
      let procdecl : ProcDecl.t =
        {qualified_name; formals_types= Some formals_types; result_type; attributes} in
      let body = if ProcDecl.is_curry_invoke procdecl then Body.dummy procdecl.qualified_name.name.loc
        (* the current HackC translation of these function raises type errors but we don't  need
           this body anyway (we will use a model), so we drop it here. We could remove this patch
           if HackC changes its translation in the future. *)
        else body in
      let {locals; nodes} : Body.t = body in
      let start_node = List.hd_exn nodes in
      let params = List.map ~f:fst params in
      let exit_loc = location_of_pos $endpos in
      Module.Proc { procdecl; nodes; start= start_node.Node.label; params; locals; exit_loc}
    }

declaration_types:
  | ELLIPSIS
    /* Declaration with an ellipsis is a special syntax to support declarations of external
       functions in Hack where the number and types of formals is unknown. */
    { None }
  | types = separated_list(COMMA, annotated_typ)
    { Some types }

body:
 | LBRACKET lcls = locals nds=block+ RBRACKET
      { Body.{ locals= lcls; nodes= nds }  }

locals:
  | { [] }
  | LOCALKEYWORD locals=separated_nonempty_list(COMMA, typed_var)
    { locals }

base_typ:
  | INT
    { Typ.Int }
  | FLOAT
    { Typ.Float }
  | VOID
    { Typ.Void }
  | name=tname
    { Typ.Struct name }
  | typ=base_typ LSBRACKET RSBRACKET
    { Typ.Array typ }
  | LPAREN typ=typ RPAREN
    { typ }

typ:
  | typ=base_typ
    { typ }
  | STAR typ=typ
    { Typ.Ptr typ }

%inline
annots:
  | l=annot*
    { l }

annot:
  | DOT name=ident values=annot_value
    { let loc = location_of_pos $startpos(name) in
      {Attr.name=name; values; loc} }

%inline
annot_value:
  |
    { [] }
  | EQ l = separated_nonempty_list(COMMA, STRING)
    { l }


annotated_typ:
  | annots=annots typ=typ
    { {Typ.attributes=annots; typ} }

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
      Node.{ label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc } }

label:
  | label=LABEL COLON
    { let label : NodeName.t = { value=label; loc=location_of_pos $startpos(label)} in
      label, [] }
  | label=LABEL LPAREN l=separated_nonempty_list(COMMA, typed_ident) RPAREN COLON
    { let label : NodeName.t = { value=label; loc=location_of_pos $startpos(label)} in
      label, l }

const:
  | INTEGER
    { Const.Int $1 }
  | STRING
    { Const.Str $1 }
  | FLOATINGPOINT
    { Const.Float $1 }
  | TRUE
    { Const.Int Z.one }
  | FALSE
    { Const.Int Z.zero }
  | NULL
    { Const.Null }

instruction:
  | id=LOCAL COLON typ=typ EQ LOAD exp=expression
    { Instr.Load {id= Ident.of_int id; exp; typ=Some typ; loc=location_of_pos $startpos } }
  | id=LOCAL EQ LOAD exp=expression
    { Instr.Load {id= Ident.of_int id; exp; typ=None; loc=location_of_pos $startpos } }
  | STORE exp1=expression ASSIGN exp2=expression COLON typ=typ
    { Instr.Store {exp1; exp2; typ=Some typ; loc=location_of_pos $startpos } }
  | STORE exp1=expression ASSIGN exp2=expression
    { Instr.Store {exp1; exp2; typ=None; loc=location_of_pos $startpos } }
  | PRUNE exp=expression
    { Instr.Prune {exp; loc=location_of_pos $startpos} }
  | PRUNE NOT exp=expression
    { Instr.Prune {exp=Exp.not exp; loc=location_of_pos $startpos} }
  | id=LOCAL EQ exp=expression
    { Instr.Let { id= Ident.of_int id; exp; loc=location_of_pos $startpos } }

bool_expression:
  | exp=expression
    { BoolExp.Exp exp }
  | bexp1=bool_expression AND bexp2=bool_expression
    { BoolExp.And (bexp1, bexp2) }
  | bexp1=bool_expression OR bexp2=bool_expression
    { BoolExp.Or (bexp1, bexp2) }
  | LPAREN bexp=bool_expression RPAREN
    { bexp }

lparen_bool_expression:
  | bexp1=lparen_bool_expression AND bexp2=bool_expression
    { BoolExp.And (bexp1, bexp2) }
  | bexp1=lparen_bool_expression OR bexp2=bool_expression
    { BoolExp.Or (bexp1, bexp2) }
  | bexp=bool_expression RPAREN
    { bexp }

terminator:
  | IF bexp=bool_expression THEN then_=terminator ELSE else_=terminator
    { Terminator.If {bexp; then_; else_} }
  | IF_AND_LPAREN bexp=lparen_bool_expression THEN then_=terminator ELSE else_=terminator
    { Terminator.If {bexp; then_; else_} }
  | RET e=expression
    { Terminator.Ret e }
  | JMP l=separated_list(COMMA, node_call)
    { Terminator.Jump l }
  | UNREACHABLE
    { Terminator.Unreachable }
  | THROW e=expression
    { Terminator.Throw e }

node_call:
  | label=nname
    { Terminator.{label; ssa_args=[]} }
  | value=name_and_lparen ssa_args=separated_nonempty_list(COMMA, expression) RPAREN
    { let label : NodeName.t = { value; loc=location_of_pos $startpos(value) } in
      Terminator.{label; ssa_args} }

opt_handlers:
  | { [] }
  | HANDLERS l=separated_list(COMMA, nname)
    { l }

expression:
  | id=LOCAL
    { Exp.Var (Ident.of_int id) }
  | LSBRACKET exp=expression COLON typ=typ RSBRACKET
    { Exp.Load {exp; typ=Some typ} }
  | LSBRACKET exp=expression RSBRACKET
    { Exp.Load {exp; typ=None} }
  | AMPERSAND name=vname
    { Exp.Lvar name }
  | vname=vname_except_load_keyword
    { Exp.Load {exp=Exp.Lvar vname; typ=None} }
  | exp=expression DOT enclosing_class=opt_tname DOT name=fname
    { let field : qualified_fieldname = {enclosing_class; name} in
      Exp.Field {exp; field} }
  | exp=expression ARROW enclosing_class=opt_tname DOT name=fname
    { let field : qualified_fieldname = {enclosing_class; name} in
      Exp.Load {exp=Exp.Field {exp; field}; typ=None} }
  | e1=expression LSBRACKET e2=expression RSBRACKET
    { Exp.Index (e1, e2) }
  | c=const
    { Exp.Const c }
  | proc=opt_qualified_pname_and_lparen args=separated_list(COMMA, expression) RPAREN
    { Exp.Call {proc; args; kind= Exp.NonVirtual} }
  | closure=expression LPAREN args=separated_list(COMMA, expression) RPAREN
    (* remark: the lexer will never generate the sequence IDENT LPAREN because PROC_AND_LPAREN
       is more prioritary. We will fix that using TextualTransform.FixClosureAppExpr later *)
    { Exp.Apply {closure; args} }
  | recv=expression DOT proc=opt_qualified_pname_and_lparen args=separated_list(COMMA, expression) RPAREN
    { Exp.call_virtual proc recv args }
  | FUN params=separated_list(COMMA, vname) RPAREN ARROW
     proc=opt_qualified_pname_and_lparen args=separated_list(COMMA, expression) RPAREN
    {
      let syntax_error () =
        let loc = location_of_pos $startpos(params) in
        let string = F.asprintf "call inside closure should end with %a"
                                (Pp.seq ~sep:"," VarName.pp) params in
        raise (SpecialSyntaxError (loc, string))
      in
      let nb_params = List.length params in
      let nb_args = List.length args in
      if nb_params > nb_args then syntax_error ()
      else
        let captured, params2 = List.split_n args (nb_args - nb_params) in
        let match_param (e: Exp.t) (x: VarName.t) =
          match e with
            | Exp.Load {exp=Lvar varname; typ=None} -> VarName.equal varname x
            | _ -> false
        in
        if List.for_all2_exn ~f:match_param params2 params then
          Exp.Closure {proc; captured; params}
        else syntax_error ()
     }
  | LABRACKET typ=typ RABRACKET
    { Exp.Typ typ }
