(*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)
%{
  open LAst
%}

(* keywords *)
%token DEFINE

(* delimiters *)
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token LSQBRACK
%token RSQBRACK
(* symbols *)
%token EQUALS
%token STAR
%token X
%token AT
%token PERCENT

(* TYPES *)
%token VOID
%token BIT (* i1 *)
%token <int> INT
%token HALF
%token FLOAT
%token DOUBLE
%token FP128
%token X86_FP80
%token PPC_FP128
%token X86_MMX
%token LABEL
%token METADATA

%token <int> SIZE
(* CONSTANTS *)
%token <int> CONSTINT
%token NULL

(* INSTRUCTIONS *)
(* terminator instructions *)
%token RET
%token BR
%token SWITCH
%token INDIRECTBR
%token INVOKE
%token RESUME
%token UNREACHABLE
(* binary operations *)
%token ADD
%token FADD
%token SUB
%token FSUB
%token MUL
%token FMUL
%token UDIV
%token SDIV
%token FDIV
%token UREM
%token SREM
%token FREM
(* arithmetic options *)
%token NUW
%token NSW
%token EXACT
(* floating point options *)
%token NNAN
%token NINF
%token NSZ
%token ARCP
%token FAST
(* bitwise binary operations *)
%token SHL
%token LSHR
%token ASHR
%token AND
%token OR
%token XOR
(* vector operations *)
%token EXTRACTELEMENT
%token INSERTELEMENT
%token SHUFFLEVECTOR
(* aggregate operations *)
%token EXTRACTVALUE
%token INSERTVALUE
(* memory access and addressing operations *)
%token ALLOCA
%token LOAD
%token STORE
%token FENCE
%token CMPXCHG
%token ATOMICRMW
%token GETELEMENTPTR
(* conversion operations *)
%token TRUNC
%token ZEXT
%token SEXT
%token FPTRUNC
%token FPEXT
%token FPTOUI
%token FPTOSI
%token UITOFP
%token SITOFP
%token PTRTOINT
%token INTTOPTR
%token BITCAST
%token ADDRSPACECAST
%token TO
(* other operations *)
%token ICMP
%token FCMP
%token PHI
%token SELECT
%token CALL
%token VA_ARG
%token LANDINGPAD

%token <string> IDENT

%token EOF

%start prog
%type <LAst.prog> prog
%type <LAst.func_def> func_def
%type <LAst.typ option> ret_typ
%type <LAst.typ> typ

%%

prog:
  | defs=list(func_def) EOF { Prog defs }

func_def:
  | DEFINE ret_tp=ret_typ name=variable LPAREN params=separated_list(COMMA, pair(typ, IDENT)) RPAREN instrs=block {
    FuncDef (name, ret_tp, params, instrs) }

ret_typ:
  | VOID { None }
  | tp=typ { Some tp }

typ:
  | tp=element_typ { tp }
  (*| X86_MMX { () }*)
  | tp=vector_typ { tp }
  | LSQBRACK sz=SIZE X tp=element_typ RSQBRACK { Tarray (sz, tp) } (* array type *)
  | LABEL { Tlabel }
  | METADATA { Tmetadata }
  (* TODO structs *)

vector_typ:
  | LANGLE sz=SIZE X tp=element_typ RANGLE { Tvector (sz, tp) }

element_typ:
  | width=INT { Tint width }
  | floating_typ { Tfloat }
  | tp=ptr_typ { tp }

floating_typ:
  | HALF { () }
  | FLOAT { () }
  | DOUBLE { () }
  | FP128 { () }
  | X86_FP80 { () }
  | PPC_FP128 { () }

ptr_typ:
  | tp=typ STAR { Tptr tp }

block:
  | LBRACE instrs=list(instr) RBRACE { instrs }

instr:
  | term=terminator { term }
  | variable EQUALS binop { Ret None } (* TODO *)

terminator:
  | RET tp=typ op=operand { Ret (Some (tp, op)) }
  | RET VOID { Ret None }
  | BR LABEL lbl=variable { UncondBranch lbl }
  | BR BIT op=operand COMMA LABEL lbl1=variable COMMA LABEL lbl2=variable { CondBranch (op, lbl1, lbl2) }
  (*
  | switch
  | indirectbr
  | invoke
  | resume
  | unreachable
  *)

binop:
  | ADD arith_options binop_args { () }
  | FADD fast_math_flags binop_args { () }
  | SUB arith_options binop_args { () }
  | FSUB fast_math_flags binop_args { () }
  | MUL binop_args { () }
  | FMUL fast_math_flags binop_args { () }
  | UDIV option(EXACT) binop_args { () }
  | SDIV option(EXACT) binop_args { () }
  | FDIV fast_math_flags binop_args { () }
  | UREM binop_args { () }
  | SREM binop_args { () }
  | FREM fast_math_flags binop_args { () }
  (* bitwise *)
  | SHL arith_options binop_args { () }
  | LSHR option(EXACT) binop_args { () }
  | ASHR option(EXACT) binop_args { () }
  | AND binop_args { () }
  | OR binop_args { () }
  | XOR binop_args { () }
  (* vector *)
  | EXTRACTELEMENT vector_typ operand COMMA typ operand { () }
  | INSERTELEMENT vector_typ operand COMMA typ operand COMMA typ operand { () }

arith_options:
  | option(NUW) option(NSW) { () }

fast_math_flags:
  | option(NNAN) option(NINF) option(NSZ) option(ARCP) option(FAST) { () }

binop_args:
  | typ operand COMMA operand { () }

(* below is fuzzy *)

operand:
  | var=variable { Var var }
  | const=constant { Const const }

variable:
  | AT id=IDENT { Global id }
  | PERCENT id=IDENT { Local id }

constant:
  | i=CONSTINT { Cint i }
  | NULL { Cnull }
