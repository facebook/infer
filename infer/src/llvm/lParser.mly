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
%token TARGET
%token DATALAYOUT
%token TRIPLE
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
%token COLON
(* symbols *)
%token EQUALS
%token STAR
%token X

(* TYPES *)
%token VOID
%token <int> INT
%token HALF
%token FLOAT
%token DOUBLE
%token FP128
%token X86_FP80
%token PPC_FP128
(*%token X86_MMX*)
%token LABEL
%token METADATA

%token <string> CONSTANT_STRING
(* CONSTANTS *)
%token <int> CONSTANT_INT
%token NULL

(* INSTRUCTIONS *)
(* terminator instructions *)
%token RET
%token BR
(*%token SWITCH*)
(*%token INDIRECTBR*)
(*%token INVOKE*)
(*%token RESUME*)
(*%token UNREACHABLE*)
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
(*%token SHUFFLEVECTOR*)
(* aggregate operations *)
(*%token EXTRACTVALUE*)
(*%token INSERTVALUE*)
(* memory access and addressing operations *)
%token ALIGN (* argument for below operations *)
%token ALLOCA
%token LOAD
%token STORE
(*%token FENCE*)
(*%token CMPXCHG*)
(*%token ATOMICRMW*)
(*%token GETELEMENTPTR*)
(* conversion operations *)
(*%token TRUNC*)
(*%token ZEXT*)
(*%token SEXT*)
(*%token FPTRUNC*)
(*%token FPEXT*)
(*%token FPTOUI*)
(*%token FPTOSI*)
(*%token UITOFP*)
(*%token SITOFP*)
(*%token PTRTOINT*)
(*%token INTTOPTR*)
(*%token BITCAST*)
(*%token ADDRSPACECAST*)
(*%token TO*)
(* other operations *)
(*%token ICMP*)
(*%token FCMP*)
(*%token PHI*)
(*%token SELECT*)
%token CALL
(*%token VA_ARG*)
(*%token LANDINGPAD*)

%token DBG_DECLARE
%token <string> NAMED_GLOBAL
%token <string> NAMED_LOCAL
%token <int> NUMBERED_GLOBAL
%token <int> NUMBERED_LOCAL
%token <string> IDENT

%token DEBUG_ANNOTATION
%token <string> NAMED_METADATA
%token <int> NUMBERED_METADATA
%token <string> METADATA_STRING
%token METADATA_NODE_BEGIN
%token METADATA_LOCATION

%token <int> ATTRIBUTE_GROUP

%token EOF

%start program
%type <LAst.program> program
%type <LAst.function_def> function_def
%type <LAst.typ option> ret_typ
%type <LAst.typ> typ

%%

program:
  | targets func_defs = function_def* opt_mappings = metadata_def* EOF {
      let mappings = IList.flatten_options opt_mappings in
      let add_mapping map (metadata_id, aggregate) = MetadataMap.add metadata_id aggregate map in
      let metadata_map = IList.fold_left add_mapping MetadataMap.empty mappings in
      Program (func_defs, metadata_map) }

targets:
  | { (None, None) }
  | dl = datalayout { (Some dl, None) }
  | tt = target_triple { (None, Some tt) }
  | dl = datalayout tt = target_triple { (Some dl, Some tt) }
  | tt = target_triple dl = datalayout { (Some dl, Some tt) }

datalayout:
  | TARGET DATALAYOUT EQUALS str = CONSTANT_STRING { str }

target_triple:
  | TARGET TRIPLE EQUALS str = CONSTANT_STRING { str }

metadata_def:
  | NAMED_METADATA EQUALS numbered_metadata_node { None }
  | metadata_id = NUMBERED_METADATA EQUALS aggregate = metadata_aggregate { Some
  (metadata_id, aggregate) }

numbered_metadata_node:
  | METADATA_NODE_BEGIN metadata_ids = separated_list(COMMA, NUMBERED_METADATA) RBRACE { metadata_ids }

metadata_aggregate:
  | components = metadata_node { Components components }
  | location = metadata_location { Location location }

metadata_location:
  | METADATA_LOCATION LPAREN IDENT COLON line_num = CONSTANT_INT COMMA
    IDENT COLON col_num = CONSTANT_INT COMMA
    IDENT COLON i = NUMBERED_METADATA RPAREN
    { { line = line_num; col = col_num; scope = MetadataVar i} }

metadata_node:
  | METADATA? METADATA_NODE_BEGIN components = separated_list(COMMA, metadata_component) RBRACE { components }

metadata_component:
  | METADATA? tp = typ? op = operand { TypOperand (tp, op) }
  | METADATA? value = metadata_value { MetadataVal value }

metadata_value:
  | i = NUMBERED_METADATA { MetadataVar i }
  | str = METADATA_STRING { MetadataString str }
  | components = metadata_node { MetadataNode components }

function_def:
  | DEFINE ret_tp = ret_typ name = variable LPAREN
    params = separated_list(COMMA, pair(first_class_typ, IDENT)) RPAREN attribute_group*
    annotated_instrs = block { FunctionDef (name, ret_tp, params, annotated_instrs) }

attribute_group:
  | i = ATTRIBUTE_GROUP { i }

ret_typ:
  | VOID { None }
  | tp = first_class_typ { Some tp }

typ:
  | tp = func_typ { tp }
  | tp = first_class_typ { tp }

func_typ:
  | ret_tp = ret_typ LPAREN param_tps = separated_list(COMMA, first_class_typ) RPAREN { Tfunc (ret_tp, param_tps) }

first_class_typ:
  | tp = element_typ { tp }
  (*| X86_MMX { () }*)
  | tp = vector_typ { tp }
  | LSQBRACK sz = CONSTANT_INT X tp = element_typ RSQBRACK { Tarray (sz, tp) } (* array type *)
  | LABEL { Tlabel }
  | METADATA { Tmetadata }
  (* TODO structs *)

vector_typ:
  | LANGLE sz = CONSTANT_INT X tp = element_typ RANGLE { Tvector (sz, tp) }

element_typ:
  | width = INT { Tint width }
  | floating_typ { Tfloat }
  | tp = ptr_typ { Tptr tp }

floating_typ:
  | HALF { () }
  | FLOAT { () }
  | DOUBLE { () }
  | FP128 { () }
  | X86_FP80 { () }
  | PPC_FP128 { () }

ptr_typ:
  | tp = typ STAR { tp }

block:
  | LBRACE annotated_instrs = annotated_instruction* RBRACE { IList.flatten_options annotated_instrs }

annotated_instruction:
  | instr = real_instruction anno = annotation? { Some (instr, anno) }
  | debug_instruction annotation? { None }

annotation:
  | COMMA DEBUG_ANNOTATION i = NUMBERED_METADATA { Annotation i }

real_instruction:
  (* terminator instructions *)
  | RET tp = typ op = operand { Ret (Some (tp, op)) }
  | RET VOID { Ret None }
  | BR LABEL lbl = variable { UncondBranch lbl }
  | BR _ = INT op = operand COMMA LABEL lbl1 = variable COMMA LABEL lbl2 = variable
      { CondBranch (op, lbl1, lbl2) }
  (* Memory access operations *)
  | var = variable EQUALS ALLOCA tp = typ align? { Alloc (var, tp, 1) }
  | var = variable EQUALS LOAD tp = ptr_typ ptr = variable align? { Load (var, tp, ptr) }
  | STORE val_tp = typ value = operand COMMA _ptr_tp = ptr_typ var = variable align?
      { Store (value, val_tp, var) }
    (* don't yet know why val_tp and _ptr_tp would be different *)
  (* Function call *)
  | ret_var = variable EQUALS CALL ret_typ func_var = variable LPAREN
    args = separated_list(COMMA, pair(typ, operand)) RPAREN { Call (ret_var, func_var, args) }
  | variable EQUALS binop { Binop }

debug_instruction:
  | CALL VOID DBG_DECLARE LPAREN separated_list(COMMA, metadata_component) RPAREN { () }

align:
  | COMMA ALIGN width = CONSTANT_INT { width }

binop:
  | ADD arith_options binop_args { () }
  | FADD fast_math_flags binop_args { () }
  | SUB arith_options binop_args { () }
  | FSUB fast_math_flags binop_args { () }
  | MUL binop_args { () }
  | FMUL fast_math_flags binop_args { () }
  | UDIV EXACT? binop_args { () }
  | SDIV EXACT? binop_args { () }
  | FDIV fast_math_flags binop_args { () }
  | UREM binop_args { () }
  | SREM binop_args { () }
  | FREM fast_math_flags binop_args { () }
  (* bitwise *)
  | SHL arith_options binop_args { () }
  | LSHR EXACT? binop_args { () }
  | ASHR EXACT? binop_args { () }
  | AND binop_args { () }
  | OR binop_args { () }
  | XOR binop_args { () }
  (* vector *)
  | EXTRACTELEMENT vector_typ operand COMMA typ operand { () }
  | INSERTELEMENT vector_typ operand COMMA typ operand COMMA typ operand { () }

arith_options:
  | NUW? NSW? { () }

fast_math_flags:
  | NNAN? NINF? NSZ? ARCP? FAST? { () }

binop_args:
  | typ operand COMMA operand { () }

operand:
  | var = variable { Var var }
  | const = constant { Const const }

variable:
  | name = NAMED_GLOBAL { Global (Name name) }
  | name = NAMED_LOCAL { Local (Name name) }
  | num = NUMBERED_GLOBAL { Global (Number num) }
  | num = NUMBERED_LOCAL { Local (Number num) }

constant:
  | i = CONSTANT_INT { Cint i }
  | NULL { Cnull }
