(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
{
  open Lexing
  open LParser

  exception LexingError of string
}

let space = [' ' '\t']
let newline = '\n'
let comment = ';' [^ '\n']*

let nonzero_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_int = nonzero_digit digit*
let nonneg_int = '0' | pos_int
let intlit = '-'? digit+

let lower = ['a'-'z']
let upper = ['A'-'Z']
let id_special_char = ['-' '$' '.' '_']
let id_char = lower | upper | id_special_char
let id = id_char (id_char | digit)*

(* definition of attribute group - not used for now *)
let attribute_junk = "attributes" [^ '\n']*

rule token = parse
  | space | comment { token lexbuf }
  | newline { token lexbuf }

  (* keywords *)
  | "define" { DEFINE }

  (* delimiters *)
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '<' { LANGLE }
  | '>' { RANGLE }
  | '[' { LSQBRACK }
  | ']' { RSQBRACK }
  (* symbols *)
  | '=' { EQUALS }
  | '*' { STAR }
  | ['x' 'X'] { X }

  (* TYPES *)
  | "void" { VOID }
  | "i1" { BIT (* INT 1 *) }
  | 'i' (pos_int as width) { INT (int_of_string width) }
  | "half" { HALF }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "fp128" { FP128 }
  | "x86_fp80" { X86_FP80 }
  | "ppc_fp128" { PPC_FP128 }
  (*| "x86_mmx" { X86_MMX }*)
  | "label" { LABEL }
  | "metadata" { METADATA }

  | pos_int as size { CONSTANT_INT (int_of_string size) }
  (* CONSTANTS *)
  | "true" { CONSTANT_INT 1 }
  | "false" { CONSTANT_INT 0 }
  | intlit as i { CONSTANT_INT (int_of_string i) }
  (* floating point constants *)
  | "null" { NULL }

  (* INSTRUCTIONS *)
  (* terminator instructions *)
  | "ret" { RET }
  | "br" { BR }
  (*| "switch" { SWITCH }
  | "indirectbr" { INDIRECTBR }
  | "invoke" { INVOKE }
  | "resume" { RESUME }
  | "unreachable" { UNREACHABLE }*)
  (* binary operations *)
  | "add" { ADD }
  | "fadd" { FADD }
  | "sub" { SUB }
  | "fsub" { FSUB }
  | "mul" { MUL }
  | "fmul" { FMUL }
  | "udiv" { UDIV }
  | "sdiv" { SDIV }
  | "fdiv" { FDIV }
  | "urem" { UREM }
  | "srem" { SREM }
  | "frem" { FREM }
  (* arithmetic options *)
  | "nuw" { NUW }
  | "nsw" { NSW }
  | "exact" { EXACT }
  (* floating point options *)
  | "nnan" { NNAN }
  | "ninf" { NINF }
  | "nsz" { NSZ }
  | "arcp" { ARCP }
  | "fast" { FAST }
  (* bitwise binary operations *)
  | "shl" { SHL }
  | "lshr" { LSHR }
  | "ashr" { ASHR }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  (* vector operations *)
  | "extractelement" { EXTRACTELEMENT }
  | "insertelement" { INSERTELEMENT }
  (*| "shufflevector" { SHUFFLEVECTOR }*)
  (* aggregate operations *)
  (*| "extractvalue" { EXTRACTVALUE }*)
  (*| "insertvalue" { INSERTVALUE }*)
  (* memory access and addressing operations *)
  | "alloca"  { ALLOCA }
  | "load" { LOAD }
  | "store" { STORE }
  (*| "fence" { FENCE }*)
  (*| "cmpxchg" { CMPXCHG }*)
  (*| "atomicrmw" { ATOMICRMW }*)
  (*| "getelementptr" { GETELEMENTPTR }*)
  (* conversion operations *)
  (*| "trunc" { TRUNC }*) (* e.g. trunc ... to ... *)
  (*| "zext" { ZEXT }*)
  (*| "sext" { SEXT }*)
  (*| "fptrunc" { FPTRUNC }*)
  (*| "fpext" { FPEXT }*)
  (*| "fptoui" { FPTOUI }*)
  (*| "fptosi" { FPTOSI }*)
  (*| "uitofp" { UITOFP }*)
  (*| "sitofp" { SITOFP }*)
  (*| "ptrtoint" { PTRTOINT }*)
  (*| "inttoptr" { INTTOPTR }*)
  (*| "bitcast" { BITCAST }*)
  (*| "addrspacecast" { ADDRSPACECAST }*)
  (*| "to" { TO }*) (* all conversion operations include this keyword *)
  (* other operations *)
  (*| "icmp" { ICMP }*)
  (*| "fcmp" { FCMP }*)
  (*| "phi" { PHI }*)
  (*| "select" { SELECT }*)
  (*| "call" { CALL }*)
  (*| "va_arg" { VA_ARG }*)
  (*| "landingpad" { LANDINGPAD }*)

  (* IDENTIFIERS *)
  | '@' (id as str) { NAMED_GLOBAL str }
  | '%' (id as str) { NAMED_LOCAL str }
  | '@' (nonneg_int as i) { NUMBERED_GLOBAL (int_of_string i) }
  | '%' (nonneg_int as i) { NUMBERED_LOCAL (int_of_string i) }
  | id as str { IDENT str }

  (* attribute groups *)
  | '#' (nonneg_int as i) { ATTRIBUTE_GROUP (int_of_string i) }
  | attribute_junk { token lexbuf }

  | eof { EOF }
