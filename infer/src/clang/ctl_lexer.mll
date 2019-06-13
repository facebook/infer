(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{
  open! IStd

  open Lexing
  open Ctl_parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1;
                pos_cnum = 1;
      }
}

let comment = "//" [^'\n']*
let whitespace = [' ' '\t']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ':']*
let file_id = ['a'-'z' 'A'-'Z' '_' '~' '/' '.'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ':' '.' '/' '-']*

rule token = parse
  | whitespace+ { token lexbuf }
  | whitespace*("\r")?"\n" { next_line lexbuf; token lexbuf }
  | comment { token lexbuf }
  | "HOLDS-UNTIL" { EU }
  | "HOLDS-EVERYWHERE-UNTIL" { AU }
  | "HOLDS-EVENTUALLY" { EF }
  | "HOLDS-EVERYWHERE-EVENTUALLY" { AF }
  | "HOLDS-NEXT" { EX }
  | "HOLDS-EVERYWHERE-NEXT" { AX }
  | "HOLDS-ALWAYS" { EG }
  | "HOLDS-EVERYWHERE-ALWAYS" { AG }
  | "HOLDS-IN-SOME-SUPERCLASS-OF" { EH }
  | "IN-NODE" { ET }
  | "HOLDS-IN-OBJC-CLASS" { HOLDS_IN_OBJCCLASS }
  | "INTERFACE" { INTERFACE }
  | "IMPLEMENTATION" { IMPLEMENTATION }
  | "WHEN" { WHEN }
  | "HOLDS-IN-NODE" { HOLDS_IN_NODE }
  | "WITH-TRANSITION" {WITH_TRANSITION}
  | "DEFINE-CHECKER" { DEFINE_CHECKER }
  | "GLOBAL-MACROS" { GLOBAL_MACROS }
  | "GLOBAL-PATHS" { GLOBAL_PATHS }
  | "#IMPORT" { HASHIMPORT }
  | "SET" { SET }
  | "LET" { LET }
  | "TRUE" { TRUE }
  | "FALSE" { FALSE }
  | "whitelist_path" { WHITELIST_PATH }
  | "blacklist_path" { BLACKLIST_PATH }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "=" { ASSIGNMENT }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "[" { LEFT_SQBRACE }
  | "]" { RIGHT_SQBRACE }
  | "AND" { AND }
  | "OR" { OR }
  | "NOT" { NOT }
  | "IMPLIES" { IMPLIES }
  | "REGEXP" { REGEXP }
  | "AccessorForProperty" { ACCESSOR_FOR_PROPERTY }
  | "Any" {ANY}
  | "Fields" { FIELDS }
  | "FieldName" { FIELD_NAME }
  | "Parameters" { PARAMETERS }
  | "ParameterName" { PARAMETER_NAME }
  | "ParameterPos" { PARAMETER_POS }
  | "Body" {BODY}
  | "Protocol" {PROTOCOL}
  | "InitExpr" {INIT_EXPR}
  | "Cond" {COND}
  | "PointerToDecl" {POINTER_TO_DECL}
   | "SourceExpr" {SOURCE_EXPR}
  | id { IDENTIFIER (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 80) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: '" ^ (Lexing.lexeme lexbuf) ^"'")) }
  | eof { EOF }

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }
