(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{

exception SyntaxError of string

(* classic Ocamllex function to update the current lexbuf line at each end of line *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

   open DoliCombined
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let letterOrNumber = letter | digit
let eol = whitespace*("\r")?"\n" (* end of line *)
let single_line_comment = "//" [^'\n']*
let start_comment = "(*"
let end_comment = "*)"
let id = letter letterOrNumber*
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | whitespace { read lexbuf }
  | eol { incr_linenum lexbuf;
          read lexbuf }
  | single_line_comment
        { read lexbuf }
  | start_comment
        { read_multi_line_comment lexbuf }
  (* Doli-specific keywords *)
  | "under" { UNDER }
  | "in" { IN }
  | "match" { MATCH }
  | "body" { BODYKW }
  | "Java" { Java }
  | "ObjectiveC" { ObjC }
  (* abbreviations -- will disappear eventually *)
  | "bodyStub" {   BODYSTUB }
  | "objCSignStub" {   ObjCSIGNSTUB }
  (* various brackets and parentheses *)
  | "(" { LPAREN }
  | ")"  { RPAREN }
  | "{" {   LBRACE }
  | "}" {   RBRACE }
  | "<" {  LANGLE }
  | ">" {  RANGLE }
  | "[" {   LSQUAREBRACKET }
  | "]" {   RSQUAREBRACKET }
  (* modifiers and throws *)
  | "public" {  PUBLIC }
  | "protected"  {  PROTECTED }
  | "private" {  PRIVATE }
  | "static" {  STATIC }
  | "abstract" { ABSTRACT }
  | "final" { FINAL }
  | "native" { NATIVE }
  | "throws" { THROWS }
  (* basic types *)
  | "byte" { BYTE }
  | "short"  {  SHORT }
  | "char" { CHAR }
  | "int" { INT }
  | "long" { LONG }
  | "float" { FLOAT }
  | "double"  { DOUBLE }
  | "boolean" { BOOLEAN }
  | "void" { VOID }
  (* separators *)
  | "." { DOT }
  | "," { COMMA }
   | ";" { SEMI }
  (* generics *)
  | "extends" { EXTENDS }
  | "super" { SUPER }
  | "?" { QUESTION }
  (* identifier *)
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
 and read_multi_line_comment = parse
  | end_comment { read lexbuf }
  | newline { incr_linenum lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }
