(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{
  open! IStd

  open Lexing
  open Types_parser

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
  | "char" { CHAR }
  | "char16_t" { CHAR16_T }
  | "char32_t" { CHAR32_T }
  | "wchar_t" { WCHAR_T }
  | "bool" { BOOL }
  | "short" { SHORT }
  | "int" { INT }
  | "long" { LONG }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "void" { VOID }
  | "signed" { SIGNED }
  | "unsigned" { UNSIGNED }
  | "__int128" { INT128 }
  | "__float128" { FLOAT128 }
  | "__wchar_t" { UNDUNDWCHAR_T }
  | "half" { HALF }
  | "__fp16" { UNDUNDFP16 }
  | "nullptr_t" { NULLPTR }
  | "id" { OBJCID }
  | "Class" { OBJCCLASS }
  | "SEL" { OBJCSEL }
  | "*" { STAR }
  | "&" { AMPERSAND }
  | "REGEXP" { REGEXP }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "<" { LEFT_ANGLE }
  | ">" { RIGHT_ANGLE }
  | id { IDENTIFIER (Lexing.lexeme lexbuf) }
  | ''' { read_string (Buffer.create 80) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: '" ^ (Lexing.lexeme lexbuf) ^"'")) }
  | eof { EOF }

  and read_string buf = parse
  | ''' { REARG (Buffer.contents buf) }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '\'' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
