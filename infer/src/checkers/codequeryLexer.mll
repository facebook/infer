(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

{

open CodequeryParser

let line_number = ref 1
let parentheses = ref 0

let parsed_tokens = Buffer.create 1
let log lexbuf = Buffer.add_char parsed_tokens ' '; Buffer.add_string parsed_tokens (Lexing.lexeme lexbuf)
let reset_log () = Buffer.reset parsed_tokens
let get_log () = Buffer.contents parsed_tokens

}

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']

let underscore = '_'
let minus = '-'
let letter = lowerletter | upperletter  

let digit = ['0'-'9']
let number = digit* | digit+ '.' digit* | digit* '.' digit+

let identifier = (letter | underscore) (letter | underscore | digit)* 

let const_string_digit = letter | underscore | digit | ' ' | '.' | ',' | '(' | ')' | '[' | ']'
let const_string = '{' const_string_digit* '}'

let single_quote = '\''

let space = [' ' '\t']
let newline =  '\n'

rule token = parse
  | space              { log lexbuf; token lexbuf }
  | newline            { log lexbuf; incr line_number; token lexbuf }
  | '.'                { log lexbuf; DOT }
  | ';'                { log lexbuf; SEMICOLON }
  | ':'                { log lexbuf; COLON }
  | ','                { log lexbuf; COMMA }
  | '`'                { log lexbuf; REV_QUOTE }
  | '\''               { log lexbuf; SINGLE_QUOTE }
  | '\"'               { log lexbuf; DOUBLE_QUOTE }
  | '%'                { log lexbuf; PERCENT }
  | '&'                { log lexbuf; AMPERSAND }
  | '!'                { log lexbuf; EXCLAMATION }
  | '='                { log lexbuf; EQUAL }
  | "=="                { log lexbuf; EQUALEQUAL }
  | "!="                { log lexbuf; EXCLAMATIONEQUAL }
  | '-'                { log lexbuf; MINUS }
  | '+'                { log lexbuf; PLUS }
  | '<'                { log lexbuf; LEFT_CHEVRON }
  | '>'                { log lexbuf; RIGHT_CHEVRON }
  | '('                { log lexbuf; incr parentheses; LEFT_PARENTHESIS }
  | ')'                { log lexbuf; decr parentheses; RIGHT_PARENTHESIS }
  | '['                { log lexbuf; LEFT_SQBRACKET }
  | ']'                { log lexbuf; RIGHT_SQBRACKET }
  | '*'                { log lexbuf; STAR }
  | '|'                { log lexbuf; PIPE }
  | '/'                { log lexbuf; SLASH }
  | '\\'               { log lexbuf; BACKSLASH }
  | "if"               { log lexbuf; IF }
  | "..."              { log lexbuf; DOTDOTDOT }
  | "@source"          { log lexbuf; SOURCE }
  | "@error"           { log lexbuf; ERROR }
  | number as n        { log lexbuf; NUMBER n }
  | identifier as s    { log lexbuf; IDENT (s) }
  | const_string as s  { log lexbuf; CONST_STRING (String.sub s 1 (String.length s - 2)) }
  | eof                { EOF }

