(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Lexer to support a parser of types *)
{

open CTypes_parser

let line_number = ref 1
let parentheses = ref 0

let parsed_tokens = Buffer.create 1
let log lexbuf = Buffer.add_char parsed_tokens ' '; Buffer.add_string parsed_tokens (Lexing.lexeme lexbuf)
let reset_log () = Buffer.reset parsed_tokens
let get_log () = Buffer.contents parsed_tokens

let attr_par = ref 0
let attr_buf = Buffer.create 1
let reset_attr_buf () = Buffer.reset attr_buf
let log_attr_buf lexbuf =
  Buffer.add_string parsed_tokens (Lexing.lexeme lexbuf);
  Buffer.add_string attr_buf (Lexing.lexeme lexbuf)
let get_attr_buf () = Buffer.contents attr_buf


}

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']

let underscore = '_'
let minus = '-'
let letter = lowerletter | upperletter

let digit = ['0'-'9']
let number = digit* | digit+ '.' digit* | digit* '.' digit+
let space = [' ' '\t']

let identifier = (letter | underscore) (letter | underscore | digit | ':')*

let anonymous_identifier = '(' 'a' 'n' 'o' 'n' 'y' 'm' 'o' 'u' 's' (letter | underscore | digit | ' ' | '.' | ':' | '/' | minus)* ')'

let nested_anonymous_identifier = (identifier ':' ':')+ (anonymous_identifier | identifier)

let nested_identifier = identifier (':' ':' identifier)+

let char = letter | digit

let hexa_digit = ['0'-'9' 'a'-'f']
let hexa = '0' 'x' hexa_digit*

let dir_sep = '/'
let dot = '.'
let extension = dot ('c' | 'h' | 'm')
let basename = (char | underscore | minus | dot )+
let filename = basename extension
let path = (dir_sep basename | basename)* filename
let attribute_digit = letter | underscore | digit | '(' | ')' | '\"' | ","

let newline =  '\n'

rule token = parse
  | space              { log lexbuf; token lexbuf }
  | newline            { log lexbuf; incr line_number; token lexbuf }
  | '.'                { log lexbuf; DOT }
  | ';'                { log lexbuf; SEMICOLON }
  | ':'                { log lexbuf; COLON }
  | ','                { log lexbuf; COMMA }
  | '\''               { log lexbuf; SINGLE_QUOTE }
	| '`'               { log lexbuf; REV_QUOTE }
  | '\"'               { log lexbuf; DOUBLE_QUOTE }
  | '^'                { log lexbuf; CARRET }
  | "class"            { log lexbuf; CLASS }
  | "struct"           { log lexbuf; STRUCT }
  | "union"            { log lexbuf; UNION }
  | "enum"             { log lexbuf; ENUM }
  | "unsigned"         { log lexbuf; UNSIGNED }
  | "signed"           { log lexbuf; SIGNED }
  | "void"             { log lexbuf; VOID }
  | "char"             { log lexbuf; CHAR }
  | "short"            { log lexbuf; SHORT }
  | "int"              { log lexbuf; INT }
  | "long"             { log lexbuf; LONG }
  | "_Bool"            { log lexbuf; UND_BOOL }
  | "__uint16_"        { log lexbuf; UND_UND_UINT16_ }
  | "__uint16_t"       { log lexbuf; UND_UND_UINT16_T }
  | "__uint32_"        { log lexbuf; UND_UND_UINT32_ }
  | "__uint32_t"       { log lexbuf; UND_UND_UINT32_T }
  | "__int32_t"        { log lexbuf; UND_UND_INT32_T }
  | "__int64_t"        { log lexbuf; UND_UND_INT64_T }
  | "__uint64_t"       { log lexbuf; UND_UND_UINT64_T }
  | "UInt8"            { log lexbuf; UINT8 }
  | "UInt16"           { log lexbuf; UINT16 }
  | "UInt32"           { log lexbuf; UINT32 }
  | "UInt64"           { log lexbuf; UINT64 }
  | "float"            { log lexbuf; FLOAT }
  | "double"           { log lexbuf; DOUBLE }
  | "volatile"         { log lexbuf; VOLATILE }
  | "*volatile"        { log lexbuf; STARVOLATILE }
  | "<builtin fn type>" { log lexbuf; BUILTIN_FN_TYPE}
  | "inline"           { log lexbuf; INLINE }
  | "typename"         { log lexbuf; TYPENAME }
  | "const"            { log lexbuf; CONST }
  | "*const"           { log lexbuf; STARCONST }
  | "__strong"         { log lexbuf; UND_UND_STRONG }
  | "__unsafe_unretained"  { log lexbuf; UND_UND_UNSAFE_UNRETAINED }
  | "__weak"           { log lexbuf; UND_UND_WEAK }
  | "__autoreleasing"  { log lexbuf; UND_UND_AUTORELEASING }
  | "*__strong"         { log lexbuf; STAR_UND_UND_STRONG }
  | "*__unsafe_unretained"  { log lexbuf; STAR_UND_UND_UNSAFE_UNRETAINED }
  | "*__weak"           { log lexbuf; STAR_UND_UND_WEAK }
  | "*__autoreleasing"  { log lexbuf; STAR_UND_UND_AUTORELEASING }
  | "noexcept"         { log lexbuf; NOEXCEPT }
  | "restrict"         { log lexbuf; RESTRICT }
  | '%'                { log lexbuf; PERCENT }
  | '&'                { log lexbuf; AMPERSAND }
  | '!'                { log lexbuf; EXCLAMATION }
  | '='                { log lexbuf; EQUAL }
  | '-'                { log lexbuf; MINUS }
  | '+'                { log lexbuf; PLUS }
  | '<'                { log lexbuf; LEFT_CHEVRON }
  | '>'                { log lexbuf; RIGHT_CHEVRON }
  | '('                { log lexbuf; incr parentheses; LEFT_PARENTHESIS }
  | ')'                { log lexbuf; decr parentheses; RIGHT_PARENTHESIS }
  | '['                { log lexbuf; LEFT_SQBRACKET }
  | ']'                { log lexbuf; RIGHT_SQBRACKET }
  | '{'                { log lexbuf; LEFT_BRACE }
  | '}'                { log lexbuf; RIGHT_BRACE }
  | '*'                { log lexbuf; STAR }
  | '|'                { log lexbuf; PIPE }
  | '/'                { log lexbuf; SLASH }
  | '\\'               { log lexbuf; BACKSLASH }
  | hexa as s          { log lexbuf; HEXA (s) }
  | number as n        { log lexbuf; NUMBER n }
  | identifier as s    { log lexbuf; IDENT (s) }
  | anonymous_identifier as s    { log lexbuf; ANONYM_IDENT (s) }
  | nested_identifier as s    { log lexbuf; NESTED_IDENT (s) }
  | nested_anonymous_identifier as s    { log lexbuf; NESTED_ANONYM_IDENT (s) }
  | eof                { EOF }

