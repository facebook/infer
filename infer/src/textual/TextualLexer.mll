(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{

  (** classic Ocamllex function to update current lexbuf line at each end of
     line *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  open TextualMenhir

}

let whitespace = [' ' '\t']
let whitespaces = whitespace*
let eol = whitespace*("\r")?"\n" (* end of line *)
let eol_comment = "//" [^'\n']*
let id = ['a'-'z' 'A'-'Z' '_' '$'] (['a'-'z' 'A'-'Z' '0'-'9' '_' '$'] | "::")* | "<init>"

let binary_numeral_prefix = "0" ("b"|"B")
let hex_numeral_prefix = "0" ("x"|"X")
let numeral_prefix = ['0'-'9'] | binary_numeral_prefix | hex_numeral_prefix
let numeral_digit = ['0'-'9' 'a'-'f' 'A'-'F' '_']
let integer_literal = numeral_prefix numeral_digit* ['l' 'L']?

let digits = ['0'-'9']+
let float_type_suffix = ['f' 'F' 'd' 'D']
let exponent_part = ['e' 'E'] ['-' '+']? digits
let floating_point_literal =
  (digits "." digits? exponent_part? float_type_suffix?)
| ("." digits exponent_part? float_type_suffix?)
| (digits exponent_part float_type_suffix?)
| (digits exponent_part? float_type_suffix)

rule main = parse
  | whitespace+
        { main lexbuf }
  | eol_comment
        { main lexbuf }
  | eol
        { incr_linenum lexbuf;
          main lexbuf }
  | "&"
        { AMPERSAND }
  | "<-"
        { ASSIGN }
  | ":"
        { COLON } 
  | ","
        { COMMA }
  | "attribute"
        { ATTRIBUTE }
  | "declare"
        { DECLARE }
  | "define"
        { DEFINE }
  | "extends"
        { EXTENDS }
  | "."
        { DOT }
  | "="
        { EQ }
  | "false"
        { FALSE }
  | "float"
        { FLOAT }
  | "global"
        { GLOBAL }
  | ".handlers"
        { HANDLERS }
  | "int"
        { INT }
  | "jmp"
        { JMP }
  | "<"
        { LABRACKET }
  | "{"
        { LBRACKET }
  | "("
        { LPAREN }
  | "load"
        { LOAD }
  | "local"
        { LOCALKEYWORD }
  | "["
        { LSBRACKET }
  | "null"
        { NULL }
  | "!"
        { NOT }
  | "prune"
        { PRUNE }
  | ">"
        { RABRACKET }
  | "}"
        { RBRACKET }
  | "ret"
        { RET }
  | ")"
        { RPAREN }
  | "]"
        { RSBRACKET }
  | ";"
        { SEMICOLON }
  | "*"
        { STAR }
  | "store"
        { STORE }
  | "throw"
        { THROW }
  | "true"
        { TRUE }
  | "type"
        { TYPE }
  | "unreachable"
        { UNREACHABLE }
  | "void"
        { VOID }

  | (floating_point_literal as f)
        { FLOATINGPOINT (float_of_string f) }
  | (integer_literal as i)
        { INTEGER (Z.of_string i) }

  | "n" (integer_literal as i)
        { LOCAL (int_of_string i) }

  | "#" (id as name)
        { LABEL name }

  | (id as name)
        { IDENT name }

  | "\"" ([^ '\"']* as s) "\""
        { STRING s }
  | eof
        { EOF }


{



}
