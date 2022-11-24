(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{

  open! IStd

  (** classic Ocamllex function to update current lexbuf line at each end of
     line *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  exception LexingError of Textual.Location.t * string

  let lex_error (lexbuf : Lexing.lexbuf) =
    let pos = Lexing.lexeme_start_p lexbuf in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let token = Lexing.lexeme lexbuf in
    raise (LexingError (Textual.Location.known ~line ~col, token))

  open TextualMenhir
}

let whitespace = [' ' '\t']
let whitespaces = whitespace*
let eol = whitespace*("\r")?"\n" (* end of line *)
let eol_comment = "//" [^'\n']*
let id = ['a'-'z' 'A'-'Z' '_' '$'] (['a'-'z' 'A'-'Z' '0'-'9' '_' '$'] | "::")*

let binary_numeral_prefix = "0" ("b"|"B")
let hex_numeral_prefix = "0" ("x"|"X")
let numeral_prefix = ['0'-'9'] | binary_numeral_prefix | hex_numeral_prefix
let numeral_digit = ['0'-'9' 'a'-'f' 'A'-'F' '_']
let integer_literal = '-'? numeral_prefix numeral_digit* ['l' 'L']?

let digits = ['0'-'9']+
let exponent_part = ['e' 'E'] ['-' '+']? digits
let positive_floating_point_literal =
  (digits "." digits? exponent_part?)
| (digits exponent_part)
let floating_point_literal = ['-' '+']? positive_floating_point_literal

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
  | "declare"
        { DECLARE }
  | "define"
        { DEFINE }
  | "extends"
        { EXTENDS }
  | "."
        { DOT }
  | "..."
        { ELLIPSIS }
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
        { match float_of_string_opt f with
          | Some f -> FLOATINGPOINT f
          | None -> lex_error lexbuf }

  | (integer_literal as i)
        { match Z.of_string i with
          | i -> INTEGER i
          | exception Invalid_argument _ -> lex_error lexbuf }

  | "n" (integer_literal as i)
        { match int_of_string_opt i with
          | Some i -> LOCAL i
          | None -> lex_error lexbuf }

  | "#" (id as name)
        { LABEL name }

  | (id as name)
        { IDENT name }

  | "\"" ([^ '\"']* as s) "\""
        { STRING s }
  | eof
        { EOF }
  | _
        { lex_error lexbuf }
