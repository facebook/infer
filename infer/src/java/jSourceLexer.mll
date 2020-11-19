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

  open JSourceParser

}

let whitespace = [' ' '\t']
let eol = whitespace*("\r")?"\n" (* end of line *)
let eol_comment = "//" [^'\n']*
let id = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*
let char = "'\\''" | "'\"'" | "'" [ ^'\'' ]+ "'"
let modifier = "public"|"protected"|"private"|"abstract"|"static"|
               "final"|"strictfp"|"transient"|"volatile"
let numeric_type = "byte"|"short"|"int"|"long"|"char"|"float"|"double"
let primitive_type = "boolean"|numeric_type
let assignment_operator = "*="|"/="|"%="|"+="|"-="|"<<="|">>="|">>>="|"&="|"^="|"|="
let binop = "||"|"&&"|"&"|"^"|"=="|"!="|"<="|">="|"<<"|">>"|">>>"|"+"|"-"|"*"|"/"|"%"

let binary_numeral_prefix = "0" ("b"|"B")
let hex_numeral_prefix = "0" ("x"|"X")
let numeral_prefix = ['0'-'9'] | binary_numeral_prefix | hex_numeral_prefix
let numeral_digit = ['0'-'9' 'a'-'f' 'A'-'F' '_']
let integer_literal = numeral_prefix numeral_digit* ['l' 'L']?
let hexadecimal_floating_point_literal = hex_numeral_prefix (numeral_digit | ".")+ ("p"|"P")
let decimal_floating_point_literal = hex_numeral_prefix (numeral_digit | ".")+ ("p"|"P")
let digits = ['0'-'9']+
let float_type_suffix = ['f' 'F' 'd' 'D']
let exponent_part = ['e' 'E'] ['-' '+']? digits
let floating_point_literal =
  (digits "." digits? exponent_part? float_type_suffix?)
| ("." digits exponent_part? float_type_suffix?)
| (digits exponent_part float_type_suffix?)
| (digits exponent_part? float_type_suffix)


(* We follow an abstraction of the official grammar described here:
    https://docs.oracle.com/javase/specs/jls/se14/html/jls-19.html *)
rule class_scan = parse
  | whitespace+
        { class_scan lexbuf }
  | eol_comment
        { class_scan lexbuf }
  | "/*"
        { skip_comments (class_scan) lexbuf }
  | eol
        { incr_linenum lexbuf;
          class_scan lexbuf }
  | "package"
        { PACKAGE }
  | "import"
        { IMPORT }
  | "class"
        { CLASS }
  | "instanceof"
        { INSTANCEOF }
  | "interface"
        { INTERFACE }
  | "void"
        { VOID }
  | "throws"
        { THROWS }
  | "enum"
        { ENUM }
  | modifier
          { class_scan lexbuf }
  | primitive_type
        { PRIMTYPE }
  | "<"
        { LANGLE }
  | ">"
        { RANGLE }
  | "new"
        { NEW }
  | "var"
        { VAR }
  | "extends"
        { EXTENDS }
  | "implements"
        { IMPLEMENTS }
  | "assert"
        { ASSERT }
  | "do"
        { DO }
  | "while"
        { WHILE }
  | "if"
        { IF }
  | "else"
        { ELSE }
  | "try"
        { TRY }
  | "catch"
        { CATCH }
  | "finally"
        { FINALLY }
  | "for"
        { FOR }
  | "break"
        { BREAK }
  | "continue"
        { CONTINUE }
  | "return"
        { RETURN }
  | "throw"
        { THROW }
  | "synchronized"
        { SYNCHRONIZED }
  | "yield"
        { YIELD }
  | "null"
        { NULL }
  | "true"
        { TRUE }
  | "false"
        { FALSE }

  | (floating_point_literal as f)
        { FLOATINGPOINT f }
  | (integer_literal as i)
        { INTEGER i }


  | (id as name)
        { IDENT name }

  | "\"" ([^ '\"']* as s) "\""
        { STRING s }
  | (char as s)
        { CHAR s }
  | ";"
        { SEMICOLON }
  | ":"
        { COLON }
  | "."
        { DOT }
  | "{"
        { LBRACKET }
  | "}"
        { RBRACKET }
  | "["
        { LSBRACKET }
  | "]"
        { RSBRACKET }
  | "("
        { LPAREN }
  | ")"
        { RPAREN }
  | ","
        { COMMA }
  | "?"
        { QMARK }
  | ("++"|"--")
        { INCR_DECR }
  | "|"
        { PIPE }
  | "="
        { EQ }
  | "!"
        { BANG }
  | "~"
        { TILDE }
  | "..."
        { THREEDOTS }
  | assignment_operator
        { ASSIGNOP}
  | binop
        { BINOP }

  | "@" whitespace* id ("." id)* "("
        { skip_well_parenthesized_parentheses 1
            (class_scan) lexbuf }
  | "@" whitespace* id ("." id)*
        {  class_scan lexbuf }

  | _
        { class_scan lexbuf }
  | eof
        { EOF }

(* we skip type annotation arguments (...) because they may contain brackets *)
and skip_well_parenthesized_parentheses width action = parse
  | eol
        { incr_linenum lexbuf;
           skip_well_parenthesized_parentheses width action lexbuf }
  | "("
        { skip_well_parenthesized_parentheses (width+1) action lexbuf }
  | ")"
        { if width<=1 then action lexbuf
          else skip_well_parenthesized_parentheses (width-1) action lexbuf }
  | eol_comment
        { skip_well_parenthesized_parentheses width action lexbuf }
  | "/*"
        { skip_comments
             (skip_well_parenthesized_parentheses width action) lexbuf }
  | "\""
        { skip_string (skip_well_parenthesized_parentheses width action) lexbuf }
  | char
        { skip_well_parenthesized_parentheses width action lexbuf }
  | _
        { skip_well_parenthesized_parentheses width action lexbuf }

and skip_comments action = parse
  | "*/"
        { action lexbuf }
  | eol
        { incr_linenum lexbuf;
          skip_comments action lexbuf }
  | _
        { skip_comments action lexbuf }

and skip_string action = parse
  | "\\\\"
        { skip_string action lexbuf }
  | "\\\""
        { skip_string action lexbuf }
  | "\""
        { action lexbuf }
  | _
        { skip_string action lexbuf }


{



}
