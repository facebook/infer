open Lexing
include Ast

let colnum pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "line " ^ l ^ ", column " ^ c

let parse_pattern (s : string) : pattern =
  let lexbuf = Lexing.from_string s in
  try Parser.full_pattern Lexer.token lexbuf
  with Parser.Error ->
    raise
      (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p ^ ":\n" ^ s))
