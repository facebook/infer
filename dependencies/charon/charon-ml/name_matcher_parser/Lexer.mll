{
  open Parser
  open Ast
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*
let whitespace = [' ']+

(* Rules *)
rule token = parse
  | "::" { SEP }
  | "mut" { MUT }
  | "const" { CONST }
  | "fn" { FN }
  | "'static" { STATIC_REGION }
  | ''' { REGION (index lexbuf) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "_" { WILDCARD }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | digit { INT (Z.of_string (Lexing.lexeme lexbuf)) }
  | '(' { LEFT_BRACKET }
  | ')' { RIGHT_BRACKET }
  | '{' { LEFT_CURLY }
  | '}' { RIGHT_CURLY }
  | '[' { LEFT_SQUARE }
  | ']' { RIGHT_SQUARE }
  | "@" { VAR(index lexbuf) }
  | ';' { SEMICOL }
  | '&' { AMPERSAND }
  | whitespace { token lexbuf }
  | eof { EOF }
  | '<' { LEFT_ANGLE }
  | '>' { RIGHT_ANGLE }
  | ',' { COMMA }
  | "->" { ARROW }
  | '*' { STAR }
  | "#" { HASH }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and index = parse
  | '_' { None }
  | ident { Some (VarName (Lexing.lexeme lexbuf)) }
  | digit+ { Some (VarIndex (int_of_string (Lexing.lexeme lexbuf))) }
  | "" { None }
