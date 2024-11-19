type token =
  | Kwd of string 
  | Ident of string 
  | Int of int 
  | Float of float 
  | String of string 
  | Char of char 
val make_lexer : string list -> char Stream.t -> token Stream.t
