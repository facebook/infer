val symbol_start : unit -> int
val symbol_end : unit -> int
val rhs_start : int -> int
val rhs_end : int -> int
val symbol_start_pos : unit -> Lexing.position
val symbol_end_pos : unit -> Lexing.position
val rhs_start_pos : int -> Lexing.position
val rhs_end_pos : int -> Lexing.position
val clear_parser : unit -> unit
exception Parse_error 
type parser_env
type parse_tables =
  {
  actions: (parser_env -> Obj.t) array ;
  transl_const: int array ;
  transl_block: int array ;
  lhs: string ;
  len: string ;
  defred: string ;
  dgoto: string ;
  sindex: string ;
  rindex: string ;
  gindex: string ;
  tablesize: int ;
  table: string ;
  check: string ;
  error_function: string -> unit ;
  names_const: string ;
  names_block: string }
exception YYexit of Obj.t 
val yyparse :
  parse_tables -> int -> (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b
val peek_val : parser_env -> int -> 'a
val is_current_lookahead : 'a -> bool
val parse_error : string -> unit
