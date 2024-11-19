type position =
  {
  pos_fname: string ;
  pos_lnum: int ;
  pos_bol: int ;
  pos_cnum: int }
val dummy_pos : position
type lexbuf =
  {
  refill_buff: lexbuf -> unit ;
  mutable lex_buffer: bytes ;
  mutable lex_buffer_len: int ;
  mutable lex_abs_pos: int ;
  mutable lex_start_pos: int ;
  mutable lex_curr_pos: int ;
  mutable lex_last_pos: int ;
  mutable lex_last_action: int ;
  mutable lex_eof_reached: bool ;
  mutable lex_mem: int array ;
  mutable lex_start_p: position ;
  mutable lex_curr_p: position }
val from_channel : ?with_positions:bool -> in_channel -> lexbuf
val from_string : ?with_positions:bool -> string -> lexbuf
val from_function : ?with_positions:bool -> (bytes -> int -> int) -> lexbuf
val set_position : lexbuf -> position -> unit
val set_filename : lexbuf -> string -> unit
val with_positions : lexbuf -> bool
val lexeme : lexbuf -> string
val lexeme_char : lexbuf -> int -> char
val lexeme_start : lexbuf -> int
val lexeme_end : lexbuf -> int
val lexeme_start_p : lexbuf -> position
val lexeme_end_p : lexbuf -> position
val new_line : lexbuf -> unit
val flush_input : lexbuf -> unit
val sub_lexeme : lexbuf -> int -> int -> string
val sub_lexeme_opt : lexbuf -> int -> int -> string option
val sub_lexeme_char : lexbuf -> int -> char
val sub_lexeme_char_opt : lexbuf -> int -> char option
type lex_tables =
  {
  lex_base: string ;
  lex_backtrk: string ;
  lex_default: string ;
  lex_trans: string ;
  lex_check: string ;
  lex_base_code: string ;
  lex_backtrk_code: string ;
  lex_default_code: string ;
  lex_trans_code: string ;
  lex_check_code: string ;
  lex_code: string }
val engine : lex_tables -> int -> lexbuf -> int
val new_engine : lex_tables -> int -> lexbuf -> int
