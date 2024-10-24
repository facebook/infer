(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module SedlexingEncoding = Sedlexing.Latin1

exception LexingError of Textual.Location.t * string

type lexbuf = Sedlexing.lexbuf

module Lexbuf = struct
  include Sedlexing
  include SedlexingEncoding
end

let lex_error (lexbuf : Sedlexing.lexbuf) =
  let pos, _ = Sedlexing.lexing_positions lexbuf in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  let token = SedlexingEncoding.lexeme lexbuf in
  raise (LexingError (Textual.Location.known ~line ~col, token))


let textual_keywords =
  TextualMenhir.
    [ ("declare", DECLARE)
    ; ("define", DEFINE)
    ; ("else", ELSE)
    ; ("extends", EXTENDS)
    ; ("equals", EQUALS)
    ; ("false", FALSE)
    ; ("float", FLOAT)
    ; ("global", GLOBAL)
    ; ("if", IF)
    ; ("int", INT)
    ; ("jmp", JMP)
    ; ("load", LOAD)
    ; ("local", LOCALKEYWORD)
    ; ("null", NULL)
    ; ("prune", PRUNE)
    ; ("ret", RET)
    ; ("store", STORE)
    ; ("then", THEN)
    ; ("throw", THROW)
    ; ("true", TRUE)
    ; ("type", TYPE)
    ; ("unreachable", UNREACHABLE)
    ; ("void", VOID) ]


let keywords_of_list l = Map.of_alist_exn (module String) l

let digit = [%sedlex.regexp? '0' .. '9']

let digits = [%sedlex.regexp? Plus digit]

let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let lower = [%sedlex.regexp? 'a' .. 'z']

let upper = [%sedlex.regexp? 'A' .. 'Z']

let letter = [%sedlex.regexp? lower | upper]

let ident =
  [%sedlex.regexp? (letter | Chars "_$"), Star (letter | digit | Chars "_$" | "::" | ":::")]


let binary_numeral_prefix = [%sedlex.regexp? "0", Chars "bB"]

let hex_numeral_prefix = [%sedlex.regexp? "0", Chars "xX"]

let numeral_prefix = [%sedlex.regexp? binary_numeral_prefix | hex_numeral_prefix]

let numeral_digit = [%sedlex.regexp? hexdigit | '_']

let sign = [%sedlex.regexp? Chars "-+"]

let integer_literal =
  [%sedlex.regexp?
    Opt '-', (numeral_prefix, Plus numeral_digit | digit, Star numeral_digit), Opt (Chars "lL")]


let exponent_part = [%sedlex.regexp? Chars "eE", Opt sign, digits]

let floating_point_literal =
  [%sedlex.regexp? Opt sign, (digits, ".", Opt digits, Opt exponent_part | digits, exponent_part)]


let blanks = [%sedlex.regexp? Star white_space]

let build_mainlex keywords =
  let rec mainlex (lexbuf : Sedlexing.lexbuf) =
    let open TextualMenhir in
    match%sedlex lexbuf with
    | Plus white_space ->
        mainlex lexbuf
    | "//", Star (Compl (Chars "\r\n")) ->
        mainlex lexbuf
    | "/*" ->
        comment lexbuf
    | "&" ->
        AMPERSAND
    | "&&" ->
        AND
    | "->" ->
        ARROW
    | "<-" ->
        ASSIGN
    | ":" ->
        COLON
    | "," ->
        COMMA
    | "." ->
        DOT
    | "..." ->
        ELLIPSIS
    | "=" ->
        EQ
    | "<" ->
        LABRACKET
    | "{" ->
        LBRACKET
    | "(" ->
        LPAREN
    | "[" ->
        LSBRACKET
    | "!" ->
        NOT
    | "||" ->
        OR
    | ">" ->
        RABRACKET
    | "}" ->
        RBRACKET
    | ")" ->
        RPAREN
    | "]" ->
        RSBRACKET
    | ";" ->
        SEMICOLON
    | "*" ->
        STAR
    | "?" ->
        QUESTION
    | ".handlers " ->
        HANDLERS
    | floating_point_literal -> (
        let f = Lexbuf.lexeme lexbuf in
        match float_of_string_opt f with Some f -> FLOATINGPOINT f | None -> lex_error lexbuf )
    | integer_literal -> (
        let i = Lexbuf.lexeme lexbuf in
        match Z.of_string i with i -> INTEGER i | exception Invalid_argument _ -> lex_error lexbuf )
    | "n", integer_literal -> (
        let lexeme = Lexbuf.lexeme lexbuf in
        let i = String.subo ~pos:1 lexeme in
        match int_of_string_opt i with Some i -> LOCAL i | None -> lex_error lexbuf )
    | "#", ident ->
        let lexeme = Lexbuf.lexeme lexbuf in
        LABEL (String.subo ~pos:1 lexeme)
    | "fun", blanks, "(" ->
        FUN
    | ("?" | ident), blanks, ".", blanks, ident, blanks, "(" -> (
        let lexeme = Lexbuf.lexeme lexbuf in
        match String.split_on_chars lexeme ~on:['.'; '('] with
        | [prefix; id; _] ->
            PROC_AND_LPAREN (Some (String.strip prefix), String.strip id)
        | _ ->
            L.die InternalError "unexpected lexing error" )
    | ident, blanks, "(" -> (
        let lexeme = Lexbuf.lexeme lexbuf in
        match String.split_on_chars lexeme ~on:['('] with
        | [id; _] when String.equal (String.strip id) "if" ->
            IF_AND_LPAREN
        | [id; _] ->
            PROC_AND_LPAREN (None, String.strip id)
        | _ ->
            L.die InternalError "unexpected lexing error" )
    | ident ->
        let lexeme = Lexbuf.lexeme lexbuf in
        Option.value ~default:(IDENT lexeme) (Map.find keywords lexeme)
    | '"', Star (Compl ('"' | '\\') | '\\', any), '"' ->
        (* a string literal may contain an escaped double quote *)
        let lexeme = Lexbuf.lexeme lexbuf in
        STRING (String.sub ~pos:1 ~len:(String.length lexeme - 2) lexeme)
    | eof ->
        EOF
    | any ->
        lex_error lexbuf
    | _ ->
        assert false
  and comment (lexbuf : Sedlexing.lexbuf) =
    match%sedlex lexbuf with
    | "*/" ->
        mainlex lexbuf
    | any ->
        comment lexbuf
    | eof ->
        lex_error lexbuf (* FIXME give more informative error message *)
    | _ ->
        assert false
  in
  mainlex


let textual_mainlex = build_mainlex @@ keywords_of_list textual_keywords
