(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ctl_lexer
open Lexing

let parse_al_file fname channel =
  let parse_with_error lexbuf =
    try Some (Ctl_parser.al_file token lexbuf) with
    | CTLExceptions.ALParserInvariantViolationException s ->
        raise CTLExceptions.(ALFileException (create_exc_info s lexbuf))
    | SyntaxError _ | Ctl_parser.Error ->
        raise CTLExceptions.(ALFileException (create_exc_info "SYNTAX ERROR" lexbuf))
  in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= fname} ;
  parse_with_error lexbuf


let validate_al_files () =
  let validate_al_file fname =
    try
      Utils.with_file_in ~f:(parse_al_file fname) fname |> ignore ;
      None
    with CTLExceptions.ALFileException exc_info -> Some (CTLExceptions.json_of_exc_info exc_info)
  in
  match List.filter_map ~f:validate_al_file Config.linters_def_file with
  | [] ->
      Ok ()
  | _ as errors ->
      Error (Yojson.Basic.to_string (`List errors))
