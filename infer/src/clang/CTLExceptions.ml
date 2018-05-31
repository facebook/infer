(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception ALParserInvariantViolationException of string

type exc_info = {description: string; filename: string; line: int}

exception ALFileException of exc_info

let hum_string_of_exc_info exc_info =
  Format.sprintf "%s at %s:%d" exc_info.description exc_info.filename exc_info.line


let create_exc_info description lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  {description; filename= pos.pos_fname; line= pos.pos_lnum}


let json_of_exc_info exc_info =
  `Assoc
    [ ("description", `String exc_info.description)
    ; ("filename", `String exc_info.filename)
    ; ("line", `Int exc_info.line) ]


let () =
  Caml.Printexc.register_printer (fun exc ->
      match exc with
      | ALFileException exc_info ->
          Some (Format.sprintf "ALFileException: %s" (hum_string_of_exc_info exc_info))
      | _ ->
          None )
