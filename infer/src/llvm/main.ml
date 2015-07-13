(*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 *)
open Lexing
open Printf

exception UsageError of string

let () = try
  if Array.length Sys.argv < 2 then
    raise (UsageError ("Missing source file as first command line argument."))
  else
    let filename = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in filename) in
    let prog = Parser.prog Lexer.token lexbuf in
    let pretty = Pretty.pretty_prog prog in
    print_string pretty
with
  | UsageError msg -> print_string ("Usage error: " ^ msg ^ "\n")
