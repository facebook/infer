(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
    let prog = LParser.prog LLexer.token lexbuf in
    let pretty = LPretty.pretty_prog prog in
    LTrans.gen_prog prog; ()
with
  | UsageError msg -> print_string ("Usage error: " ^ msg ^ "\n")
