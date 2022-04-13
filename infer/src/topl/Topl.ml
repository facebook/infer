(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let parse topl_file =
  let f ch =
    let lexbuf = Lexing.from_channel ch in
    try ToplParser.properties (ToplLexer.token ()) lexbuf
    with ToplParser.Error ->
      let Lexing.{pos_lnum; pos_bol; pos_cnum; _} = Lexing.lexeme_start_p lexbuf in
      let col = pos_cnum - pos_bol + 1 in
      L.die UserError "@[%s:%d:%d: topl parse error@]@\n@?" topl_file pos_lnum col
  in
  try In_channel.with_file topl_file ~f
  with Sys_error msg -> L.die UserError "@[topl:%s: %s@]@\n@?" topl_file msg


let properties = lazy (List.concat_map ~f:parse Config.topl_properties)

let automaton = lazy (ToplAutomaton.make (Lazy.force properties))

let automaton () = Lazy.force automaton

let is_active () = Config.is_checker_enabled Topl && not (List.is_empty (Lazy.force properties))
