(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

let collect_class_location (program : JProgramDesc.t) (file : SourceFile.t) =
  let path = SourceFile.to_abs_path file in
  if String.is_suffix path ~suffix:".java" then (
    let cin = In_channel.create path in
    let filebuf = Lexing.from_channel cin in
    let action_on_class_location ~classname ~col ~line =
      let loc : Location.t = {line; col; file} in
      let cn : JBasics.class_name = JBasics.make_cn classname in
      Logging.debug Capture Verbose "set_java_location %s with location %a@." (JBasics.cn_name cn)
        Location.pp_file_pos loc ;
      JProgramDesc.set_java_location program cn loc
    in
    ( try
        let cl = JSourceParser.main JSourceLexer.class_scan filebuf in
        JSourceAST.iter_on_declarations ~action_on_class_location cl
      with
    | Failure s ->
        Logging.debug Capture Verbose "Error parsing source file %s\n%s"
          (SourceFile.to_abs_path file) s
    | JSourceParser.Error ->
        Logging.debug Capture Verbose "JSourceParser error on file %s\n"
          (SourceFile.to_abs_path file) ) ;
    In_channel.close cin )


let debug_on_file path =
  if String.is_suffix path ~suffix:".java" then (
    let cin = In_channel.create path in
    let filebuf = Lexing.from_channel cin in
    let action_on_class_location ~classname ~col ~line =
      Printf.printf "class %s at line %d, column %d\n" classname line col
    in
    ( try
        let cl = JSourceParser.main JSourceLexer.class_scan filebuf in
        JSourceAST.iter_on_declarations ~action_on_class_location cl
      with JSourceParser.Error ->
        let pos = filebuf.Lexing.lex_curr_p in
        let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
        Printf.eprintf "Java source syntax error at line %d, column %d.\n%!" line col ) ;
    In_channel.close cin )
