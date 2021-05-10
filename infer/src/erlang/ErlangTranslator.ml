(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst

let to_source_and_cfg module_ =
  let source =
    let extract_path = function
      | {Ast.line= _; simple_form= File {path}} ->
          Some path
      | _ ->
          None
    in
    match List.find_map ~f:extract_path module_ with
    | None ->
        SourceFile.invalid __FILE__
    | Some path ->
        SourceFile.create path
  in
  let cfg = (* TODO *) Cfg.create () in
  (source, cfg)
