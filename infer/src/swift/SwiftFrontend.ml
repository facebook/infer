(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let dump_textual_file source_file module_ =
  let source_file =
    if Config.frontend_tests then
      let suffix = ".test.sil" in
      String.append source_file suffix
    else
      let suffix = ".sil" in
      String.append source_file suffix
  in
  TextualSil.dump_module ~filename:source_file module_


let to_module source_file llair_program =
  let sourcefile = Textual.SourceFile.create source_file in
  let module_ = Llair2Textual.translate sourcefile llair_program in
  if Config.dump_textual then dump_textual_file source_file module_ ;
  module_


let capture source_file _llvm_bitcode =
  let dummy_program = Llair.Program.mk ~globals:[] ~functions:[] in
  ignore (to_module source_file dummy_program)


let capture_llair source_file llair_program = ignore (to_module source_file llair_program)
