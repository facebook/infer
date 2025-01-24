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
      let source_file = String.chop_suffix_exn ~suffix:".swift" source_file in
      let suffix = ".sil" in
      String.append source_file suffix
  in
  TextualSil.dump_module ~filename:source_file module_


let to_module source_file _llvm_bitcode =
  let sourcefile = Textual.SourceFile.create source_file in
  let module_ = Textual.Module.{attrs= []; decls= []; sourcefile} in
  if Config.dump_textual then dump_textual_file source_file module_ ;
  module_


let capture source_file llvm_bitcode = ignore (to_module source_file llvm_bitcode)
