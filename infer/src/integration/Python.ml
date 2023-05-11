(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let capture ~pyc =
  let sourcefile = Textual.SourceFile.create pyc in
  let code = FFI.from_file ~is_binary:true pyc in
  let module_ = PyTrans.to_module ~sourcefile code in
  if Config.dump_textual then
    let filename = SourceFile.create pyc in
    let filename = Filename.chop_extension (SourceFile.to_abs_path filename) ^ ".sil" in
    TextualSil.dump_module ~filename module_
