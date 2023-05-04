(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* Usually compiled files end up like [foo.pyc] or [foo.cpython-38.pyc]. Let's strip these suffix *)
let module_name pyc =
  let pyc = Filename.chop_suffix_opt ~suffix:"pyc" pyc |> Option.value ~default:pyc in
  Filename.chop_suffix_opt ~suffix:"cpython-38" pyc |> Option.value ~default:pyc


let capture ~pyc =
  let sourcefile = Textual.SourceFile.create pyc in
  let modulename = module_name @@ Filename.basename pyc in
  let modulename = Printf.sprintf "%s::$toplevel$" modulename in
  let code = FFI.from_file ~is_binary:true pyc in
  let module_ = PyTrans.to_module ~sourcefile modulename code in
  if Config.dump_textual then
    let filename = SourceFile.create pyc in
    let filename = Filename.chop_extension (SourceFile.to_abs_path filename) ^ ".sil" in
    TextualSil.dump_module ~filename module_
